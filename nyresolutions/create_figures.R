# 0) Importing libraries

library(dplyr)
library(stringr)
library(purrr)
library(plotly)
library(lubridate)

# 1) Loading data ---------

nyresolution_data <- readr::read_tsv(file.path('nyresolutions','tweetdata.tsv'), 
                                     col_types = "iiiicclcTciciiiic") %>%
    filter(lang == 'en') # english tweets only because they made up 95% of all tweets

tz_import <- readr::read_tsv(file.path('nyresolutions','timezonenames.tsv'), 
                             col_types = 'ccc', skip = 1) %>%
    .[c(25:14, 1:13),] # rearranging rows to order from west to east
emoji_data <- readr::read_tsv(file.path("emoji13-1","emojidata.tsv"), 
                              col_types = 'ccccccc')

# 2) defining functions ---------
# importing plot_emoji_chart(), plot_hashtag_chart(), plot_sankey()
source(file.path('util','plotting_functions.R'))

# importing return_pairs()
source(file.path('util','emoji_functions.R'))

# 3) prepping and plotting figures ----------

# 3.1) tweets per hour (prep & plot) ----
# 31st Dec - 1st Jan
# prepping
nyr_tweets_per_hour <- nyresolution_data %>% 
    mutate(hr = round_date(created_at, unit = '1 hour')) %>%
    group_by(hr) %>%
    summarise(cnt = n()) %>% 
    filter(hr >= ymd_hms('2020-12-31 11:30:00'), 
           hr < ymd_hms('2021-01-01 11:30:00'), ) %>%
    arrange(desc(hr)) %>% # the later times correspond to most westerly regions # west to east
    mutate(hr = as.character(hr), x = seq_along(hr),
           breaks = seq(ymd_hms('2021-01-01 11:00:00'), ymd_hms('2020-12-31 12:00:00'),
                        -3600), # west to east
           labels = tz_import$Name[2:nrow(tz_import)],
           UTC_Offset = tz_import$UTC_Offset[2:nrow(tz_import)])# west to east 

ax <- list(
    zeroline = FALSE,
    showline = TRUE,
    #mirror = "ticks",
    showgrid = FALSE,
    linecolor = toRGB('grey30'),
    linewidth = 4
)
xticktext <- nyr_tweets_per_hour$UTC_Offset
annotationtext <- paste0('<b>',nyr_tweets_per_hour$labels, '</b>')

# plotting
fig_nyr_tweets_per_hour <- nyr_tweets_per_hour %>% 
    plot_ly() %>%
    add_trace(x = ~x, y = ~cnt, type = 'scatter', mode = 'markers+lines', 
              customdata = ~labels,
              text = ~UTC_Offset,
              hovertemplate = '<b>%{customdata}</b> | %{text}<br>%{y} Tweets<extra></extra>',
              line = list(width = 4, color = '2b8cbe'),
              marker = list(size = 8, color = 'a6bddb', 
                            line = list(color = '2b8cbe', width = 3))
              ) %>%
    add_annotations(x = ~x[c(7, 12, 19)],
                    y = ~cnt[c(7, 12, 19)] + 150,
                    text = annotationtext[c(7, 12, 19)],
                    xref = "x",
                    yref = "y", 
                    showarrow = FALSE,
                    font = list(color = 'grey30')
    ) %>%
    layout(xaxis = ax, yaxis = ax) %>%
    layout(xaxis = list(title = list(text = '<b>Time Region</b>', 
                                     standoff = 10, 
                                     font = list(color = 'grey30')
                                     ), 
                        tickvals = ~x[seq(2,24,2)],
                        ticktext = ~UTC_Offset[seq(2,24,2)],
                        tickfont = list(color = 'grey30'),
                        range = c(0,26),
                        showgrid = TRUE
        ),
           yaxis = list(title = list(text = '<b>Tweets per Hour</b>', 
                                     standoff = 10,
                                     font = list(color = 'grey30')
                                    ),
                        tickfont = list(color = 'grey30')
                                       )
        ) %>%
    layout(shapes = list(type = 'line', x0 = 24, x1 = 24, y0 = 0, y1 = 4500,
                         opacity = 0.8, 
                         line = list(dash = 'dash')
                         ))

# 3.2) top emojis  (prep only)----
# prepping data
nyr_emoji_freq_table <- nyresolution_data$text_with_emojis_replaced %>%
    str_extract_all('jaugur_[\\w-]+') %>% # this also removes the ":_medium_skin_tone" part of the desc but keeps the "-eyes" in "heart-eyes"
    unlist() %>% str_remove_all('jaugur_') %>% str_replace_all('_',' ') %>%
    as_tibble() %>% group_by(value) %>%
    summarise(tot = n()) %>%
    arrange(desc(tot)) %>% 
    # adding the emoji itself
    left_join(select(emoji_data, emoji, description, url), by = c('value' = 'description'))  %>%
    distinct(value, .keep_all = T) %>% # removing duplicates due to left join where two emojis have the same desc
    mutate(value = str_replace_all(value, '^flag$', 'country flag'))

# 3.2) top hashtags  (prep only)----
# prepping data
nyr_hashtag_freq_table <- nyresolution_data$text %>% 
    str_extract_all('#\\w+') %>%
    unlist() %>% str_to_lower() %>% 
    as_tibble() %>% group_by(value) %>%
    summarise(tot = n()) %>% 
    arrange(desc(tot))

# 3.3) Pairs of emojis (prep & plotting) -----
# prep
# one tag followed by one or more tags which may or may not be preceded by a space
emoji_chain_pattern <- '(jaugur_[\\w-]+)(\\s*(jaugur_[\\w-]+))+'

nyr_emoji_pairs <- nyresolution_data %>% select(ID, text_with_emojis_replaced) %>%
    # emoji_chain a character vector per row
    mutate(emoji_chain = str_extract_all(text_with_emojis_replaced, emoji_chain_pattern)) %>% 
    # unnest to one string per row with ID and text_with_emojis_replaced values duplicated
    tidyr::unnest(cols = emoji_chain) %>% 
    # pre_split is a character vector per row
    mutate(pre_split = str_split(emoji_chain, ' ')) %>%
    # pair_ is a character vector per row
    mutate(pair_ = map(pre_split, return_pairs, presplit = TRUE)) %>% 
    select(ID, pair_) %>%
    # unnest to one pair-string per row with ID duplicated
    tidyr::unnest(cols = pair_) %>% 
    mutate(
             emoji1 = str_extract(pair_,'^jaugur_[\\w-]+'),
             emoji2 = str_extract(pair_,'jaugur_[\\w-]+$'),
             emojis_sorted = map2_chr(emoji1, emoji2, function(.x, .y) {
                     paste(str_sort(c(.x, .y)), collapse = ' ')
                 })
             )

# 3.3.1 filtering out twins, grouping by ordered order
# based on sorted, the two laughing combos together are in 2nd place
nyr_sorted_emoji_pairs <- nyr_emoji_pairs %>% filter(emoji1 != emoji2) %>% 
    group_by(emojis_sorted) %>%
    summarise(tot = n()) %>%
    arrange(desc(tot)) %>%
    rename(pair_ = emojis_sorted) # columns = pair_ and tot

# 3.3.2 filtering out twins, grouping by unsorted order
# based on order, the two laughing ones are in place 3 and 4
# nyr_unsorted_emoji_pairs <- nyr_emoji_pairs %>% filter(emoji1 != emoji2) %>% 
#     group_by(pair_) %>%
#     summarise(tot = n()) %>%
#     arrange(desc(tot)) # columns = pair_ and tot

# 3.3.3 filtering on twins, (order is irrelevant)
# see which emoji was most commonly used with itself
# nyr_twin_emoji_pairs <- nyr_emoji_pairs %>% filter(emoji1 == emoji2) %>%
#     group_by(pair_) %>%
#     summarise(tot = n()) %>%
#     arrange(desc(tot)) # columns = pair_ and tot

# 3.3.4 no filtering out of twins, grouping by sorted order
nyr_sorted_emoji_pairs_all <- nyr_emoji_pairs %>% 
    group_by(emojis_sorted) %>%
    summarise(tot = n()) %>%
    arrange(desc(tot)) %>%
    rename(pair_ = emojis_sorted) # columns = pair_ and tot

# 3.3.5 no filtering out of twins, grouping by unsorted order
# nyr_unsorted_emoji_pairs_all <- nyr_emoji_pairs %>% 
#     group_by(pair_) %>%
#     summarise(tot = n()) %>%
#     arrange(desc(tot)) # columns = pair_ and tot

# plotting
# 3.3.1
nyr_sorted_emoji_pairs_sankey <- nyr_sorted_emoji_pairs %>% plot_sankey(to_ = 20)
# 3.3.2
# nyr_unsorted_emoji_pairs_sankey <- nyr_unsorted_emoji_pairs %>% plot_sankey(to_ = 20)  %>%
#     layout(annotations = list(text = 'First Emoji',
#                               font = list(size = 20, family = 'Catamaran, sans-serif'),
#                               showarrow = FALSE,
#                               x = 0, xanchor = "left",
#                               y = 1, anchor = 'top', yshift = 30)
#     ) %>%
#     layout(annotations = list(text = 'Second Emoji',
#                               font = list(size = 20, family = 'Catamaran, sans-serif'),
#                               showarrow = FALSE,
#                               x = 1, xanchor = "right",
#                               y = 1, anchor = 'top', yshift = 30)
#     )
# 3.3.3
# nyr_twin_emoji_pairs_sankey <- nyr_twin_emoji_pairs %>% plot_sankey(to_ = 20)
# 3.3.4
nyr_sorted_emoji_pairs_all_sankey <- nyr_sorted_emoji_pairs_all %>% plot_sankey(to_ = 20)
# 3.3.5
# nyr_unsorted_emoji_pairs_all_sankey <- nyr_unsorted_emoji_pairs_all %>% plot_sankey(to_ = 20) %>%
#     layout(annotations = list(text = 'First Emoji',
#                               font = list(size = 20, family = 'Catamaran, sans-serif'),
#                               showarrow = FALSE,
#                               x = 0, xanchor = "left",
#                               y = 1, anchor = 'top', yshift = 30)
#     ) %>%
#     layout(annotations = list(text = 'Second Emoji',
#                               font = list(size = 20, family = 'Catamaran, sans-serif'),
#                               showarrow = FALSE,
#                               x = 1, xanchor = "right",
#                               y = 1, anchor = 'top', yshift = 30)
#     )

# note: show common non-twins, then combined twins+non-twins, then for interest combined twins
