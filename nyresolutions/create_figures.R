# 0) Importing libraries

#library(jsonlite)
library(dplyr)
library(stringr)
library(purrr)
library(tictoc)
library(ggplot2)
library(tidyjson)
library(plotly)
library(lubridate)
library(textclean)
library(rvest)
library(ggtext)
library(tidytext)

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
# https://www.hvitfeldt.me/blog/real-emojis-in-ggplot2/#:~:text=We%20don't%20have%20a,png%20of%20that%20emoji.
emoji_to_link <- function(x) {
    paste0("https://emojipedia.org/emoji/",x) %>%
        read_html() %>%
        html_nodes("tr td a") %>%
        .[1] %>%
        html_attr("href") %>%
        paste0("https://emojipedia.org/", .) %>%
        read_html() %>%
        html_node('div[class="vendor-image"] img') %>%
        html_attr("src")
}

link_to_img <- function(x, size = 25) {
    paste0("<img src='", x, "' width='", size, "'/>")
}

# 3) creating figures ----------

# 3.1) tweets per hour (31st Dec - 1st Jan) ----
# prepping data
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

# prepping for figure
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

# creating figure
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

# 3.2) top emojis  ----
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


plot_emoji_chart <- function(df) {
    ax <- list(
        zeroline = FALSE,
        showline = TRUE,
        mirror = FALSE, "ticks",
        showgrid = FALSE,
        linecolor = toRGB('grey30'),
        linewidth = 4
    )
    df %>% mutate(emoji = str_replace_na(emoji, '')) %>%
        plot_ly() %>%
        add_trace(x = ~rank, y = ~tot, type = "bar", visible = TRUE,
                  text = ~emoji,
                  textposition = "outside",
                  textfont = list(size = 18),
                  customdata = ~value,  
                  hovertemplate = paste0('Rank: %{x}<br>',
                                         '<b>%{customdata}</b><br>',
                                         'Used %{y} Times<extra></extra>'),
                  marker = list(color = '99d8c9',
                                line = list(width = 2, color = '2ca25f'))) %>%
        # frame & hover layout
        layout(xaxis = ax, yaxis = ax,
               hoverlabel = list(font = list(size = 14))) %>%
        layout(xaxis = list(title = list(font = list(color = 'grey30')), 
                            tickvals = ~rank,
                            showticklabels = FALSE,
                            tickfont = list(color = 'grey30'),
                            tickangle = 60,
                            range = c(df$rank - 1, df$rank + 1)
        ),
        yaxis = list(title = list(text = '<b>Times Used</b>', 
                                  standoff = 10,
                                  font = list(color = 'grey30')),
                     tickfont = list(color = 'grey30'),
                     range = c(0,max(df$tot) * 1.1)
        )
        ) %>%
        layout(annotations = list(text = '<i>Out of a total 165,719 Tweets</i>',
                                  showarrow = FALSE,
                                  x = max(df$rank) + 0.5, xanchor = "right",
                                  y = 0, yanchor = "top", yshift = -6)
               )
}

# 3.2) top hashtags  ----
# prepping data

nyr_hashtag_freq_table <- nyresolution_data$text %>% 
    str_extract_all('#\\w+') %>%
    unlist() %>% str_to_lower() %>% 
    as_tibble() %>% group_by(value) %>%
    summarise(tot = n()) %>% 
    arrange(desc(tot))


plot_hashtag_chart <- function(df) {
    ax <- list(
        zeroline = FALSE,
        showline = TRUE,
        mirror = FALSE, "ticks",
        showgrid = FALSE,
        linecolor = toRGB('grey30'),
        linewidth = 4
    )
    df %>% 
        plot_ly() %>%
        add_trace(x = ~rank, y = ~tot, type = "bar", visible = TRUE,
                  customdata = ~rank,
                  hovertemplate = paste0('Rank: %{customdata}<br>',
                                         '<b>%{x}</b><br>',
                                         'Used %{y} Times<extra></extra>'),
                  marker = list(color = 'bcbddc',
                                line = list(width = 2, color = '756bb1'))) %>%
        # frame & hover layout
        layout(xaxis = ax, yaxis = ax,
               hoverlabel = list(font = list(size = 14))) %>%
        # title layout
        # layout(title = list(text = '<b>Most common hashtags</b><br>',
        #                    font = list(size = 20, color = 'grey30')
        # )
        # ) %>%
        # x and y axis layout
        layout(xaxis = list(title = list(font = list(color = 'grey30')), 
                            tickvals = ~rank,
                            ticktext = ~value,
                            tickfont = list(color = 'grey30'),
                            tickangle = 60,
                            range = c(0, nrow(df)+1)
        ),
        yaxis = list(title = list(text = '<b>Times Used</b>', 
                                  standoff = 10,
                                  font = list(color = 'grey30')),
                     tickfont = list(color = 'grey30')
        )
        ) %>%
        layout(annotations = list(text = '<i>Out of a total 165,719 Tweets</i>',
                                  showarrow = FALSE,
                                  x = max(df$rank) + 0.5, xanchor = "right",
                                  y = 0, yanchor = "top", yshift = -6)
        )
}

# 3.3) Pairs of emojis -----
# one tag followed by one or more tags which may or may not be preceded by a space
emoji_chain_pattern <- '(jaugur_[\\w-]+)(\\s*(jaugur_[\\w-]+))+'

#' @param string A string containing 2+ non-space sequences separated by a space
#' @param presplit If `TRUE`, assumes string has already been through `str_split(string, ' ')`
#' @return 
#' A character vector of length L where L is the number of pairs found.
#' Each string is two non-space sequences separated by a space. 
return_pairs <- function(string, presplit = FALSE) {
    if (!presplit) {
        splitted <- str_split(string, ' ')[[1]]
    } else {
        splitted <- string
    }
    
    if (length(splitted) < 2) {
        rlang::abort('String must contain 2+ non-space sequences separated by a space.')
    } else if (length(splitted) == 2) {
        return(paste(splitted, collapse = ' '))
    } else {
        from_ <- 1:(length(splitted) - 1)
        to_ <- from_ + 1
        res <- purrr::map2(from_, to_,
                    function(.x, .y) paste(splitted[.x:.y], collapse = ' ')
        )
    }
    unlist(res)
}

emoji_pairs <- nyresolution_data %>% select(ID, text_with_emojis_replaced) %>%
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
    mutate(vec_of_pair = str_split(pair_, ' '),
             emoji1 = map_chr(vec_of_pair, function(x) x[1]), # alternative: str_extract(pair_,'^jaugur_[\\w-]+')
             emoji2 = map_chr(vec_of_pair, function(x) x[2]), # alternative: str_extract(pair_,'jaugur_[\\w-]+$')
             emojis_sorted = map2_chr(emoji1, emoji2, function(.x, .y) {
                     paste(str_sort(c(.x, .y)), collapse = ' ')
                 })
             )

# based on sorted, the two laughing combos together are in 2nd place
sorted_emoji_pairs <- emoji_pairs %>% filter(emoji1 != emoji2) %>% 
    group_by(emojis_sorted) %>%
    summarise(tot = n()) %>%
    arrange(desc(tot)) %>%
    rename(pair_ = emojis_sorted) # columns = pair_ and tot

# based on order, the two laughing ones are in place 3 and 4
unsorted_emoji_pairs <- emoji_pairs %>% filter(emoji1 != emoji2) %>% 
    group_by(pair_) %>%
    summarise(tot = n()) %>%
    arrange(desc(tot)) # columns = pair_ and tot

# see which emoji was most commonly used with itself
twin_emoji_pairs <- emoji_pairs %>% filter(emoji1 == emoji2) %>%
    group_by(pair_) %>%
    summarise(tot = n()) %>%
    arrange(desc(tot)) # columns = pair_ and tot

plot_sankey <- function(data, from_ = NULL, to_ = NULL) {
    if (is.null(from_)) from_ <- 1
    if (is.null(to_)) to_ <- nrow(data)
    d <- data[from_:to_, ]
    unique_descriptions <- unique(unlist(str_split(d$pair_, ' ')))
    # lookup table
    source_lookup <- seq_along(unique_descriptions) - 1
    names(source_lookup) <- unique_descriptions
    # lookup table
    target_lookup <- seq_along(unique_descriptions) + length(unique_descriptions) - 1
    names(target_lookup) <- unique_descriptions
    # joining to emojis and untagged desc
    unique_emoji_data <- tibble(tagged_desc = unique_descriptions) %>% 
        left_join(select(emoji_data, tagged_desc, description, emoji), by = 'tagged_desc') %>%
        # removing duplicates due to left join where two emojis have the same
        distinct(tagged_desc, .keep_all = T) %>%
        mutate(description = str_replace_all(description, '^flag$', 'country flag'))
        
    sankey_data <- d %>% 
            mutate(emoji1 = str_extract(pair_,'^jaugur_[\\w-]+'),
                   emoji2 = str_extract(pair_,'jaugur_[\\w-]+$'),
                   source_ = map(emoji1, function(x) source_lookup[[x]]) %>% unlist(),
                   target_ = map(emoji2, function(x) target_lookup[[x]]) %>% unlist(),
            )
    # create fig
    fig <- plot_ly(
        type = "sankey",
        orientation = "h",
        valueformat = ".0f",
        node = list(
            label = rep(unique_emoji_data$emoji, 2),
            customdata = rep(unique_emoji_data$description, 2),
            hovertemplate = '%{customdata}',
            color = rep('blue', length(unique_emoji_data$emoji) *2),
            pad = 15,
            thickness = 10,
            line = list(
                color = "black",
                width = 0.5
            )
        ),
        link = list(
            source = sankey_data$source_,
            target = sankey_data$target_,
            value =  sankey_data$tot,
            hovertemplate = '"%{source.customdata}" & "%{target.customdata}"'
        )
    ) %>% 
        # add layout
        layout(
        #title = "Basic Sankey Diagram",
        font = list(
            size = 20
        )
    )
    
    fig
    
}

twin_emoji_pairs %>% plot_sankey(to_ = 5)
unsorted_emoji_pairs %>% plot_sankey(to_ = 15)
sorted_emoji_pairs %>% plot_sankey(to_ = 30)

# no filtering if emoji1 and 2 are the same
emoji_pairs %>% group_by(pair_) %>%
    summarise(tot = n()) %>%
    arrange(desc(tot)) %>%
    plot_sankey(to_ = 15)

en <- 15
u_list <- sorted_emoji_pairs %>% head(en) %>% pull(emojis_sorted) %>% 
    str_split(' ') %>% unlist() %>%
    unique()

emoji_sankey_lookup_src <- seq_along(u_list) - 1
names(emoji_sankey_lookup_src) <- u_list

emoji_sankey_lookup_trg <- seq_along(u_list) + length(u_list) - 1
names(emoji_sankey_lookup_trg) <- u_list

u_emoji_data <- tibble(tagged_desc = u_list) %>% 
    left_join(select(emoji_data, tagged_desc, description, emoji), by = 'tagged_desc') %>%
    distinct(tagged_desc, .keep_all = T) %>% # removing duplicates due to left join where two emojis have the same desc
    mutate(value = str_replace_all(description, '^flag$', 'country flag'))
    
sankey_data <- sorted_emoji_pairs %>% head(en) %>% 
    mutate(emoji1 = str_extract(emojis_sorted,'^jaugur_[\\w-]+'),
             emoji2 = str_extract(emojis_sorted,'jaugur_[\\w-]+$'),
             source_ = map(emoji1, function(x) emoji_sankey_lookup_src[[x]]) %>% unlist(),
             target_ = map(emoji2, function(x) emoji_sankey_lookup_trg[[x]]) %>% unlist(),
             )

fig <- plot_ly(
    type = "sankey",
    orientation = "h",
    valueformat = ".0f",
    node = list(
        label = rep(u_emoji_data$emoji, 2),
        customdata = rep(u_emoji_data$description, 2),
        hovertemplate = '%{customdata}',
        color = rep('blue', length(u_emoji_data$emoji) *2),
        pad = 15,
        thickness = 10,
        line = list(
            color = "black",
            width = 0.5
        )
    ),
    link = list(
        source = sankey_data$source_,
        target = sankey_data$target_,
        value =  sankey_data$tot,
        hovertemplate = '"%{source.customdata}" & "%{target.customdata}"'
    )
)
fig <- fig %>% layout(
    #title = "Basic Sankey Diagram",
    font = list(
        size = 20
    )
)

fig

plot_sorted_emoji_pairs <- function(df) {
    ax <- list(
        zeroline = FALSE,
        showline = TRUE,
        mirror = FALSE, "ticks",
        showgrid = FALSE,
        linecolor = toRGB('grey30'),
        linewidth = 4
    )
    df %>% mutate(emoji = str_replace_na(emoji, '')) %>%
        plot_ly() %>%
        add_trace(x = ~rank, y = ~tot, type = "bar", visible = TRUE,
                  text = ~emoji,
                  textposition = "outside",
                  textfont = list(size = 18),
                  customdata = ~value,  
                  hovertemplate = paste0('Rank: %{x}<br>',
                                         '<b>%{customdata}</b><br>',
                                         'Used %{y} Times<extra></extra>'),
                  marker = list(color = '99d8c9',
                                line = list(width = 2, color = '2ca25f'))) %>%
        # frame & hover layout
        layout(xaxis = ax, yaxis = ax,
               hoverlabel = list(font = list(size = 14))) %>%
        # title layout
        layout(xaxis = list(title = list(font = list(color = 'grey30')), 
                            tickvals = ~rank,
                            showticklabels = FALSE,
                            tickfont = list(color = 'grey30'),
                            tickangle = 60,
                            range = c(df$rank - 1, df$rank + 1)
        ),
        yaxis = list(title = list(text = '<b>Times Used</b>', 
                                  standoff = 10,
                                  font = list(color = 'grey30')),
                     tickfont = list(color = 'grey30'),
                     range = c(0,max(df$tot) * 1.1)
        )
        ) %>%
        layout(annotations = list(text = '<i>Out of a total 165,719 Tweets</i>',
                                  showarrow = FALSE,
                                  x = max(df$rank) + 0.5, xanchor = "right",
                                  y = 0, yanchor = "top", yshift = -6)
        )
}
