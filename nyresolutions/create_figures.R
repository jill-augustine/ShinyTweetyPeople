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
    # title layout
    #layout(title = list(text = '<b>Peaks in tweets per hour as regions passed midnight</b><br>',
    #                    font = list(size = 16, color = 'grey30')
    #                    ),
    #       margin = list(t = 60)
    #       ) %>%
    # x and y axis layout
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
                        #range = c(0, 5000),
                        tickfont = list(color = 'grey30')
                                       )
        ) %>%
    layout(shapes = list(type = 'line', x0 = 24, x1 = 24, y0 = 0, y1 = 4500,
                         opacity = 0.8, 
                         line = list(dash = 'dash')
                         ))
    
#fig_nyr_tweets_per_hour

# 3.2) top emojis  ----
# prepping data

nyr_emoji_freq_table <- nyresolution_data$text_with_emojis_replaced %>%
    str_extract_all('jaugur_[\\w-]+') %>% # this also removes the ":_medium_skin_tone" part of the desc but keeps the "-eyes" in "heart-eyes"
    unlist() %>% str_remove_all('jaugur_') %>% str_replace_all('_',' ') %>%
    as_tibble() %>% group_by(value) %>%
    summarise(tot = n()) %>% # creating a frequency table
    arrange(desc(tot)) %>% 
    # adding the emoji itself
    left_join(select(emoji_data, emoji, description, url), by = c('value' = 'description'))  %>%
    distinct(value, .keep_all = T) %>% # removing duplicates due to left join where two emojis have the same desc
    mutate(value = str_replace_all(value, '^flag$', 'country flag'))

# creating figure (this should actually be done within server or with shiny functions to allow for reactivity)

#emoji_from <- 10
#emoji_to <- 20
#emoji_freq_subset <- nyr_emoji_freq_table %>% 
#    mutate(rank = seq_along(emoji)) %>%
#    .[emoji_from:emoji_to,] %>%
#    mutate(value = factor(value, levels = value))

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
        # title layout
        #layout(title = list(text = '<b>Most common emojis</b><br>',
        #                    font = list(size = 20, color = 'grey30')
        #)
        #) %>%
        # x and y axis layout
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

#plot_emoji_chart(emoji_freq_subset)

# 3.2) top hashtags  ----
# prepping data

nyr_hashtag_freq_table <- nyresolution_data$text %>% #head(10) %>%
    str_extract_all('#\\w+') %>%
    unlist() %>% str_to_lower() %>% 
    as_tibble() %>% group_by(value) %>%
    summarise(tot = n()) %>% # creating a frequency table
    arrange(desc(tot))

# creating figure (this should actually be done within server or with shiny functions to allow for reactivity)

#hashtag_from <- 1
#hashtag_to <- 10
#hashtag_freq_subset <- nyr_hashtag_freq_table %>% .[hashtag_from:hashtag_to,] %>%
#    mutate(value = factor(value, levels = value),
#           rank = seq_along(value))

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

#plot_hashtag_chart(hashtag_freq_subset)

# 3.3) Pairs of emojis -----
# one tag followed by zero or more tags which may or may not be preceded by a space
emoji_chain_pattern <- '(jaugur_[\\w-]+)(\\s*(jaugur_[\\w-]+))*'
x <- nyresolution_data %>% .[117,] %>% 
    mutate(emoji_chain = str_extract(text_with_emojis_replaced, emoji_chain_pattern)) %>%
    select(text, text_with_emojis_replaced, emoji_chain)

cat(x$text)
x$emoji_chain

eg <- "jaugur_smiling_face_with_hearts jaugur_rolling_on_the_floor_laughing jaugur_grinning_face_with_sweat"
eg2 <- "jaugur_smiling_face_with_hearts jaugur_rolling_on_the_floor_laughing something else jaugur_grinning_face_with_sweat jaugur_grinning_face_with_sweat"
str_extract(eg, emoji_chain_pattern) # finds all
str_extract(eg2, emoji_chain_pattern) # finds first two
str_extract_all(eg, emoji_chain_pattern) # finds all in vec of length 1
str_extract_all(eg2, emoji_chain_pattern) # finds all in vec of length 2

## Trying joint emojis only
# one tag followed by one or more tags which may or may not be preceded by a space
emoji_chain_pattern2 <- '(jaugur_[\\w-]+)(\\s*(jaugur_[\\w-]+))+'

str_extract(eg, emoji_chain_pattern2) # finds all
str_extract(eg2, emoji_chain_pattern2) # finds first two
str_extract_all(eg, emoji_chain_pattern2) # finds all in list of length 1 and vec of length 1
str_extract_all(eg2, emoji_chain_pattern2) # finds all in list of length 1 and vec of length 2

return_pairs <- function(string, presplit = FALSE) {
    # if presplit then string has already been through str_split(string, ' ')
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

x <- nyresolution_data %>% #.[1:1000,] %>% 
    select(ID, text_with_emojis_replaced) %>%
    mutate(emoji_chain = str_extract_all(text_with_emojis_replaced, emoji_chain_pattern2)) %>% # makes a vector per row
    tidyr::unnest(cols = emoji_chain) %>% # unnests to one string per row with ID and text_with_emojis_replaced columns duplicated
    
    mutate(pre_split = str_split(emoji_chain, ' ')) %>%# prep for return_pairs
    mutate(pair_ = map(pre_split, return_pairs, presplit = TRUE)) %>% # makes pair_ which is one char vector per row
    select(ID, pair_) %>%
    tidyr::unnest(cols = pair_) %>% # unnests to one string per row
    mutate(vec_of_pair = str_split(pair_, ' '),
             emoji1 = map_chr(vec_of_pair, function(x) x[1]),
             emoji2 = map_chr(vec_of_pair, function(x) x[2]),
             emojis_sorted = map2_chr(emoji1, emoji2, function(.x, .y) {
                     paste(str_sort(c(.x, .y)), collapse = ' ')
                 })
             )


# based on sorted, the two laughing combos together are in 2nd place
x %>% filter(emoji1 != emoji2) %>% 
    group_by(emojis_sorted) %>%
    summarise(tot = n()) %>%
    arrange(desc(tot)) 

# based on order, the two laughing ones are in place 3 and 4
x %>% filter(emoji1 != emoji2) %>% 
    group_by(pair_) %>%
    summarise(tot = n()) %>%
    arrange(desc(tot)) 

# see which emoji was most commonly used with itself
x %>% filter(emoji1 == emoji2) %>%
    group_by(emoji1) %>%
    summarise(tot = n()) %>%
    arrange(desc(tot)) 

#y <- x$pre_split[[1]]
#y <- c(y,y)
#y
#z <- tibble(from_ = 1:3, to_ = from_ + 1)

tic()
x %>% mutate(emoji_pairs = return_pairs(pre_split, presplit = TRUE))
toc()

str_split(x$emoji_chain, ' ') %>% map(length) %>% unlist() %>% min()

tibble(eg2) %>% 
    mutate(one = str_extract_all(eg2, emoji_chain_pattern2)) %>% 
    tidyr::unnest(cols = one) #unnest_regex(output = 'chains', input = 'eg2', pattern = emoji_chain_pattern2)

emoji_pair_pattern <- '(jaugur_[\\w-]+)\\s*(jaugur_[\\w-]+)'
str_extract(eg, emoji_pair_pattern) # first two
str_extract_all(eg, emoji_pair_pattern) # first two

one <- str_extract(eg, emoji_chain_pattern2) # finds all
one %>% str_split(' ') %>% map(length)

# might have to make function at some point which takes a vectors not list and outputs vector

# data should be in the form of a data frame with the ID in one column and the string in another
# if a tweet contained more than one set of emojis, the ID might be duplicated


