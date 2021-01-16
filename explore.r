# requires 'data' and 'lang_info_df' as created in clean.r
library(jsonlite)
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

replies_pattern <- '^(@\\w+\\s*)+'
tag_pattern <- '@\\w+'
hashtag_pattern <- '#\\w+'

# defining functions
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

# creating summary statistics and charts ---------------------------------------

# Top languages ------------
num_tweets_per_lang <- data %>% group_by(lang) %>% 
    summarise(num = n()) %>%
    arrange(desc(num)) %>%
    left_join(lang_info_df %>% filter(Type == 'language') %>% select(Subtag, Description), 
              by = c('lang' = 'Subtag')) %>%
    mutate(pct = num / sum(num) * 100)

# static
num_tweets_per_lang %>% head(20) %>%
    ggplot(aes(x = factor(Description, levels = rev(Description)), y = pct)) +
    geom_bar(stat = 'identity') +
    geom_text(aes(label = round(pct,2)), nudge_y = 4) +
    coord_flip() +
    scale_y_continuous(breaks = c(0,25,50,75,100), labels = str_c(c(0,25,50,75,100), '%')) +
    labs(title = paste('Top 20 tweeted languages (of',nrow(data),'tweets)'), 
         subtitle = 'Tweets containing both "new" and "resolution".', x = 'Language', y = 'Proportion of Tweets')

# Interacitve: To do

# Length of tweet ------------
# this is not so interesting
# static

data %>%
    ggplot(aes(text_count)) +
    geom_histogram(binwidth = 10, boundary = 0, 
                   fill = 'white', colour = 'black') + 
    labs(title = 'Over 15,000 tweets contained 50-60 characters',
         subtitle = paste0('Total tweets: ', nrow(data))) +
    scale_x_continuous(breaks = seq(0,280,70)) +
    scale_y_continuous(sec.axis = sec_axis(
                                        trans = ~./nrow(data)*100,
                                        name = 'Percentage of Total Tweets'))

# Time of tweet --------------------

breaks <- seq(ymd_hms('2020-12-31 12:00:00'), ymd_hms('2021-01-01 11:00:00'), 3600)
labels <- tz_import$Name[2:nrow(tz_import)]

tweets_per_hour <- data %>% mutate(d = floor_date(created_at, unit = '1 hour')) %>%
    group_by(d) %>%
    summarise(cnt = n()) %>% 
    filter(d >= ymd_hms('2020-12-31 12:00:00'), 
           d < ymd_hms('2021-01-01 12:00:00'), ) %>%
    arrange(desc(d)) %>%
    mutate(d = as.character(d), x = seq_along(d))
tweets_per_hour %>%
    ggplot(aes(x = x, y = cnt)) +
    geom_line() +
    annotate('text', x = 7, y = tweets_per_hour$cnt[7] + 250, label = labels[7]) +
    annotate('text', x = 12, y = tweets_per_hour$cnt[12] + 250, label = labels[12]) +
    annotate('text', x = 20, y = tweets_per_hour$cnt[20] + 250, label = labels[20]) +
    scale_x_continuous(breaks = seq(2,24,2), labels = tz_import$GMT_Offset[seq(3,25,2)],
                     #sec.axis = sec_axis(function(.) ., 
                     #                    #breaks = seq(1,24),
                     #                    breaks = c(7,12,20),
                     #                    #labels = labels,
                     #                    labels = labels[c(7,12,20)])
                     ) +
    labs(title = "Peaks in tweets per hour as regions passed midnight.") +
    xlab('Time Zone') +
    ylab('Number of Tweets') +
    theme_minimal() +
    theme(#axis.text.x.top = element_text(angle = 45, hjust = 0),
          axis.text.x.bottom  = element_text(angle = 45, hjust = 1),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank())

# Most used emojis ---------

# add a noteable tag to every emoji and join all words with underscores so I can search for bords beginning
# with jaugur later

tagged_emoji_data <- emoji_data %>% mutate(tagged_desc = str_c('jaugur_',
                                      str_replace_all(description,'\\s+','_')))
# --------
# tic()
# replaced_emojis <- parallel::mclapply(data$text_wo_replies_links, FUN = function(txt) {
#     replace_emoji(txt, data.table::data.table(x = tagged_emoji_data$ASCII,
#                                                  y = tagged_emoji_data$tagged_desc))
#     },
#                 mc.cores = parallel::detectCores() -1)
# toc()
# 474.967 sec elapsed
#
# data <- data %>% mutate(txt_with_emojis_replaced = unlist(replaced_emojis))
# data %>% readr::write_rds('df_with_emojis_replaced.rds')
data <- readr::read_rds('df_with_emojis_replaced.rds')

common_emojis <- data$txt_with_emojis_replaced %>%
    str_extract_all('jaugur_\\w+') %>% # this also removes the ":_medium_skin_tone" part of the desc
    unlist() %>% str_remove_all('jaugur_') %>% str_replace_all('_',' ') %>%
    as_tibble() %>% group_by(value) %>%
    summarise(tot = n()) %>% # creating a frequency table
    arrange(desc(tot)) %>% 
    # adding the emoji itself
    left_join(select(emoji_data, emoji, description), by = c('value' = 'description'))  %>%
    distinct(value, .keep_all = T) # removing duplicates due to left join where two emojis have the same desc

top_emojis <- common_emojis %>%
    slice(1:10) %>%
    mutate(url = map_chr(emoji, slowly(~emoji_to_link(.x), rate_delay(0.2))),
           label = link_to_img(url))

top_emojis %>% 
    ggplot(aes(x = 1, y = tot, label = label)) +
    geom_richtext(position = position_jitter(height = 0),
                  fill = NA, label.color = NA,
                  label.padding = grid::unit(rep(0, 4), "pt")
    ) +
    scale_y_log10() +
    theme_minimal()
# option 1: scatter  ----
top_emojis %>%
    ggplot(aes(emoji, tot, label = label)) +
    geom_richtext(aes(y = tot), 
                  fill = NA, label.color = NA, # remove background and outline
                  label.padding = grid::unit(rep(0, 4), "pt") # remove padding
    ) +
    theme_minimal()

offset <- max(top_emojis$tot) / 20 # 5% of the largest value

# option 2: geom_col with emojis on top -------------
top_emojis %>%
    ggplot(aes(forcats::fct_reorder(emoji, tot, .desc = TRUE), tot, label = label)) +
    geom_col() +
    geom_richtext(aes(y = tot + offset), fill = NA, label.color = NA,
                  label.padding = grid::unit(rep(0, 4), "pt")
    ) +
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank()) +
    labs(x = NULL) +
    theme_minimal()

# option 3: geom_col with emojis as tick labels
top_emojis %>%
    ggplot(aes(forcats::fct_reorder(label, tot, .desc = TRUE), tot)) +
    geom_col() +
    theme_minimal() +
    theme(axis.text.x = element_markdown()) +
    labs(x = NULL)


x <- '😏'
x %>% emoji_to_link() %>% link_to_img()

# Most used emjois per region ----------
# plit into GMT-5, GMT and GMT +8

# Top words

# Most meaningful words

# Number of emojis used

# favourite emojis used

# MISC : testing snippets -------------
# testing emojis

top_emojis <- common_emojis %>%
    slice(1:10) %>%
    mutate(url = map_chr(emoji, slowly(~emoji_to_link(.x), rate_delay(0.2))),
           label = link_to_img(url))
# option 1: scatter  ----
top_emojis %>%
    ggplot(aes(emoji, tot, label = label)) +
    geom_richtext(aes(y = tot), fill = NA, label.color = NA, # remove background and outline
                  label.padding = grid::unit(rep(0, 4), "pt") # remove padding
    ) +
    theme_minimal()

offset <- max(top_emojis$tot) / 20 # 5% of the largest value

# option 2: geom_col with emojis on top -------------
top_emojis %>%
    ggplot(aes(forcats::fct_reorder(emoji, tot, .desc = TRUE), tot, label = label)) +
    geom_col() +
    geom_richtext(aes(y = tot + offset), fill = NA, label.color = NA,
                  label.padding = grid::unit(rep(0, 4), "pt")
    ) +
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank()) +
    labs(x = NULL) +
    theme_minimal()

# option 3: geom_col with emojis as tick labels
top_emojis %>%
    ggplot(aes(forcats::fct_reorder(label, tot, .desc = TRUE), tot)) +
    geom_col() +
    theme_minimal() +
    theme(axis.text.x = element_markdown()) +
    labs(x = NULL)
                                                            