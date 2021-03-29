#!/usr/bin/env Rscript
# This script creates (tweet)'data' and saves it as tsv

# Load packages -------------------
# library(jsonlite)
library(dplyr)
library(stringr)
library(tictoc)
#library(ggplot2)
#library(tidyjson)
#library(plotly)
#library(textclean)
#library(rvest)
library(purrr)

# 1) Loading helper data ----------
tz_import <- readr::read_tsv(file.path('nyresolutions','timezonenames.tsv'), 
                             col_types = 'ccc', skip = 1) %>%
    .[c(25:14, 1:13),] # rearranging rows to order from west to east
emoji_data <- readr::read_tsv(file.path("emoji13-1","emojidata.tsv"), 
                              col_types = 'ccccccc')

# tweet data -------
replies_pattern <- '^(@\\w+\\s*)+'
tag_pattern <- '@\\w+'
hashtag_pattern <- '#\\w+'

txt_import <- readr::read_file(file.path('valentinesday_helperfiles','20210217222505_tidy.json'))
df_import <- jsonlite::fromJSON(txt_import, simplifyVector = FALSE, simplifyDataFrame = TRUE, simplifyMatrix = FALSE) %>% as_tibble()

data <- df_import %>% 
    mutate(created_at = lubridate::as_datetime(created_at), 
           num_replying_to = text %>% str_extract_all(replies_pattern, simplify = TRUE) %>% 
               str_count(tag_pattern), # finding tags at the beginning of the text
           text_wo_replies_links = text %>% 
               str_remove_all('^(@\\w+\\s+)+') %>% # removing these tags
               #str_remove_all('@\\w+') %>% # also removes replies at the end of tweets
               str_remove_all('https://t.co/\\w{10}s*') %>%
               str_trim(), 
           num_tagged = text_wo_replies_links %>% 
               str_extract_all(tag_pattern, simplify = FALSE) %>%  # finding tags within the text
               map(length) %>% unlist(),
           num_hashtags = text %>% str_extract_all(hashtag_pattern, simplify = FALSE) %>% 
               map(length) %>% unlist(), # counting the number of hashtags
           text_length = text_wo_replies_links %>% str_replace_all('&\\w+;', '§') %>% str_length(), # catches e.g. "&amp;" and "&lte;" str_length() can change depending on the encoding
           text_count = text_wo_replies_links %>% str_replace_all('&\\w+;', '§') %>% str_count() # str_count() is the number of characters
    )# changed some characters to § for calculating length of the tweet

# THIS TAKES AT LEAST 20 MINS
message('tagging emojis')
tic()
data <- data %>% 
    # replacing emojis with the "jaugur_"-tagged description
    # for later emoji analysis
    mutate(text_with_emojis_replaced = 
               textclean::replace_emoji(x = text_wo_replies_links,
                                        emoji_dt = data.table::data.table(x = emoji_data$ASCII, 
                                                                          y = emoji_data$tagged_desc
                                        ))
    )
toc()

# checks no tabs are in the data before saving as tsv
num_tabs <- data %>% mutate(across(everything(), as.character)) %>% 
    tidyr::unite('all', sep = ' ') %>% pull() %>% str_locate_all('\\t') %>%
    unlist() %>% length()

if (num_tabs == 0) {
    message(paste('writing to', file.path("valentinesday","tweetdata.tsv")))
data %>% readr::write_tsv(file.path("valentinesday","tweetdata.tsv"))
} else {
    rlang::warn(paste(num_tabs, 'tabs were found in data. Writing to plain file'))
     readr::write_file(file.path('valentinesday','tweetdata.txt'))
}