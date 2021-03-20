#!/usr/bin/env Rscript
# 'emoji_data', (tweet)'data' with replaced emojis separated by a space

# Load packages -------------------
library(jsonlite)
library(dplyr)
library(stringr)
library(tictoc)
library(ggplot2)
library(tidyjson)
library(plotly)
library(textclean)
library(rvest)
library(purrr)

emoji_data <- readr::read_tsv(file.path("emoji13-1","emojidata.tsv"))

# tweet data -------
replies_pattern <- '^(@\\w+\\s*)+'
tag_pattern <- '@\\w+'
hashtag_pattern <- '#\\w+'

txt_import <- readr::read_file(file.path('nyresolutions_helperfiles','20210103141011_tidy.json'))
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
print('tagging emojis')
tic()
data <- data %>% 
    # replacing emojis with the "jaugur_"-tagged description
    # for later emoji analysis
    mutate(text_with_emojis_replaced = 
               textclean::replace_emoji(x = text_wo_replies_links,
                                        emoji_dt = data.table::data.table(
                                            x = emoji_data$ASCII, 
                                            y = str_c(emoji_data$tagged_desc, ' ') # add a space to make sequential emojis separable
                                        ))
    )
toc()

# checks no tabs are in the data before saving as tsv
data %>% mutate(across(everything(), as.character)) %>% 
    tidyr::unite('all', sep = ' ') %>% pull() %>% str_locate_all('\\t') %>%
    unlist()
data %>% readr::write_tsv(file.path("nyresolutions","tweetdata.tsv"))
