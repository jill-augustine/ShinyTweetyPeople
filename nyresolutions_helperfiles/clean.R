# This script creates 'emoji_data', (tweet)'data', 'lang_info_df',  and saves them as tsv

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


# defining functions ---------
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

# emoji data ---------------

emoji_data_raw <- readr::read_delim(file.path(getwd(),'emoji13-1','emoji-test.txt'),
                                    delim = ';', skip = 35,
                                    col_names = c('code','notes')) %>% 
    as_tibble() %>% 
    filter(!is.na(notes)) %>%
    mutate(code = str_trim(code),
           notes = str_trim(notes))

emoji_data <- emoji_data_raw$notes %>% 
    # spliting on # or ' Ex.x ' which splits in 3, then dropping first column of matrix
    str_split_fixed('(#\\s)|(\\sE\\d{1,2}\\.\\d\\s)', n = 3) %>% 
    as_tibble() %>% rename('emoji' = 'V2', 'description' = 'V3') %>%
    select(-V1) %>%
    mutate(ASCII = iconv(emoji, 'UTF-8','ASCII','byte'))

emoji_data <- bind_cols(emoji_data_raw, emoji_data) %>%
    # add the "jaugur_" tag to the description and remove the underscores from the desc
    mutate(tagged_desc = str_c('jaugur_',str_replace_all(description,'\\s+','_')))

emojis <- emoji_data %>% pull(emoji)
urls <- vector(mode = "character", length = nrow(emoji_data))

# this takes approx 1 hr!
for (i in 1:nrow(emoji_data)) {
    cat(paste(i, 'of', nrow(emoji_data)), emojis[i])
    cat('\n')
    urls[i] <- emoji_to_link(emojis[i])
    Sys.sleep(0.2)
}
emoji_data <- emoji_data %>% mutate(url = urls)

emoji_data %>% mutate(across(everything(), as.character)) %>% 
    tidyr::unite('all', sep = ' ') %>% pull() %>% str_locate_all('\\t') %>%
    unlist()
emoji_data %>% readr::write_tsv(file.path("emoji13-1","emojidata.tsv"))


# saving emoji pngs to file
emoji_filenames <- emoji_data$url %>% str_split('/') %>% map_chr(last)
for (i in 1:nrow(emoji_data)) {
    if (is.na(emoji_data$url[i])) {next}
    cat(i, "of", nrow(emoji_data))
    download.file(emoji_data$url[i], file.path('emoji13-1','png',emoji_filenames[i]))
}

# this has 4590 of the latest emojis
# emoji_DT <- data.table::data.table(x = emoji_data$ASCII, y = emoji_data$description)

# tweet data -------
replies_pattern <- '^(@\\w+\\s*)+'
tag_pattern <- '@\\w+'
hashtag_pattern <- '#\\w+'

txt_import <- readr::read_file(file.path('nyresolutions','20210103141011_tidy.json'))
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
data %>% mutate(across(everything(), as.character)) %>% 
    tidyr::unite('all', sep = ' ') %>% pull() %>% str_locate_all('\\t') %>%
    unlist()
data %>% readr::write_tsv(file.path("nyresolutions","tweetdata.tsv"))

# NOTE an emoji with UTF code \U0001f497 as shown in r has a real code of U+1F497

# not run
# this checks that the only twitter links are those wth 10 characters
# data$text %>% head(200) %>% 
#     str_remove_all('https://t.co/\\w{10}s*') %>% # remove the ones with length of 10
#     str_extract_all('https://t.co/\\w+s*') %>% # extract what is left
#     unlist() %>% unique()


# not run
# identifying html characters ("&amp;" "&gt;"  "&lt;" )
# data$text %>%
#     str_extract_all('&\\w+;') %>% 
#     unlist() %>% unique()

# language code data ------------------------------------

language_codes_import <- readr::read_file('language-subtag-registry.txt')

# split into one language per line
languages <- (language_codes_import %>% str_split('\\n%%\\n', simplify = FALSE))[[1]] #  taking 1st element because the input was only one long string , if the input has been a vector of 2, the list would have 2 elements

# split each language into (a list of) elements containing Type, Subtag, Description and other elements
# list of char_vecs
languages_split <- (languages[2:length(languages)] %>% str_split('\\n', simplify = FALSE)) # the first item was just the time the file was created

# making a function to make a tibble out of each character vecotr
create_tibble_from_char <- function(char_vec) {
    # extracting the values after the ":" for each item in the character vector
    element_values <- char_vec %>% str_extract('(?<=:\\s)[:alnum:]+')
    # putting the relevant values together in a tibble
    tibble(Type = element_values[1],
           Subtag = element_values[2], Description = element_values[3])
}

tic(quiet = FALSE)
# list of tibbles (takes approx 20s for a list of length 9122)
lang_info_list <- map(languages_split, create_tibble_from_char)
toc()

lang_info_df <- bind_rows(lang_info_list)

lang_info_df %>% mutate(across(everything(), as.character)) %>% 
    tidyr::unite('all', sep = ' ') %>% pull() %>% str_locate_all('\\t') %>%
    unlist()
lang_info_df %>% readr::write_tsv(file.path("nyresolutions","language_info.tsv"))

# time zone data ---------------

tz_import <- readr::read_delim(file.path('nyresolutions','timezonenames.tsv'), '\t', skip = 1)
tz_import <- tz_import[c(25:14, 1:13),] # rearranging rows

