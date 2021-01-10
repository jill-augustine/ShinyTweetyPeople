# creates 'data' and 'lang_info_df'
library(jsonlite)
library(dplyr)
library(stringr)
library(purrr)
library(tictoc)
library(ggplot2)
library(tidyjson)
library(plotly)

replies_pattern <- '^(@\\w+\\s*)+'
tag_pattern <- '@\\w+'
hashtag_pattern <- '#\\w+'
# paste0('^(',tag_pattern,'\\s*)+') # this is the same as the replies pattern


#json <- readr::read_file('all_joined.json')
txt_import <- readr::read_file('20210103141011_tidy.json')
df_import <- fromJSON(txt_import, simplifyVector = FALSE, simplifyDataFrame = TRUE, simplifyMatrix = FALSE) %>% as_tibble()

data <- df_import %>% 
    mutate(created_at = lubridate::as_datetime(created_at), 
           num_replying_to = text %>% str_extract_all(replies_pattern, simplify = TRUE) %>% 
                             str_count(tag_pattern), # finding tags at the beginning of the text
           text_wo_replies_links = text %>% str_remove_all('^(@\\w+\\s+)+') %>% # removing these tags
                             str_remove_all('https://t.co/\\w{10}s*') %>%
                             str_trim(), 
           num_tagged = text_wo_replies_links %>% 
                        str_extract_all(tag_pattern, simplify = FALSE) %>%  # finding tags within the text
                        map(length) %>% unlist(),
           num_hashtags = text %>% str_extract_all(hashtag_pattern, simplify = FALSE) %>% 
                          map(length) %>% unlist(), # counting the number of hashtags
           text_length = text_wo_replies_links %>% str_replace_all('&\\w+;', '§') %>% str_length(), # this can change depending on the encoding
           text_count = text_wo_replies_links %>% str_replace_all('&\\w+;', '§') %>% str_count() # this is the number of characters
           )# changed some characters to § for calculating length of the tweet


max(data$text_length) #539
max(data$text_count) #280 !!! finally

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

# getting supporting data on language codes ------------------------------------

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

