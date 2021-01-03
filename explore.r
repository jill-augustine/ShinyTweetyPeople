library(jsonlite)
library(dplyr)
library(stringr)
library(purrr)
library(tictoc)
library(ggplot2)

json <- readr::read_file('all_joined.json')

json2 <- fromJSON(json, simplifyVector = FALSE, simplifyDataFrame = TRUE, simplifyMatrix = FALSE) %>% tibble()

geo_df <- tibble(json2$geo) %>% mutate(id = json2$id)
data <- json2 %>% select(-geo) %>% 
    mutate(created_at = lubridate::as_datetime(created_at), 
           Length = str_length(text))

# need to remove any @ before a word that does not start with @

x <- "1345199238444183552"
txt <- data %>% filter(id == x) %>% pull(text)
txt2 <- paste(txt,txt)


txt2 %>% str_extract_all('^(@\\w+\\s+)+') # finds one or more @tags (@\\w+\\s+) if they are at the beginning of the tweet only, this identifies who the reply is to.

txt %>% str_replace_all('^(@\\w+\\s+)+','')

# getting supporting data on language codes ------------------------------------

language_codes_import <- readr::read_file('language-subtag-registry.txt')

# split into one language per line
languages <- (language_codes_import %>% str_split('\\n%%\\n', simplify = FALSE))[[1]] #  taking 1st element because the input was only one long string , if the input has been a vector of 2, the list would have 2 elements

# split each language into (a list of) elements containing Type, Subtag, Description and other elements
# list of char_vecs
tag_and_desc <- (languages[2:length(languages)] %>% str_split('\\n', simplify = FALSE)) # the first item was just the time the file was created

# making a function to make a tibble out of each character vecotr
create_tibble <- function(char_vec) {
    # extracting the values after the ":" for each item in the character vector
    element_values <- char_vec %>% str_extract('(?<=:\\s)[:alnum:]+')
    # putting the relevant values together in a tibble
    tibble(Type = element_values[1],
           Subtag = element_values[2], Description = element_values[3])
}

tic(quiet = FALSE)
# list of tibbles (takes approx 20s for a list of length 9122)
tag_desc_list <- map(tag_and_desc, create_tibble)
toc()

tag_desc_df <- bind_rows(tag_desc_list)

# creating summary statistics --------------------------------------------------

# Top languages 
num_tweets_per_lang <- data %>% group_by(lang) %>% 
    summarise(num = n()) %>%
    arrange(desc(num)) %>%
    left_join(tag_desc_df %>% filter(Type == 'language') %>% select(Subtag, Description), 
              by = c('lang' = 'Subtag')) %>%
    mutate(pct = num / sum(num) * 100)

num_tweets_per_lang %>% head(20) %>%
    ggplot(aes(x = factor(Description, levels = rev(Description)), y = pct)) +
    geom_bar(stat = 'identity') +
    geom_text(aes(label = round(pct,2)), nudge_y = 5) +
    coord_flip() +
    scale_y_continuous(breaks = c(0,25,50,75,100), labels = str_c(c(0,25,50,75,100), '%')) +
    labs(title = paste('Top 20 tweeted languages (of',nrow(data),'tweets)'), 
         subtitle = 'Tweets containing both "new" and "resolution".', x = 'Language', y = 'Proportion of Tweets')

# Length of tweet

data <- data %>% mutate(Length = str_length(text))

data %>% filter(Length > 960) %>% pull(text)

data %>%
    ggplot(aes(Length)) +
    geom_histogram()

# Top words

# Most meaningful words


                                                            