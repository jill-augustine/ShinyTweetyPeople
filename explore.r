# requires 'data' and 'lang_info_df' as created in clean.r
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

# Intercaitve: To do

# Length of tweet ------------
# this is not interesting
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

f
data$text_count %>% cut_width(10, boundary = 0) %>% levels()

x <- data %>%
    ggplot(aes(text_count)) +
    geom_histogram(binwidth = 5, fill = 'white', colour = 'black') 
x %>%
    ggplotly()

data %>%
    ggplot(aes(text_count)) +
    geom_freqpoly(binwidth = 10)

dens <- data %>%
    ggplot(aes(text_count)) +
    geom_density()

ggplotly(dens)
data %>% group_by(text_count) %>%
    summarise(n = n()) %>%
    mutate(prop = n / sum(n) * 100) %>% 
    ggplot(aes(x = text_count, y = prop)) +
    geom_bar(stat = 'identity')
    
x <- data %>% group_by(text_count) %>%
    summarise(n = n()) %>%
    mutate(prop = n / sum(n) * 100,
           bin = cut(text_count, seq(0,280,20), include.lowest = TRUE))%>% 
    group_by(bin) %>% summarise(prop = sum(prop))

x %>% ggplot(aes(x = bin, y = prop)) +
    geom_bar(stat = 'identity')
    

# Top words

# Most meaningful words

# Number of emojis used

# favourite emojis used


                                                            