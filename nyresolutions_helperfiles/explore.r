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
library(tidytext)

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

theme_jill <- theme(#panel.grid.major.x = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold", debug = F),
    axis.text = element_text(face = "bold", colour = 'black'),
    axis.line.x.bottom = element_line(colour = 'black', size = 1),
    axis.line.y.left = element_line(colour = 'black', size = 1))


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

#breaks <- seq(ymd_hms('2020-12-31 12:00:00'), ymd_hms('2021-01-01 11:00:00'), 3600)
breaks <- seq(ymd_hms('2020-12-31 12:00:00'), ymd_hms('2021-01-01 12:00:00'), 3600)
#labels <- tz_import$Name[2:nrow(tz_import)]
labels <- tz_import$Name

tweets_per_hour <- data %>% mutate(d = floor_date(created_at, unit = '1 hour')) %>%
    group_by(d) %>%
    summarise(cnt = n()) %>% 
    filter(d >= ymd_hms('2020-12-31 12:00:00'), 
           d < ymd_hms('2021-01-01 12:00:00'), ) %>%
    arrange(desc(d)) %>% # the later times correspond to most westerly regions # west to east
    mutate(d = as.character(d), x = seq_along(d),
           breaks = seq(ymd_hms('2021-01-01 11:00:00'), ymd_hms('2020-12-31 12:00:00'),
                        -3600), # west to east
           labels = tz_import$Name[2:nrow(tz_import)],
           UTC_Offset = tz_import$UTC_Offset[2:nrow(tz_import)]) # west to east
# SELECT FOR APP
fig1 <- tweets_per_hour %>%
    ggplot(aes(x = x, y = cnt)) +
    geom_line() +
    annotate('text', x = 7, y = tweets_per_hour$cnt[7] + 250, label = tweets_per_hour$labels[7]) +
    annotate('text', x = 12, y = tweets_per_hour$cnt[12] + 250, label = tweets_per_hour$labels[12]) +
    annotate('text', x = 20, y = tweets_per_hour$cnt[20] + 250, label = tweets_per_hour$labels[20]) +
    scale_x_continuous(breaks = seq(2,24,2), labels = tweets_per_hour$UTC_Offset[seq(2,24,2)],
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
fig1

#htmltools::save_html(ggplotly(fig1), file = "tweets_per_hour.html")
htmlwidgets::saveWidget(ggplotly(fig1), 
                        file = file.path(getwd(),'www',"resolutions__tweets_per_hour.html"), 
                        selfcontained = FALSE,
                        libdir = file.path(getwd(),'www'))

#htmlwidgets::saveWidget(ggplotly(fig1), file = "tweets_per_hour.html", selfcontained = TR

# Most used emojis ---------

# add a noteable tag to every emoji and join all words with underscores so I can search for bords beginning
# with jaugur later

tagged_emoji_data <- emoji_data %>% mutate(tagged_desc = str_c('jaugur_',
                                      str_replace_all(description,'\\s+','_')))

data <- readr::read_rds(file.path('nyresolutions','df_with_emojis_replaced.rds'))

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
           #label = link_to_img(url)
           )

offset <- max(top_emojis$tot) / 15 # 5% of the largest value

# SELECT FOR APP
fig2 <- top_emojis %>%
    ggplot(aes(forcats::fct_reorder(emoji, tot, .desc = TRUE), tot, label = label)) +
    geom_col(width = 0.5, fill = 'white', colour = 'black', size = 1) +
    geom_richtext(aes(y = tot + offset), fill = NA, label.color = NA,
                  label.padding = grid::unit(rep(0, 4), "pt")
    ) +
    scale_x_discrete(labels = NULL) +
    scale_y_continuous(breaks = seq(0,6000,1000), limits = c(0,6500), expand = c(0,0)) +
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank()) +
    labs(x = 'Emoji', y = 'Count\n',
         title = 'Most Popular Emojis Used') +
    theme_minimal() +
    theme_jill

# Most used emjois per region ----------
# split into UTC-5, UTC and UTC +8

# Top words -----------

clean_words <- function(char_vec) {
    # removing words that do not start with a alphanum character and if they are longer than 1 character those which do not end in an alnum character
    res <- str_extract(char_vec, '[:alnum:]\\w*[:alnum:]*') # this also removes terms such as "don't"
    res
}

# Most meaningful words: Approach A (counts) ---------
# this conludes contractions e.g. don't
# it excludes stop words from the tidytext::stop_words dataset

#takes 1 min
common_words <- data %>% select(ID, text_wo_replies_links) %>% 
    unnest_tokens(output = word, input = text_wo_replies_links, token = 'tweets',
                  stopwords = stop_words$word,
                  #stopwords = filter(stop_words, lexicon == 'snowball') %>% pull(word)
                  ) %>%
    filter(!str_detect(word, '^#')) %>%
    mutate(cleaned_words = word
    ) %>% 
    group_by(cleaned_words) %>%
    summarise(n = n()) %>%
    arrange(desc(n))

#words_to_exclude <- c('new','year','years','resolution','get','make','im','will')
words_to_exclude <- c('happy','2021','2020','resolution','im','dont','whats','amp','na','resolutions','ko','ive','1','ill','2','youre','sa','3','#newyear','#happynewyear','ng','yall','#happynewyear2021',"\U0001f602",'ako','4','ang','5','wont','didnt','heres','lot','lol')

common_words %>% tibble::rowid_to_column('row') %>%
    filter(cleaned_words %in% c('laugh','love','eat','pray'))

# stemmed words
common_words_stemmed <- common_words %>% filter(!cleaned_words %in% words_to_exclude) %>%
    mutate(stemmed_word = SnowballC::wordStem(cleaned_words, language = 'english')) %>%
    group_by(stemmed_word) %>%
    summarise(n = sum(n)) %>%
    arrange(desc(n))


# bar chart
common_words_stemmed %>%
    slice(1:30) %>%
    ggplot(aes(forcats::fct_reorder(stemmed_word, n, .desc = TRUE), 
               y = n, label = forcats::fct_reorder(stemmed_word, n, .desc = TRUE))) +
    geom_col() +
    labs(title = 'top 30 stemmed words') +
    theme(axis.text.x = element_text(angle = 90))

# text chart
offset <- max(common_words_stemmed$n) * 0.025
# SELECT FOR APP
fig3 <- common_words_stemmed %>%
    slice(1:30) %>%
    ggplot(aes(forcats::fct_reorder(stemmed_word, n, .desc = FALSE), 
               y = n, label = forcats::fct_reorder(stemmed_word, n, .desc = TRUE))) +
    geom_col(width = 0.1, colour = 'black', fill = 'black') +
    geom_text(aes(y = n + offset), angle = 00, hjust = 0) +
    scale_x_discrete(expand = expansion(mult = 0.03)) +
    scale_y_continuous(breaks = seq(0,12500,2500), limits = c(0,12500), expand = c(0,0)
                       ) +
    labs(title = 'top 30 stemmed words') +
    coord_flip() + 
    theme_minimal() +
    #theme_jill +
    theme(
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.line.x.bottom = element_blank(),
          axis.line.y.left = element_blank(),
         panel.border = element_rect(colour = 'black', size = 2, fill = NA)
          )
    

common_words_stemmed %>% #filter(!cleaned_words %in% words_to_exclude) %>%
    slice(1:30) %>%
    ggplot(aes(label = stemmed_word, size = n)) +
    ggwordcloud::geom_text_wordcloud() +
    scale_size_area(max_size = 8) +
    labs(title = 'top 30 stemmed words') +
    theme_minimal()

# following tidytext example

# tidytext example: word frequency per tweet

pattern = "(#\\w+)|(@\\w+)"
tweet_words <- data %>% mutate(text_wo_mentions_hashtags = str_remove_all(text_wo_replies_links, 
                                                                          "(#\\w+)|(@\\w+)")) %>%
    select(ID, text_wo_mentions_hashtags) %>% 
    unnest_tokens(output = word, input = text_wo_mentions_hashtags) %>%
    mutate(#cleaned_words = clean_words(word),
        cleaned_words = word) %>% 
    filter(!is.na(cleaned_words)) %>%
    count(ID, cleaned_words, sort = T) # takes approx 1min

# titdytext example: words_per_tweet
total_words <- tweet_words %>% group_by(ID) %>%
    summarise(total = sum(n))

tweet_words <- left_join(tweet_words, total_words) # note: total column not necessary for tdidf
tweet_words_tf_idf <- tweet_words %>%
    bind_tf_idf(term = cleaned_words, document = ID, n = n)

# note: bind_tf_idf calculates idf for word X as the log of... total_num_of_docs / number_of_times_W_appeared_over_all_docs
# note: doc_totals gives the total number of words per doc
# tf for X is the number_of_times_X_came_up_in_a_doc / total_num_words_in_that_doc
tweet_words_tf_idf %>% arrange(desc(tf_idf))

# this shows an idf of 12.1 when only one of the word existed
low_df_word <- tweet_words_tf_idf %>% arrange(desc(idf)) %>% pull(cleaned_words) %>% .[1]
tweet_words %>% filter(cleaned_words == low_df_word)
# this shows an idf of 0.00962 for the word "new" which was in every document
tweet_words_tf_idf %>% arrange(idf, n)

# THERE ARE 955 DIFFERENT LEVELS OF IDF, 12.1 IS THE HIGHEST LEVELS AND CORRESPONDS TO A DF OF 1
tweet_words_tf_idf %>% group_by(idf) %>% summarise(n()) %>% arrange(desc(idf))





# Most meaningful words: Approach B (TFIDF-based) ---------

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
                                                            