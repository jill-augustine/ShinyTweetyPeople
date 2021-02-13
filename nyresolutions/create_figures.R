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
nyr_tweets_per_hour <- nyresolution_data %>% mutate(hr = round_date(created_at, unit = '1 hour')) %>%
    group_by(hr) %>%
    summarise(cnt = n()) %>% 
    filter(hr >= ymd_hms('2020-12-31 12:00:00'), 
           hr < ymd_hms('2021-01-01 12:00:00'), ) %>%
    arrange(desc(hr)) %>% # the later times correspond to most westerly regions # west to east
    mutate(hr = as.character(hr), x = seq_along(hr),
           breaks = seq(ymd_hms('2021-01-01 11:00:00'), ymd_hms('2020-12-31 12:00:00'),
                        -3600), # west to east
           labels = tz_import$Name[2:nrow(tz_import)],
           GMT_Offset = tz_import$GMT_Offset[2:nrow(tz_import)]) # west to east

# prepping for figure
ax <- list(
    zeroline = FALSE,
    showline = TRUE,
    #mirror = "ticks",
    showgrid = FALSE,
    linecolor = toRGB('grey30'),
    linewidth = 4
)
xticktext <- paste0(nyr_tweets_per_hour$GMT_Offset, '</b>')
annotationtext <- paste0('<b>',nyr_tweets_per_hour$labels, '</b>')

# creating figure
fig_nyr_tweets_per_hour <- nyr_tweets_per_hour %>% 
    plot_ly() %>%
    add_trace(x = ~x, y = ~cnt, type = 'scatter', mode = 'markers+lines', 
              customdata = ~labels,
              text = ~GMT_Offset,
              hovertemplate = '<b>%{customdata}</b> | %{text}<br>%{y} Tweets<extra></extra>',
              line = list(width = 4),
              marker = list(size = 8)
              ) %>%
    add_annotations(x = c(7, 12, 19),
                    y = ~cnt[c(7, 12, 19)] + 150,
                    text = annotationtext[c(7, 12, 19)],
                    xref = "x",
                    yref = "y", 
                    showarrow = FALSE,
                    font = list(color = 'grey30')
                    ) %>%
    # frame layout
    layout(xaxis = ax, yaxis = ax) %>%
    # title layout
    layout(title = list(text = '<b>Peaks in tweets per hour as regions passed midnight</b><br>',
                        font = list(size = 20, color = 'grey30')
                        )
           ) %>%
    # x and y axis layout
    layout(xaxis = list(title = list(text = '<b>Time Region</b>', 
                                     standoff = 10, 
                                     font = list(color = 'grey30')
                                     ), 
                        tickvals = seq(2,24, 2),
                        ticktext = xticktext[seq(2,24, 2)],
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
)
    
fig_nyr_tweets_per_hour

# 3.2) top emojis  ----
# prepping data

emoji_freq_table <- nyresolution_data$text_with_emojis_replaced %>%
    str_extract_all('jaugur_\\w+') %>% # this also removes the ":_medium_skin_tone" part of the desc
    unlist() %>% str_remove_all('jaugur_') %>% str_replace_all('_',' ') %>%
    as_tibble() %>% group_by(value) %>%
    summarise(tot = n()) %>% # creating a frequency table
    arrange(desc(tot)) %>% 
    # adding the emoji itself
    left_join(select(emoji_data, emoji, description, url), by = c('value' = 'description'))  %>%
    distinct(value, .keep_all = T) # removing duplicates due to left join where two emojis have the same desc

# prepping for figure
ax <- list(
    zeroline = FALSE,
    showline = TRUE,
    #mirror = "ticks",
    showgrid = FALSE,
    linecolor = toRGB('grey30'),
    linewidth = 4
)
xticktext <- NULL
annotationtext <- NULL

# creating figure (this should actually be done within server or with shiny functions to allow for reactivity)

emoji_from <- 1
emoji_to <- 3
emoji_freq_subset <- emoji_freq_table %>% .[emoji_from:emoji_to,] %>%
    mutate(value = factor(value, levels = value),
           x = seq_along(value))

emojis_to_plot <- vector(mode = 'list', length = nrow(emoji_freq_subset))

for (i in seq_len(nrow(emoji_freq_subset))) {
    emojis_to_plot[[i]] <- list(source = emoji_freq_subset$url[i],
                                xref = "paper",
                                yref = "paper",
                                x= 0,
                                y= 1,
                                #sizex = 0.2,
                                #sizey = 0.2#,
                                #xref = "x",
                                #yref = "y",
                                #x= emoji_freq_subset$x[i],
                                #y= emoji_freq_subset$tot[i] + 1000#,
                                #sizex = 0.2,
                                #sizey = 0.2#,
                                #opacity = 0.8
    )
}

test_layout1 <- list(#source = file.path('emoji13_1',str_split(emoji_freq_subset$url[1], '/') %>% unlist() %>% last()),
    #source =  "https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/120/apple/271/face-with-tears-of-joy_1f602.png",
    source = "https://images.plot.ly/language-icons/api-home/r-logo.png",
    xref = "x",
    yref = "y",
    x = emoji_freq_subset$x[1],
    xanchor = 'center',
    y= emoji_freq_subset$tot[1],
    yanchor = "bottom",
    sizex = 0.2,
    sizey = emoji_freq_subset$tot[i],
    opacity = 0.8)

test_layout1_1 <- list(#source = "emoji13_1/face-with-tears-of-joy_1f602.png",
    #source = "https://images.plot.ly/language-icons/api-home/python-logo.png",
    source = "https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/120/apple/271/grinning-face_1f600.png",
                     xref = "x",
                     yref = "paper",
                     x= 1,
                     y= 1,
                     sizex = 0.2,
                     sizey = 0.2,
                     opacity = 0.8
)
test_layout2 <- list(source = "https://images.plot.ly/language-icons/api-home/python-logo.png",
                     xref = "paper",
                     yref = "paper",
                     x= 0,
                     y= 1,
                     sizex = 0.2,
                     sizey = 0.2,
                     opacity = 0.8
)

emoji_freq_subset %>%
    plot_ly() %>%
    add_trace(x = ~x, y = ~tot, type = "bar", visible = TRUE) %>%
    #layout(images = emojis_to_plot)
    #layout(images = list(test_layout1_1)) #%>%
    #layout(images = test_layout2) %>%
    # frame layout
    layout(xaxis = ax, yaxis = ax) %>%
    # title layout
    layout(title = list(text = '<b>Peaks in tweets per hour as regions passed midnight</b><br>',
                        font = list(size = 20, color = 'grey30')
    )
    ) %>%
    # x and y axis layout
    layout(xaxis = list(title = list(text = '<b>Time Region</b>', 
                                     standoff = 10, 
                                     font = list(color = 'grey30')
    ), 
    tickvals = ~x  ,
    ticktext = ~value,
    tickfont = list(color = 'grey30'),
    #range = c(0,26),
    showgrid = TRUE
    ),
    yaxis = list(title = list(text = '<b>Tweets per Hour</b>', 
                              standoff = 10,
                              font = list(color = 'grey30')
    ),
    tickfont = list(color = 'grey30'),
    range = c(0,max(emoji_freq_subset$tot) +1000)
    )
    )



