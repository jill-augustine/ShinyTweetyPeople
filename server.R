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

function(input, output) { 
    nyr_tweetsperhour <- reactive({
        breaks <- seq(ymd_hms('2020-12-31 12:00:00'), ymd_hms('2021-01-01 11:00:00'), 3600)
        labels <- tz_import$Name[2:nrow(tz_import)]
        
        tweets_per_hour <- data %>% mutate(d = round_date(created_at, unit = '1 hour')) %>%
            group_by(d) %>%
            summarise(cnt = n()) %>% 
            filter(d >= ymd_hms('2020-12-31 12:00:00'), 
                   d <= ymd_hms('2021-01-01 11:00:00')) %>% # cut off is 11AM on 1/1 because anything that rounded to 12PM on 1/1 (GMT) is newzealand timezone and is one day ahead (celebrated at 12PM on 31/12 (GMT) )
            arrange(desc(d)) %>%
            mutate(d = as.character(d), x = seq_along(d))
        # SELECT FOR APP
        ggfig <- tweets_per_hour %>%
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
    })
    output$nyr_tweetsperhour <- renderPlotly(expr = nyr_tweetsperhour())
    
        
}