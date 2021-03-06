# 0) Importing libraries

#library(jsonlite)
library(dplyr)
library(stringr)
library(purrr)
#library(tictoc)
#library(ggplot2)
#library(tidyjson)
library(plotly)
#library(textclean)
#library(rvest)
#library(ggtext)
#library(tidytext)

# importing the following:
# - nyr_tweets_per_hour, fig_nyr_tweets_per_hour, nyr_emoji_freq_table, nyr_hashtag_freq_table,
# - plot_emoji_chart, plot_hashtag_chart
# - pairs data sets and associated sankey charts
source(file.path('nyresolutions','create_figures.R'))

function(input, output) { 
    # valentine's day ---------------------------
    
    # new years resolutions ---------------------------
    # -----
    output$nyr_tweetsperhour <- renderPlotly(expr = fig_nyr_tweets_per_hour)
    # -----
    nyr_top_emojis <- reactive({
        req(input$nyr_emoji_range)
        df <- nyr_emoji_freq_table %>% 
              mutate(rank = seq_along(emoji)) %>%
              .[input$nyr_emoji_range[1]:input$nyr_emoji_range[2],]
        plot_emoji_chart(df)
        })
    output$nyr_top_emojis <- renderPlotly(nyr_top_emojis())
    output$DT_nyr_emojis <- DT::renderDT({
        nyr_emoji_freq_table %>%
            select(-url) %>%
            rename(Description = value, `Times Used` = tot, Emoji = emoji) %>%
            relocate(Emoji)
        })
    # -----
    output$nyr_sorted_emoji_pairs_sankey <- renderPlotly(expr = nyr_sorted_emoji_pairs_sankey)
    #output$DT_nyr_sorted_emoji_pairs <- DT::renderDT(nyr_sorted_emoji_pairs)
    # -----
    output$nyr_sorted_emoji_pairs_all_sankey <- renderPlotly(expr = nyr_sorted_emoji_pairs_all_sankey)
    #output$DT_nyr_sorted_emoji_pairs_all <- DT::renderDT(nyr_sorted_emoji_pairs_all)

    # -----
    #output$nyr_twin_emoji_pairs_sankey <- renderPlotly(expr = nyr_twin_emoji_pairs_sankey)
    # -----
    nyr_top_hashtags <- eventReactive(input$refresh_nyr_hashtag_plot, {
        req(input$nyr_hashtag_range)
        df <- nyr_hashtag_freq_table %>% 
            filter(tot >= 10) %>%
            mutate(rank = seq_along(value)) %>%
            .[input$nyr_hashtag_range[1]:input$nyr_hashtag_range[2],]
        # don't plot the xaxis labels if there are more than 30 to plot
        if ((input$nyr_hashtag_range[2] - input$nyr_hashtag_range[1]) <= 30) {
            plot_hashtag_chart(df) %>% 
                layout(xaxis = list(range = c(input$nyr_hashtag_range[1] - 0.5,
                                              input$nyr_hashtag_range[2] + 0.5)))
        } else { 
            plot_hashtag_chart(df) %>% 
                layout(xaxis = list(showticklabels = FALSE,
                                    range = c(input$nyr_hashtag_range[1] - 0.5,
                                              input$nyr_hashtag_range[2] + 0.5)))
            }
    })
    output$nyr_top_hashtags <- renderPlotly(nyr_top_hashtags())
    output$DT_nyr_hashtags <- DT::renderDT({
        nyr_hashtag_freq_table %>%
            rename(Hashtag = value, `Times Used` = tot)
    })
        
}