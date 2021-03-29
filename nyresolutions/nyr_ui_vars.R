library(shiny)
library(shinydashboard)

# importing home_text,desc_nyr_the_data, desc_nyr_tweets_per_hour, desc_nyr_rank_emojis, desc_nyr_rank_hashtags
source(file.path('nyresolutions','nyr_text_descriptions.R'))

# 2.1) menuSubItems
nyr_subtab0 <- menuSubItem('The Data', tabName = 'resolutions__intro')
nyr_subtab1 <- menuSubItem('Tweets Per Hour', tabName = 'resolutions__per_hour')
nyr_subtab2 <- menuSubItem('Top Emojis', tabName = 'resolutions__top_emojis')
nyr_subtab4 <- menuSubItem('Top Emoji Combos', tabName = 'resolutions__top_emoji_combos') 
nyr_subtab3 <- menuSubItem('Top Hashtags', tabName = 'resolutions__top_hashtags') 

# 3.1) individual 'tabItem' units

nyres_thedata_tabItem <- tabItem(tabName = 'resolutions__intro',
                                 h1(strong('The Datasets')),
                                 HTML(desc_nyr_the_data)
                                 )

nyres_perhour_tabItem <- tabItem(tabName = "resolutions__per_hour",
                                 h1(strong('Peaks in tweets per hour as regions passed midnight')),
                                 HTML(desc_nyr_tweets_per_hour), br(), 
                                 plotlyOutput(outputId = 'nyr_tweetsperhour') %>%
                                     shinycssloaders::withSpinner(type = 7, color = "999999"),
)
nyres_top_emojis_tabItem <- tabItem(tabName = "resolutions__top_emojis",
                                    h1(strong('Top Emojis')),
                                    HTML(desc_nyr_rank_emojis),
                                    tags$hr(),
                                    sliderInput('nyr_emoji_range', 
                                                label = 'Choose the top n emojis to plot',
                                                min = 1, max = 1150,
                                                value = c(0, 10),
                                                width = '100%'),
                                    plotlyOutput(outputId = 'nyr_top_emojis') %>%
                                    shinycssloaders::withSpinner(type = 7, color = "999999"),
                                    tags$hr(),
                                    h2('DataTable'), br(),
                                    DT::DTOutput(outputId = 'DT_nyr_emojis')
                                )

nyres_top_emoji_combos_tabItem <- tabItem(tabName = "resolutions__top_emoji_combos",
                                    h1(strong('Top Emoji Combinations')),
                                    HTML(desc_nyr_emoji_combos[[1]]),
                                    HTML(desc_nyr_emoji_combos[[2]]),
                                    h2('Frequency of non-identical emoji pairs'),
                                    plotlyOutput(outputId = 'nyr_sorted_emoji_pairs_sankey') %>%
                                        shinycssloaders::withSpinner(type = 7, color = "999999"),
                                    #tags$hr(),
                                    h2('Frequency of identical and non-identical emoji pairs'),
                                    HTML(desc_nyr_emoji_combos[[3]]),
                                    plotlyOutput(outputId = 'nyr_sorted_emoji_pairs_all_sankey') %>%
                                        shinycssloaders::withSpinner(type = 7, color = "999999")
                                    #tags$hr(),
                                    #plotlyOutput(outputId = 'nyr_twin_emoji_pairs_sankey') %>%
                                    #    shinycssloaders::withSpinner(type = 7, color = "999999"),
)

nyres_top_hashtags_tabItem <- tabItem(tabName = "resolutions__top_hashtags",
                                    h1(strong('Top Hashtags')),
                                    HTML(desc_nyr_rank_hashtags),
                                    tags$hr(),
                                    sliderInput('nyr_hashtag_range', 
                                                label = paste0('Choose the top n hashtags to plot', 
                                                               ' and click "Update Plot"'),
                                                min = 1, max = 731, value = c(0, 20),
                                                width = '100%'),
                                    actionButton(inputId = 'refresh_nyr_hashtag_plot',
                                                 label = "Update Plot"),
                                    br(),br(),
                                    plotlyOutput(outputId = 'nyr_top_hashtags') %>%
                                        shinycssloaders::withSpinner(type = 7, color = "999999"),
                                    tags$hr(),
                                    h2('Datatable'), br(),
                                    DT::DTOutput(outputId = 'DT_nyr_hashtags')
)