## ui.R ##
library(shinydashboard)
library(shiny)
library(plotly)

# importing home_text,desc_nyr_the_data, desc_nyr_tweets_per_hour, desc_nyr_rank_emojis, desc_nyr_rank_hashtags
source(file.path('nyresolutions','nyr_text_descriptions.R'))

# 1) Structure of the Header
# dashboard header
header <- dashboardHeader(title = 'Shiny Tweety People', disable = FALSE)

# 2) Structure of the Sidebar
# 2.1) menuSubItems
nyr_subtab0 <- menuSubItem('The Data', tabName = 'resolutions__intro')
nyr_subtab1 <- menuSubItem('Tweets Per Hour', tabName = 'resolutions__per_hour')
nyr_subtab2 <- menuSubItem('Top Emojis', tabName = 'resolutions__top_emojis') 
nyr_subtab3 <- menuSubItem('Top Hashtags', tabName = 'resolutions__top_hashtags') 
# 2.2) menuItems, sidebarMenu, and dashboardSidebar
sidebar <- dashboardSidebar(
    sidebarMenu(
        #menuItem("Placeholder", tabName = "placeholder",icon = icon("home")),
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("New Year's Resolutions", 
                 nyr_subtab0, nyr_subtab1, nyr_subtab2, nyr_subtab3,
                 icon = icon("star-half-alt"), startExpanded = TRUE),
        menuItem("Visit jill.codes", href = "https://www.jill.codes", newtab = TRUE)
    )
)

# 3) Structure of the Body
# 3.1) individual 'tabItem' units
placeholder_tabItem <- tabItem(tabName = "placeholder",
                    p('text here')
)
home_tabItem <- tabItem(tabName = "home", h1('Shiny Tweety People', class='title'),#36
                        p(class = 'subtitle', 'by Jill Augustine', br(), HTML(social_links)),
                        br(),
                        p("Welcome to Shiny Tweety People, a Shiny app analysis of people's tweets."),
                        p("I mainly use Python in my day-to-day work so I use this app to play around with Shiny and keep my R skills up-to-date. This is a private project and is in no way connected to my professional work."),
                        p("On the left you can check out the different ways I have analysed tweets about New Years Resolutions. More content on Valentine's Day tweets to come."),
                        p("For more content visit", a('jill.codes', href = 'https://www.jill.codes')) )

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

# 3.2) tabItems, and dashboardBody
body <- dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css2?family=Catamaran")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    #tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "font-awesome/css/font-awesome.css")),
    tabItems(#placeholder_tabItem, 
             home_tabItem,
             nyres_thedata_tabItem,
             nyres_perhour_tabItem,
             nyres_top_emojis_tabItem,
             nyres_top_hashtags_tabItem#,
             #sub_tabItem
    )
)

# 4) Structure of the Page
dashboardPage(header, sidebar, body, skin = 'purple')