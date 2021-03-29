## ui.R ##
library(shinydashboard)
library(shiny)
library(plotly)

# importing variables:
# - desc_nyr_the_data, desc_nyr_tweets_per_hour, desc_nyr_rank_emojis, desc_nyr_rank_hashtags, desc_nyr_emoji_combos
# - nyr_subtab0 thru myr_subtab4
# - nyres_thedata_tabItem, nyres_perhour_tabItem, nyres_top_emojis_tabItem,
# - nyres_top_emoji_combos_tabItem, nyres_top_hashtags_tabItem

source(file.path('nyresolutions','nyr_ui_vars.R'))

# 1) Structure of the Header
# dashboard header
header <- dashboardHeader(title = 'Shiny Tweety People', disable = FALSE)

# 2) Structure of the Sidebar
# 2.1) menuSubItems
# imported above

# 2.2) menuItems, sidebarMenu, and dashboardSidebar
sidebar <- dashboardSidebar(
    sidebarMenu(
        #menuItem("Placeholder", tabName = "placeholder",icon = icon("home")),
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("New Year's Resolutions", 
                 nyr_subtab0, nyr_subtab1, nyr_subtab2, nyr_subtab4, nyr_subtab3,
                 icon = icon("star-half-alt"), startExpanded = TRUE),
        menuItem("Visit jill.codes", href = "https://www.jill.codes", newtab = TRUE)
    )
)

# 3) Structure of the Body
# 3.1) individual 'tabItem' units

social_links <- {'
  <a href="https://github.com/jill-augustine" class="social-links__entry" target="_blank">
  <i class="fa fa-github"></i>
  </a>\t
  <a href="https://in.linkedin.com/in/jillianaugustine" class="social-links__entry" target="_blank">
  <i class="fa fa-linkedin"></i>
  </a>\t
  <a href="https://twitter.com/jill_codes" class="social-links__entry" target="_blank">
  <i class="fa fa-twitter"></i>
  </a>
'}

home_tabItem <- tabItem(tabName = "home", h1('Shiny Tweety People', class='title'),#36
                        p(class = 'subtitle', 'by Jill Augustine', br(), HTML(social_links)),
                        br(),
                        p("Welcome to Shiny Tweety People, a Shiny app analysis of people's tweets."),
                        p("I mainly use Python in my day-to-day work so I use this app to play around with Shiny and keep my R skills up-to-date. This is a private project and is in no way connected to my professional work."),
                        p("On the left you can check out the different ways I have analysed tweets about New Years Resolutions. More content on Valentine's Day tweets to come."),
                        p("For more content visit", a('jill.codes', href = 'https://www.jill.codes')) )


# 3.2) tabItems, and dashboardBody
body <- dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css2?family=Catamaran")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    tabItems(
             home_tabItem,
             nyres_thedata_tabItem,
             nyres_perhour_tabItem,
             nyres_top_emojis_tabItem,
             nyres_top_emoji_combos_tabItem,
             nyres_top_hashtags_tabItem
    )
)

# 4) Structure of the Page
dashboardPage(header, sidebar, body, skin = 'purple')