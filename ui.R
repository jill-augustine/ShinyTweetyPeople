## ui.R ##
library(shinydashboard)
library(shiny)
library(plotly)

# 1) Structure of the Header
# dashboard header
header <- dashboardHeader(title = "Shiny Tweety People", disable = FALSE)

extract_item <- function(text, item = NULL, from = NULL, to = NULL) {
    require(stringr)
    # finds the first instance of the item's start and end tag only
    # returns a character if input is a character, otherwise returns a list when given a character vector
    if (is.null(from) & is.null(to) & !is.null(item)) {
        from <- paste0('<',item,'>')
        to = paste0('</',item,'>')
    }
    from_idx <- str_locate(text, from)[,'start']
    to_idx <- str_locate(text, to)[,'end']
    res <- str_sub(text, start = from_idx, end = to_idx)
    if (length(res) == 1) {
        return(res[[1]])  
    } else {
        return(res)
    }
}
#plot_tweets_per_hour %>% extract_item('body') %>% HTML()

# 2) Structure of the Sidebar
# 2.1) menuSubItems
subtab1 <- menuSubItem('Tweets Per Hour', tabName = 'resolutions__per_hour')
subtab2 <- menuSubItem('Sub here', tabName = 'sub')
# 2.2) menuItems, sidebarMenu, and dashboardSidebar
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Placeholder", tabName = "placeholder",icon = icon("home")),
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("Resolutions", subtab1, subtab2, icon = icon("star-half-alt"))
    )
)

# 3) Structure of the Body
# 3.1) individual 'tabItem' units
placeholder_tabItem <- tabItem(tabName = "placeholder",
                               plotlyOutput(outputId = 'nyr_tweetsperhour'),
                               p('Placeholder text')
)
home_tabItem <- tabItem(tabName = "home",
                        p('This is an intro statement'),
                        strong("New Year's Resolutions"), 
                        br(),
                        p("Text description here"),
                        h2("For more content visit", a('www.jill.codes', href = 'https://www.jill.codes')), 
)

nyres_perhour_tabItem <- tabItem(tabName = "resolutions__per_hour",
                                 tags$embed(src = 'resolutions__tweets_per_hour.html', type="text/html", width="500", height="200"),
                                 br(),
                                 p('text here')
)
sub_tabItem <- tabItem(tabName = "sub",
                       h2("sub tab content")
)
# 3.2) tabItems, and dashboardBody
body <- dashboardBody(
    tabItems(placeholder_tabItem, 
             home_tabItem,
             nyres_perhour_tabItem, 
             sub_tabItem
    )
)

# 4) Structure of the Page
dashboardPage(header, sidebar, body)