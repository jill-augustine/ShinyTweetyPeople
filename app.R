library(shinydashboard)
library(shiny)

header <- dashboardHeader(title = "Shiny Tweety People", disable = FALSE)


plot_tweets_per_hour_str <- readr::read_file('tweets_per_hour.html')
plot_tweets_per_hour <- plot_tweets_per_hour_str %>% extract_item('body') %>% HTML()

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

subtab1 <- menuSubItem('Tweets Per Hour', tabName = 'per_hour')
subtab2 <- menuSubItem('Sub here', tabName = 'sub')

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Widgets", subtab1, subtab2, icon = icon("th"))
)
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "dashboard",
                h2("Dashboard tab content")
        ),
        
        tabItem(tabName = "per_hour",
                tags$embed(src = 'tweets_per_hour.html', type="text/html", width="500", height="200"),
                br(),
                p('text here')
        ),
        
        tabItem(tabName = "sub",
                h2("sub tab content")
        )
    )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) { }

shinyApp(ui, server)

