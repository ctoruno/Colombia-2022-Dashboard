## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Political Observatory Colombia: Shiny App
##                    APP FILE
##
## Author:            Carlos A. Toruño Paniagua   (carlos.toruno@gmail.com)
##
## Creation date:     December 25th, 2021
##
## This version:      December 31st, 2021
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Required packages
lapply(list("rtweet", "haven", "qdap", "tm", "syuzhet", "SnowballC", "wesanderson",
            "shiny", "shinydashboard", "shinydashboardPlus", "shinythemes", "shinyWidgets", "waiter",
            "plotly", "wordcloud2", "DT", "tidytext", "tidyverse", "magrittr"),
       library, character.only = T)

# Loading Twitter data
load("twitter_data4dash.RData")


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                1.  App UI                                                                                ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## 1.1 Dashboard header ========================================================================================
header <- dashboardHeader(
  title = "Colombia 2022: A Twitter companion",
  controlbarIcon = shiny::icon("cogs"),
  dropdownMenu(type = "notifications",
               icon = icon("exclamation-triangle"),
               notificationItem(
                 text = paste0("Last successful Twitter extraction: ", batches.df$Date[1]),
                 icon("calendar-alt"))
  )
)

## 1.2 Dashboard sidebar =======================================================================================
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Overview", 
             tabName = "overview", icon = icon("dharmachakra")),
    menuItem("Speech Analysis",
             tabName = "speech", icon = icon("comment-dots")),
    menuItem("Social monitoring",
             tabName = "social", icon = icon("chart-line")),
    menuItem("Sentiment trends",
             tabName = "sentiment", icon = icon("grin")),
    menuItem("About",
             tabName = "about", icon = icon("chess"))
  )
)

## 1.3 Dashboard body ==========================================================================================

body <- dashboardBody(
  tabItems(
    
    # Speech panel
    tabItem(
      tabName = "speech",
      fluidRow(
        shinydashboard::box(title = "Filters",
                            width = 3,
                            height = 450,
                            tagsFilter_UI("speech_tags"),
                            candidates_input(id = "speech_candidate"),
                            submit_input(id = "speech_filter")),
        shinydashboard::box(title = "Overview",
                            width = 9,
                            height = 450
        )
      ),
      fluidRow(
        column(
          width = 9,
          shinydashboard::box(title = "Tweets timeline",
                              width = NULL,
                              height = 350
          ) 
        ),
        column(
          width = 3,
          # valueBox_UI("speech_vboxes"),
          valueBoxOutput("user_since", width = NULL),
          valueBoxOutput("speech_tweets_count", width = NULL),
          valueBoxOutput("followers_tun", width = NULL)
        )
      ),
      fluidRow(
        column(
          width = 4,
          box(title = "Most used terms: candidate",
              width = NULL,
              height = 400,
              solidHeader = F, 
              collapsible = T,
              collapsed = T,
              DTOutput("speech_words_main", height = 30)
          ),
          box(title = "Most used terms: comparison candidates",
              width = NULL,
              height = 400,
              solidHeader = F, 
              collapsible = T,
              collapsed = T,
              DTOutput("speech_words_com", height = 30)
          )
        ),
        box(title = "Wordcloud",
            width = 8,
            height = 700,
            solidHeader = F, 
            collapsible = T,
            collapsed = T,
            wordcloud2Output("speech_wordcloud", height = "600px")
        )
      ),
      fluidRow(
        box(title = "Topic Modelling",
            width = 8,
            height = 400,
            solidHeader = F, 
            collapsible = T,
            collapsed = T
        ),
        box(title = "Top used hashtags: candidate",
            width = 4,
            height = 450,
            solidHeader = F, 
            collapsible = T,
            collapsed = T,
            DTOutput("speech_toph")
        )
      )
    )
    
    # Social panel
    
    
  )
)

## 1.4 Back front ==============================================================================================

appUI <- dashboardPage(header, sidebar, body,
                       controlbar = dashboardControlbar(),
                       footer = dashboardFooter(),
                       skin = "green-light")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                2.  App SERVER                                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

appSERVER <- function(input, output, session) {
  
  glob <- reactiveValues()

## 2.1 Speech Panel  ===========================================================================================
  
  # Filtering data modules
  candidates_server(id = "speech_candidate", glob = glob)
  data2analyze <- filtering_server(id = "speech_filter", data = "speech", glob = glob)
  
  # Applying freq analysis module
  freqoutputs4speech <- reactive({
    frequency_server(id = "speech_tags", filtered_data = data2analyze, glob = glob)
    })
  
  output$speech_wordcloud  <- renderWordcloud2(freqoutputs4speech()[[1]])
  output$speech_words_main <- renderDT(freqoutputs4speech()[[2]])
  output$speech_words_com  <- renderDT(freqoutputs4speech()[[3]])
  output$speech_toph       <- renderDT(freqoutputs4speech()[[4]])
  
  # Value boxes module
  vboxes_values <- valueBox_SERVER(id = "speech_vboxes", glob = glob)
    
  output$user_since <- renderValueBox({
    valueBox(
      subtitle = "Using Twitter since:",
      value = vboxes_values()[1],
      icon = icon("twitter"),
      color = "olive"
    )
  })
  output$speech_tweets_count <- renderValueBox({
    valueBox(
      subtitle = "Tweets send since June-2021:",
      value = vboxes_values()[2],
      icon = icon("pen-fancy"),
      color = "yellow"
    )
  })
  output$followers_tun <- renderValueBox({
    valueBox(
      subtitle = "Followers",
      value = vboxes_values()[3],
      icon = icon("users"),
      color = "navy"
    )
  })
  
  
}

shiny::shinyApp(appUI, appSERVER)
