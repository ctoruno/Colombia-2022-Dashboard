## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Political Observatory Colombia: Shiny App
##                    APP FILE
##
## Author:            Carlos A. Toruño Paniagua   (carlos.toruno@gmail.com)
##
## Creation date:     December 25th, 2021
##
## This version:      January 26th, 2022
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Required packages
lapply(list("rtweet", "haven", "qdap", "tm", "topicmodels", "syuzhet", "SnowballC", "wesanderson",
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
                            status = "warning", 
                            solidHeader = F,
                            tagsFilter_UI("speech_tags"),
                            candidates_input(id = "speech_candidate"),
                            submit_input(id = "speech_filter")),
        shinydashboard::box(title = "Overview",
                            width = 9,
                            height = 450,
                            status = "warning", 
                            solidHeader = F
        )
      ),
      fluidRow(
        valueBoxOutput("user_since", width = 4),
        valueBoxOutput("speech_tweets_count", width = 4),
        valueBoxOutput("followers_tun", width = 4)
      ),
      fluidRow(
        shinydashboard::box(title = "Tweets timeline",
                            width = 12,
                            height = 500,
                            status = "warning", 
                            solidHeader = F,
                            plotlyOutput("timeline_plot")
        )
      ),
      fluidRow(
          box(title = "Most used terms",
              width = 5,
              height = 600,
              status = "warning", 
              solidHeader = F,
              accordion(
                id = "acc",
                accordionItem(
                  title = "Selected candidate",
                  collapsed = F,
                  width = 4,
                  DTOutput("speech_words_main", width = "100%", height = 450)
                ),
                accordionItem(
                  title = "Comparison candidates",
                  collapsed = T,
                  DTOutput("speech_words_com", height = 450)
                ),
                accordionItem(
                  title = "Top used hashtags: candidate",
                  collapsed = T,
                  DTOutput("speech_toph", height = 450)
                )
              )
          ),
        box(title = "Wordcloud",
            width = 7,
            height = 600,
            status = "warning",
            solidHeader = F, 
            collapsible = T,
            collapsed = F,
            wordcloud2Output("speech_wordcloud", 
                             width = "100%", 
                             height = "600px")
        )
      ),
      fluidRow(
        box(
          title = "Topic Modelling",
          width = 12,
          height = 500,
          status = "warning", 
          solidHeader = F, 
          collapsible = T,
          collapsed = F,
          sidebar = boxSidebar(
            id = "tmodels_sidebar",
            width = 25,
            tmodelling_UI("tmodels"),
            verbatimTextOutput("tmodels_fvalues")
          ),
          DTOutput("tmodels_words")
        )
      ),
      fluidRow(h4(strong("Most popular tweets")),
               tags$head(tags$script(async = NA, src = "https://platform.twitter.com/widgets.js")),
               column(width = 4,
                      height = 500,
                      uiOutput("speech_pop1_widget")),
               column(width = 4,
                      height = 500,
                      uiOutput("speech_pop2_widget")),
               column(width = 4,
                      height = 500,
                      uiOutput("speech_pop3_widget"))
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
  
  # Applying a module that updates the comparison candidates inputs according to the main candidate input
  candidates_server(id = "speech_candidate", glob = glob)
  
  # Filtering data using the data_filtering module
  data2analyze <- filtering_server(id = "speech_filter", data = "speech", glob = glob)
  
  # Applying timeline module
  timeline4speech <- timeline_server("speech_timeline",
                                     filtered_data = data2analyze,
                                     panel = "speech",
                                     glob = glob)
  
  # Assigning timeline to outputs
  output$timeline_plot <- renderPlotly(timeline4speech())
  
  # Applying the freq analysis module
  freqoutputs4speech <- reactive({
    frequency_server(id = "speech_tags", 
                     filtered_data = data2analyze, 
                     glob = glob)
    })
  
  # Assigning the freq analysis results to outputs
  output$speech_wordcloud  <- renderWordcloud2(freqoutputs4speech()[[1]])
  output$speech_words_main <- renderDT(freqoutputs4speech()[[2]])
  output$speech_words_com  <- renderDT(freqoutputs4speech()[[3]])
  output$speech_toph       <- renderDT(freqoutputs4speech()[[4]])
  
  # Applying the value boxes module
  vboxes_values <- valueBox_SERVER(id = "speech_vboxes", glob = glob)
  
  # Assigning the resulting value boxes to outputs
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
  
  # Generating tweet widgets using the widgets module
  popular_urls <- popular_tweets_UI("speech_widgets",
                                    panel = "speech",
                                    filtered_data = data2analyze,
                                    glob = glob)
  
  # Assigning resulting widgets to outputs
  output$speech_pop1_widget <- renderUI({
    tagList(
      tags$blockquote(class = "twitter-tweet",
                      tags$a(href = popular_urls()[[1]])),
      tags$script('twttr.widgets.load(document.getElementById("tweet"));'),
    )
  })

  output$speech_pop2_widget <- renderUI({
    tagList(
      tags$blockquote(class = "twitter-tweet",
                      tags$a(href = popular_urls()[[2]])),
      tags$script('twttr.widgets.load(document.getElementById("tweet"));')
    )
  })

  output$speech_pop3_widget <- renderUI({
    tagList(
      tags$blockquote(class = "twitter-tweet",
                      tags$a(href = popular_urls()[[3]])),
      tags$script('twttr.widgets.load(document.getElementById("tweet"));')
    )
  })

  # Applying topic modelling module
  tokenized_data <- reactive(freqoutputs4speech()[[5]])
  tmodelling <- tmodelling_SERVER("tmodels",
                                  tokenized_data = tokenized_data,
                                  glob = glob)
  
  # Assigning resulting topics to outputs
  output$tmodels_words <- renderDT({
    datatable(tmodelling()[["Top Words"]])
  })

  output$tmodels_fvalues <- renderPrint({
    tmodelling()[["Fit Values"]]
  })

}

shiny::shinyApp(appUI, appSERVER)
