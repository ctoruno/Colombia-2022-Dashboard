## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Political Observatory Colombia: Shiny App
##                    APP FILE
##
## Author:            Carlos A. Toruño Paniagua   (carlos.toruno@gmail.com)
##
## Creation date:     December 25th, 2021
##
## This version:      January 27th, 2022
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Required packages and data loading
source("R/global.R")


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                1.  App UI                                                                                ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## 1.1 Dashboard header ========================================================================================
header <- dashboardHeader(
  title = tagList(
    span(class = "logo-lg", "Colombia 2022"), 
    img(class = "logo-mini", src = "https://img.icons8.com/color/48/000000/colombia-circular.png")
    ),
  fixed = T,
  controlbarIcon = shiny::icon("cogs"),
  dropdownMenu(type = "notifications",
               icon = icon("exclamation-triangle"),
               notificationItem(
                 text = paste0("Last successful Twitter extraction: ", "lol"),
                 icon("calendar-alt"))
  )
)

## 1.2 Dashboard sidebar =======================================================================================
sidebar <- dashboardSidebar(
  collapsed = T,
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
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "CSS_styling.css")
  ),
  tabItems(
    
    # Overview panel
    tabItem(
      tabName = "overview",
      overview_UI("overview_panel")
    ),

    # Speech panel
    tabItem(
      tabName = "speech",
      fluidRow(
        box(title = "Filters",
            width = 3,
            height = 450,
            status = "warning",
            solidHeader = F,
            candidates_input(id = "speech_candidate"),
            actionBttn(
              inputId = "submit_speech",
              label   = "Submit",
              style   = "unite", 
              color   = "success",
              size    =  "sm")
        ),
        box(title = "Overview",
            width = 9,
            height = 450,
            status = "warning",
            solidHeader = F,
            verbatimTextOutput("goul")
        )
      ),
      fluidRow(
        valueBoxes_UI(id = "speech_vboxes")
      ),
      fluidRow(
        box(title = "Tweets timeline",
            width = 12,
            height = 500,
            status = "warning",
            solidHeader = F,
            dailylog_output("speech_dailylog")
        )
      ),
      fluidRow(
          box(title = "Most used terms",
              width = 5,
              height = 600,
              status = "warning",
              solidHeader = F,
              collapsible = T,
              collapsed = F,
              freqTables_output("speech_freqAnalysis")
          ),
        box(title = "Wordcloud",
            width = 7,
            height = 600,
            status = "warning",
            solidHeader = F,
            collapsible = T,
            collapsed = F,
            wordclouds_output("speech_freqAnalysis")
        )
      ),
      fluidRow(
        tmodelling_UI("speech_tmodels")
      ),
      fluidRow(h3(strong("Most popular tweets from the selected candidate")),
               widgets_outputs("speech_widgets")
      )
    )
    
    # Social panel
    
    
  )
)

## 1.4 Back front ==============================================================================================

appUI <- dashboardPage(header, sidebar, body,
                       controlbar = dashboardControlbar(),
                       footer = dashboardFooter(),
                       skin = "blue")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                2.  App SERVER                                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

appSERVER <- function(input, output, session) {
  
  glob <- reactiveValues()
  
  ## 2.1 Overview Panel  =======================================================================================
  
  overview_server("overview_panel")
  
  ## 2.2 Speech Panel  =========================================================================================
  
  # Applying a module that updates the comparison candidates inputs according to the main candidate input
  candidates_server(id = "speech_candidate", glob = glob)
  glob$submitted <- reactive(paste0(input$submit_speech, "TRUEcount")) %>%
    bindEvent(input$submit_speech)
  
  output$goul <- renderPrint(glob$comp_candidate)
  
  # Value boxes module
  valueBoxes_SERVER(id = "speech_vboxes", 
                    glob = glob)
  
  # Daily Activity Log module
  dailylog_server("speech_dailylog",
                  panel = "speech",
                  glob = glob)
  
  # Frequency Analysis module
  frequency_server("speech_freqAnalysis",
                   panel = "speech",
                   glob = glob)
  
  tmodelling_SERVER("speech_tmodels",
                    glob = glob)
  
  # Twitter Widgets module
  widgets_SERVER("speech_widgets",
                 panel = "speech",
                 glob = glob)
  
  ## 2.2 Speech Panel  =========================================================================================
  
}

shiny::shinyApp(appUI, appSERVER)

# library(rsconnect)
# deployApp()
