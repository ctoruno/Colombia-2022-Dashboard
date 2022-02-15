## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Political Observatory Colombia: Shiny App
##                    APP FILE
##
## Author:            Carlos A. Toruño Paniagua   (carlos.toruno@gmail.com)
##
## Creation date:     December 25th, 2021
##
## This version:      February 15th, 2022
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
    id = "panelTab",
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
      h3("Speech Analysis"),
      tags$head(
        tags$style(HTML("
      h4 {
        font-family: 'Fira Sans', sans-serif;
        text-align: center;
        font-weight: 300;
        font-size: 120%;
        margin-right: 75px;
        margin-left:  75px;
        color: #2A3948
        }"))
      ),
      h4(paste("In this section we present some data about the twitter activity",
                    "of the different candidates that at some point had intentions to run",
                    "for the presidential election in Colombia.")),
      br(),
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
        box(title = "Candidate overview",
            width = 9,
            height = 450,
            status = "warning",
            solidHeader = F
        )
      ),
      fluidRow(
        valueBoxes_UI(id = "speech_vboxes")
      ),
      fluidRow(
        box(id = "speech_tseries_box",
            title = "Tweets posted in time",
            width = 12,
            height = 500,
            status = "warning",
            solidHeader = F,
            collapsible = T,
            collapsed = T,
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
    ),

    # Social panel
    tabItem(
      tabName = "social",
      h3("Social Monitoring"),
      h4(paste("In this section we present some data about what the twitter community",
               "post about the different candidates that at some point had intentions to run",
               "for the presidential election in Colombia.")),
      br(),
      fluidRow(
        box(title = "Filters",
            width = 3,
            height = 450,
            status = "warning",
            solidHeader = F,
            candidates_input(id = "social_candidate"),
            actionBttn(
              inputId = "submit_social",
              label   = "Submit",
              style   = "unite",
              color   = "success",
              size    =  "sm")
        ),
        box(title = "Overview",
            width = 9,
            height = 450,
            status = "warning",
            solidHeader = F
        )
      ),
      fluidRow(
        valueBoxes_UI(id = "social_vboxes")
      ),
      fluidRow(
        box(id = "social_tseries_box",
            title = "Twitter mentions in time",
            width = 12,
            height = 500,
            status = "warning",
            solidHeader = F,
            collapsible = T,
            collapsed = T,
            dailylog_output("social_dailylog")
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
            freqTables_output("social_freqAnalysis")
        ),
        box(title = "Wordcloud",
            width = 7,
            height = 600,
            status = "warning",
            solidHeader = F,
            collapsible = T,
            collapsed = F,
            wordclouds_output("social_freqAnalysis")
        )
      ),
      fluidRow(h3(strong("Most popular tweets from the selected candidate")),
               widgets_outputs("social_widgets")
      )
    )
    
    
  )
)

## 1.4 Back front ==============================================================================================

appUI <- dashboardPage(title = "Colombia 2022: A twitter companion",
                       header, sidebar, body,
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
  
  observe({
    print(input$panelTab)
    print(glob$main_candidate)
    print(glob$comp_candidate)
  })
  
  activePanel <- reactive({
    if (input$panelTab == "speech"){
      panel <- "speechTAB"
    } else if (input$panelTab == "social"){
      panel <- "socialTAB"
    }
    return(panel)
  })

  ## 2.1 Overview Panel  =======================================================================================
  
  overview_server("overview_panel")
  
  ## 2.2 Speech Panel  =========================================================================================
  
  # Trigger definition and actions
  trigger_speech <- reactive(input$submit_speech)
  observeEvent(input$submit_speech, {
    updateBox(
      "speech_tseries_box",
      action = "toggle"
    )
  })

  # Applying a module that updates the comparison candidates inputs according to the main candidate input
  candidates_server(id = "speech_candidate", 
                    glob = glob,
                    resetX = activePanel)
  
  # Value boxes module
  valueBoxes_SERVER(id = "speech_vboxes", 
                    glob = glob,
                    panel = "speech",
                    trigger = trigger_speech)
  
  # Daily Activity Log module
  dailylog_server("speech_dailylog",
                  panel = "speech",
                  glob = glob,
                  trigger = trigger_speech)
  
  # Frequency Analysis module
  frequency_server("speech_freqAnalysis",
                   panel = "speech",
                   glob = glob,
                   trigger = trigger_speech)
  
  # Topic Models module
  tmodelling_SERVER("speech_tmodels",
                    glob = glob,
                    trigger = trigger_speech)
  
  # Twitter Widgets module
  widgets_SERVER("speech_widgets",
                 panel = "speech",
                 glob = glob)
  
  ## 2.3 Social Panel  =========================================================================================
  
  # Trigger definitions and actions
  trigger_social <- reactive(input$submit_social)
  observeEvent(input$submit_social, {
    updateBox(
      "social_tseries_box",
      action = "toggle"
    )
  })
  
  # Applying a module that updates the comparison candidates inputs according to the main candidate input
  candidates_server(id = "social_candidate", 
                    glob = glob,
                    resetX = activePanel)
  
  # Value boxes module
  valueBoxes_SERVER(id = "social_vboxes", 
                    glob = glob,
                    panel = "social",
                    trigger = trigger_social)

  # Daily Activity Log module
  dailylog_server("social_dailylog",
                  panel = "social",
                  glob = glob,
                  trigger = trigger_social)

  # Frequency Analysis module
  frequency_server("social_freqAnalysis",
                   panel = "social",
                   glob = glob,
                   trigger = trigger_social)
  
  # Twitter Widgets module
  widgets_SERVER("social_widgets",
                 panel = "social",
                 glob = glob)
  
}

shiny::shinyApp(appUI, appSERVER)

# library(rsconnect)
# deployApp()
