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
#                1.  App UI                                                                                 ----
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
    
    ### 1.3.1. App Overview Panel Tab ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#### 
    tabItem(
      tabName = "overview",
      overview_UI("overview_panel")
    ),

    ### 1.3.2. Speech Analysis Panel Tab +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++####
    tabItem(
      tabName = "speech",
      h3("Speech Analysis"),
      h4(paste("In this section we present some data about the twitter activity",
                    "of the different candidates that at some point had intentions to run",
                    "for the 2022 presidential election in Colombia.")),
      br(),
      fluidRow(
        box(title = "Filters",
            width = 3,
            height = 450,
            status = "warning",
            solidHeader = F,
            div(
              id = "filters4speech",
              candidates_input(id = "speech_candidate")
            ),
            fluidRow(
              actionBttn(
                inputId = "submit_speech",
                label   = "Submit",
                style   = "unite", 
                color   = "success",
                size    =  "sm"),
              # actionBttn(
              #   inputId = "reset_speech",
              #   label   = "Reset",
              #   style   = "unite", 
              #   color   = "warning",
              #   size    =  "sm"),
              align = "center"
            )
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

    ### 1.3.3. Social Monitoring Panel Tab +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++####
    
    tabItem(
      tabName = "social",
      h3("Social Monitoring"),
      h4(paste("In this section we present some data about what the twitter community",
               "post about the different candidates that at some point had intentions to run",
               "for the 2022 presidential election in Colombia.")),
      br(),
      fluidRow(
        box(title = "Filters",
            width = 3,
            height = 450,
            status = "warning",
            solidHeader = F,
            candidates_input(id = "social_candidate"),
            fluidRow(
              actionBttn(
                inputId = "submit_social",
                label   = "Submit",
                style   = "unite", 
                color   = "success",
                size    =  "sm"),
              # actionBttn(
              #   inputId = "reset_social",
              #   label   = "Reset",
              #   style   = "unite", 
              #   color   = "warning",
              #   size    =  "sm"),
              align = "center"
            )
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
    ),
    
    ### 1.3.4. Sentiment Trends Panel Tab ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++####
    
    tabItem(
      tabName = "sentiment",
      h3("Sentiment Trends"),
      h4(paste("In this section we present some data regarding the sentiments surrounding the",
               "different candidates that at some point had intentions to run",
               "for the 2022 presidential election in Colombia.")),
      br(),
      
      h3("Content under development")
      
    ),
    
    
    ### 1.3.5. Sentiment Trends Panel Tab ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++####
    
    tabItem(
      tabName = "about",
      h3("About"),
      br(),
      userBox(
        title = userDescription(
          title = h1("Carlos Toruño"),
          subtitle = "Lead Developer",
          type = 1,
          image = paste0("https://media.istockphoto.com/vectors/man-silhouette-profile-picture-vector-id5269",
                         "47869?k=20&m=526947869&s=612x612&w=0&h=j528SMpxB1AOCNs-WUcuQjvNRVuO-0PO1djfq-Rq6dE=")
        ),
        width = 12,
        status = "navy",
        collapsible = F,
        br(),
        tags$div(
          tags$ul(
            tags$li(paste("Development economist specialize in political issues.",
                          "Data analysis enthusiast. Follow me on:")),
            br(),
            fluidRow(
              align = "center",
              socialButton(
                href = "https://twitter.com/ctorunop",
                icon = icon("twitter")),
              socialButton(
                href = "https://github.com/ctoruno",
                icon = icon("github")),
              socialButton(
                href = "https://www.linkedin.com/in/carlostoruno/",
                icon = icon("linkedin")),
            ),
            br(),
            tags$li("Entire code is available in my GitHub page. For data extraction and analysis, you can check ",
                    a("this repository. ", href = "https://github.com/ctoruno/Colombia-2022-twitter-data"),
                    "Regarding app development, you can check ",
                    a("this repository.", href = "https://github.com/ctoruno/Colombia-2022-Dashboard")),
            br(),
            tags$li(paste("For comments and other inquiries you can email me to: carlos.toruno@gmail.com.",
                          "Full dataset used for the analysis displayed in this app is available by rquest.",
                          "Feel free to contact me."))
          )
        )
      )
      
    )
  )
)


## 1.4 Back front ==============================================================================================

appUI <- dashboardPage(title = "Colombia 2022: A twitter companion",
                       header, sidebar, body,
                       # controlbar = dashboardControlbar(),
                       # footer = dashboardFooter(),
                       skin = "blue")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
#                2.  App SERVER                                                                            ----
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
  # observeEvent(input$reset_speech, {
  #   updateBox(
  #     "speech_tseries_box",
  #     action = "toggle"
  #   )
  # })

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
  
  # Trigger definition and actions
  trigger_social <- reactive(input$submit_social)
  
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
