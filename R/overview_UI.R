## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Political Observatory Colombia: Shiny App
##                    Overview Module
##
## Author:            Carlos A. Toruño Paniagua   (carlos.toruno@gmail.com)
##
## Creation date:     January 27th, 2021
##
## This version:      January 27th, 2022
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                1.  Overview UI                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

overview_UI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      h3(strong("This app contains information about:")),
      br(),
      box(
        title = "What do the presidential candidates talk about in Twitter",
        width = 6,
        height = 600,
        status = "warning",
        solidHeader = F,
        column(
          width = 12,
          valueBoxOutput(ns("speech_ntweets"), width = NULL),
          valueBoxOutput(ns("speech_tlapse"), width = NULL),
          valueBoxOutput(ns("speech_retwts"), width = NULL))
      ),
      box(
        title = "What do the people talk about them in Twitter",
        width = 6,
        height = 600,
        status = "warning",
        solidHeader = F,
        column(
          width = 12,
          valueBoxOutput(ns("social_ntweets"), width = NULL),
          valueBoxOutput(ns("social_nusers"), width = NULL),
          valueBoxOutput(ns("social_tlapse"), width = NULL))
      )
    ),
    fluidRow(
      box(title = "Daily activity log in Twitter",
          width = 12,
          height = 500,
          status = "warning",
          solidHeader = F,
          plotlyOutput(ns("timeline_plot"), height = "280px")
      )
    )
    # ,
    # fluidRow(
    #   box(title = "Most active candidate",
    #       width = 5,
    #       height = 600,
    #       status = "warning", 
    #       solidHeader = F),
    #   box(title = "Most mentioned candidate",
    #       width = 5,
    #       height = 600,
    #       status = "warning", 
    #       solidHeader = F)
    # ),
    # fluidRow(
    #   box(title = "Most popular candidate",
    #       width = 5,
    #       height = 600,
    #       status = "warning", 
    #       solidHeader = F),
    #   box(title = "Most retweeted candidate",
    #       width = 5,
    #       height = 600,
    #       status = "warning", 
    #       solidHeader = F)
    # ),
    # fluidRow(h3(strong("Most popular tweets about the candidates")),
    #          tags$head(tags$script(async = NA, src = "https://platform.twitter.com/widgets.js")),
    #          column(
    #            width = 4,
    #            height = 500,
    #            uiOutput(ns("pop_tweet1"))
    #          ),
    #          column(
    #            width = 4,
    #            height = 500,
    #            uiOutput(ns("pop_tweet2"))
    #          ),
    #          column(
    #            width = 4,
    #            height = 500,
    #            uiOutput(ns("pop_tweet3"))
    #          )
    # ),
    # fluidRow(h3(strong("Most popular tweets from the candidates")),
    #          tags$head(tags$script(async = NA, src = "https://platform.twitter.com/widgets.js")),
    #          column(
    #            width = 4,
    #            height = 500,
    #            uiOutput(ns("can_tweet1"))
    #          ),
    #          column(
    #            width = 4,
    #            height = 500,
    #            uiOutput(ns("can_tweet2"))
    #          ),
    #          column(
    #            width = 4,
    #            height = 500,
    #            uiOutput(ns("can_tweet3"))
    #          )
    # )
  )
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                2.  Overview SERVER                                                                       ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Defining SERVER function
overview_server <- function(id){
  
  moduleServer(
    id,
    function(input, output, session){
      
      # Rendering value boxes from the Speech Analysis
      output$speech_ntweets <- renderValueBox({
        valueBox(
          value = "Total tweets:",
          subtitle = overview.ls[["speech_ntweets"]],
          icon = icon("twitter"),
          color = "olive"
        )
      })
      
      output$speech_tlapse <- renderValueBox({
        valueBox(
          value = "Posted between:",
          subtitle = overview.ls[["speech_tlapse"]],
          icon = icon("calendar-alt"),
          color = "yellow"
        )
      })
      
      output$speech_retwts <- renderValueBox({
        valueBox(
          value = "Retweeted:",
          subtitle = overview.ls[["speech_retwts"]],
          icon = icon("retweet"),
          color = "navy"
        )
      })
      
      # Rendering value boxes from the Social Monitoring
      output$social_ntweets <- renderValueBox({
        valueBox(
          value = "Total tweets:",
          subtitle = overview.ls[["social_ntweets"]],
          icon = icon("twitter"),
          color = "olive"
        )
      })
      
      output$social_nusers <- renderValueBox({
        valueBox(
          value = "From:",
          subtitle = overview.ls[["social_nusers"]],
          icon = icon("users"),
          color = "yellow"
        )
      })
      
      output$social_tlapse <- renderValueBox({
        valueBox(
          value = "Posted between:",
          subtitle = overview.ls[["social_tlapse"]],
          icon = icon("calendar-alt"),
          color = "navy"
        )
      })
      
      # Creating time series ggplot
      tweets.plot <- ggplot(overview.ls[["daily_log"]], aes(x = date, group=1)) +
        geom_line(aes(y = n), color = "#DC863B") +
        theme_bw() +
        labs(title = "Tweets related to the candidates",
             x = NULL, 
             y = "Tweets") +
        scale_x_date(date_breaks = "4 weeks", date_labels = "%b%Y") +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black"),
              legend.position = "top",
              text = element_text(size = 18, family = "Ledger"),
              plot.title = element_text(size = 22),
              # plot.subtitle = element_text(size = 20, face = "italic"),
              plot.caption = element_text(vjust = -0.5, hjust = 1, size = 14))
      
      # Converting to plotly element
      tweets.plotly <- ggplotly(tweets.plot, dynamicTicks = T) %>%
        layout(hovermode = "x") %>%
        config(displaylogo = F)
      
      # Rendering plotly output
      output$timeline_plot <- renderPlotly(tweets.plotly)
      
    }
  )
}

