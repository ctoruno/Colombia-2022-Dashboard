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
      h3("Colombia 2022: A Twitter companion"),
      h4(paste("This app contains relevant information about the 2022 National Elections in Colombia.",
               "More specifically, it presents a brief summary about the twitter activity related to",
               "the different presidential candidates.")),
      br(),
      box(
        title = "What do the presidential candidates post in Twitter",
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
        title = "What do people talk about them in Twitter",
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
      box(title = "Daily activity related to the candidates in Twitter",
          width = 12,
          height = 500,
          status = "warning",
          solidHeader = F,
          plotlyOutput(ns("timeline_plot"), height = "280px")
      )
    ),
    fluidRow(
      box(title = "Most active candidates",
          width = 6,
          height = 600,
          status = "warning",
          solidHeader = F,
          plotOutput(ns("active5"))
      ),
      box(title = "Most mentioned candidates",
          width = 6,
          height = 600,
          status = "warning",
          solidHeader = F,
          plotOutput(ns("mentions5"))
      )
    )
    # ,
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
          value = tags$p("They have posted", style = "font-size: 50%;"),
          subtitle = paste0(overview.ls[["speech_ntweets"]], " tweets"),
          icon = tags$i(class = "fa fa-twitter", style="font-size: 70%"),
          color = "olive"
        )
      })
      
      output$speech_tlapse <- renderValueBox({
        valueBox(
          value = tags$p("Between", style = "font-size: 50%;"),
          subtitle = overview.ls[["speech_tlapse"]],
          icon = tags$i(class = "fa fa-calendar-alt", style="font-size: 70%"),
          color = "yellow"
        )
      })
      
      output$speech_retwts <- renderValueBox({
        valueBox(
          value = tags$p("Which have been retweeted", style = "font-size: 50%;"), 
          subtitle = paste0(overview.ls[["speech_retwts"]], " times"),
          icon = tags$i(class = "fa fa-retweet", style="font-size: 70%"),
          color = "navy"
        )
      })
      
      # Rendering value boxes from the Social Monitoring
      output$social_ntweets <- renderValueBox({
        valueBox(
          value = tags$p("We have registered", style = "font-size: 50%;"),
          subtitle = paste0(overview.ls[["social_ntweets"]], " tweets"),
          icon = tags$i(class = "fa fa-twitter", style="font-size: 70%"),
          color = "olive"
        )
      })
      
      output$social_nusers <- renderValueBox({
        valueBox(
          value = tags$p("From", style = "font-size: 50%;"),
          subtitle = paste0(overview.ls[["social_nusers"]], " users"),
          icon = tags$i(class = "fa fa-users", style="font-size: 70%"),
          color = "yellow"
        )
      })
      
      output$social_tlapse <- renderValueBox({
        valueBox(
          value = tags$p("Posted between", style = "font-size: 50%;"),
          subtitle = overview.ls[["social_tlapse"]],
          icon = tags$i(class = "fa fa-calendar-alt", style="font-size: 70%"),
          color = "navy"
        )
      })
      
      # Creating time series ggplot
      tweets.ggplot <- ggplot(overview.ls[["daily_log"]], aes(x = date, group=1)) +
        geom_line(aes(y = n), color = "#DC863B") +
        theme_bw() +
        labs(
          # title = "Tweets related to the candidates",
          x = NULL, 
          y = "Tweets"
          ) +
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
      tweets.plotly <- ggplotly(tweets.ggplot, dynamicTicks = T) %>%
        layout(hovermode = "x") %>%
        config(displaylogo = F)
      
      # Rendering plotly output
      output$timeline_plot <- renderPlotly(tweets.plotly)
      
      # Creating ggplot for most active candidates
      active5data  <- user_info.df %>% slice_max(tweets_count, n = 5)
      active5index <- match(paste0("@", active5data %>% pull(screen_name)), 
                            candidates.ls %>% map_chr(1))
      
      active5.ggplot <- ggplot(active5data, 
                               aes(x = tweets_count, 
                                   y = reorder(screen_name, tweets_count))) +
        geom_bar(stat = "identity", width = 0.8, fill = "#798E87") +
        geom_text(aes(label = tweets_count), 
                  nudge_x = 40,
                  size = 5, family = "Fira Sans") +
        labs(x = "Posted tweets", y = NULL) +
        theme_bw() +
        coord_cartesian(xlim = c(1500, 2250)) +
        scale_y_discrete(labels = rev(
          names(candidates.ls[active5index])
          )) +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black"),
              text = element_text(size = 18, family = "Ledger"))
        
      # Rendering plot
      output$active5 <- renderPlot(active5.ggplot)
      
      # Creating ggplot for most mentioned candidates
      mentions5data  <- user_info.df %>% 
        slice_max(total_comm_tweets, n = 5) %>%
        mutate(total_comm_tweets = total_comm_tweets/1000)
      mentions5index <- match(paste0("@", mentions5data %>% pull(screen_name)), 
                              candidates.ls %>% map_chr(1))
      
      mentions5.ggplot <- ggplot(mentions5data, 
                                 aes(x = total_comm_tweets, 
                                     y = reorder(screen_name, total_comm_tweets))) +
        geom_bar(stat = "identity", width = 0.8, fill = "#C27D38") +
        geom_text(aes(label = format(round(total_comm_tweets, digits = 1), 
                                     big.mark = ",", 
                                     decimal.mark = ".", 
                                     scientific = F)), 
                  nudge_x = 125,
                  size = 5, family = "Fira Sans") +
        labs(x = "Total mentions (thousands)", y = NULL) +
        theme_bw() +
        coord_cartesian(xlim = c(0, 1750)) +
        scale_y_discrete(labels = rev(
          names(candidates.ls[mentions5index])
        )) +
        scale_x_continuous(labels = function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = F)) +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black"),
              text = element_text(size = 18, family = "Ledger"))
      
      # Rendering plot
      output$mentions5 <- renderPlot(mentions5.ggplot)
      
      
    }
  )
}


