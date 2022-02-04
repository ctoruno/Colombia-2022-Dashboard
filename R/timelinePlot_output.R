## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Political Observatory Colombia: Shiny App
##                    Timeline Plot Module
##
## Author:            Carlos A. Toruño Paniagua   (carlos.toruno@gmail.com)
##
## Creation date:     December 25th, 2021
##
## This version:      February 2nd, 2022
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                1.  Timeline UI                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

dailylog_output <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("dailylog_plot"))
  )
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                2.  Timeline SERVER                                                                       ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Defining a SERVER function
dailylog_server <- function(id, panel, glob){
  
  moduleServer(
    id,
    function(input, output, session){
      
      # Which data frame are we gonna work with?
      if (panel == "social"){
        data2plot <- daily_activity.ls$`Social Monitoring`
      } else {
        data2plot <- daily_activity.ls$`Speech Analysis`
      }
      
      # Reactive server
      tweets.plotly <- reactive({
        
        # Collapsing data
        collapsed_data <- data2plot %>%
          select(date, 
                 starts_with(glob$main_candidate %>% str_sub(2)),
                 starts_with(glob$comp_candidate %>% str_sub(2))) %>%
          pivot_longer(!date, 
                       names_to = "user",
                       names_pattern = "(.*)_tweets",
                       values_to = "tweets")
        
        # Creating ggplot
        tweets.plot <- ggplot(collapsed_data, aes(x = date, group=1)) +
          geom_line(aes(y = tweets, color = user)) +
          theme_bw() +
          labs(title = NULL,
               x = NULL, 
               y = "Tweets") +
          scale_x_date(date_breaks = "4 weeks", date_labels = "%b%Y") +
          scale_color_manual(name = NULL, values = wes_palette("Darjeeling2", 5, type = "discrete")) +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                axis.line = element_line(colour = "black"),
                legend.position = "top",
                text = element_text(size = 18, family = "Ledger"),
                plot.title = element_text(size = 22),
                plot.subtitle = element_text(size = 20, face = "italic"),
                plot.caption = element_text(vjust = -0.5, hjust = 1, size = 14))
        
        # Converting to plotly element
        ggplotly(tweets.plot, dynamicTicks = T) %>%
          rangeslider() %>%
          layout(hovermode = "x") %>%
          config(displaylogo = F)
        
      }) %>%
        bindEvent(glob$submitted())
      
      # Rendering output
      output$dailylog_plot <- renderPlotly(tweets.plotly())
      
      
    }
  )
}