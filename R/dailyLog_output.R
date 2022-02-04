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

timeline_output <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("timeline_plot"))
  )
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                2.  Timeline SERVER                                                                       ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Defining a SERVER function
timeline_server <- function(id, filtered_data, panel, glob){
  
  moduleServer(
    id,
    function(input, output, session){
      
      eventReactive(glob$submitted, {
        
        # Defining inputs
        input_data  <- filtered_data()
        others    <- glob$sec_candidates
        # This"others" variable needs to be specified within this server because it is going to be needed
        # within the next collapsed_data function.
        
        # Collapssing data
        collapsed_data <- collapse4Server(filtered_data = input_data,
                                          xdata = panel,
                                          glob = glob)
        
        # Creating ggplot
        mentions.plot <- ggplot(collapsed_data, aes(x = date, group=1)) +
          geom_line(aes(y = sum, color = user)) +
          theme_bw() +
          labs(title = "Timeline per Candidate",
               x = NULL, 
               y = "Mentions") +
          scale_x_date(date_breaks = "4 weeks", date_labels = "%b%Y") +
          scale_color_manual(name = NULL, values = wes_palette("Darjeeling2", 5, type = "discrete"))
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
        mentions.plotly <- ggplotly(mentions.plot, dynamicTicks = T) %>%
          rangeslider() %>%
          layout(hovermode = "x") %>%
          config(displaylogo = F)
        
      }) 
    }
  )
}