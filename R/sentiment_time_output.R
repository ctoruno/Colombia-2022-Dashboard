## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Political Observatory Colombia: Shiny App
##                    Time Sentiment Analysis
##
## Author:            David Granada   (dagrado@gmail.com)
##
## Creation date:     December 25th, 2021
##
## This version:      February 2nd, 2022
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                1.  Sentiment Time UI                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sentiment_time_output <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("sentiment.time.plot"))

  )
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                2.  Sentiment NRC SERVER                                                                       ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

'%not_in%' <- function(x,y)!('%in%'(x,y))
# Defining a SERVER function
sent_time_server <- function(id, panel, glob, trigger){
  
  moduleServer(
    id,
    function(input, output, session){
      
      # Reactive server
      tweets.plotly <- reactive({
        
        #& Date<=glob$date_selected
        # Creating ggplot
        sentiment.time.plot<-sentiment.df.ls[[2]]%>%
          filter(candidate %in% c(glob$main_candidate,glob$comp_candidate))%>%
          group_by(names_candidate,Date)%>%
          mutate(nrow=sum(n,na.rm=T))%>%ungroup()%>%
          group_by(names_candidate,sentiment,Date)%>%
          summarise(n=sum(n,na.rm=T),
                    nrow=first(nrow))%>%
          mutate(perc = n *100/ nrow)%>%
          ggplot(.,aes(x=Date, group=sentiment))+
          geom_line(aes(y=perc, color=sentiment))+
          facet_wrap(~names_candidate)+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
          labs(y = "% of Words", x = "Date")+
          guides(fill=guide_legend(title="Sentiment"))+
          scale_x_discrete( breaks=c("2021-11-01","2021-11-15",
                                     "2021-12-01","2021-12-15",
                                     "2022-01-01","2022-01-15",
                                     "2022-02-01","2022-02-15",
                                     "2022-03-01","2022-03-15"))+
          theme_bw() 

        # Converting to plotly element
        ggplotly(sentiment.time.plot, dynamicTicks = F) %>%
          layout(hovermode = "x") %>%
          config(displaylogo = F)
        
      }) %>%
        
      bindEvent(trigger())
       
      
      # Rendering output
      output$sentiment.time.plot <- renderPlotly(tweets.plotly())

    }
  )
}
