## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Political Observatory Colombia: Shiny App
##                    NRC Sentiment Analysis
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
##                1.  Sentiment NRC UI                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sentiment_nrc_output <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("sentiment.nrc.plot"))

  )
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                2.  Sentiment NRC SERVER                                                                       ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

'%not_in%' <- function(x,y)!('%in%'(x,y))
# Defining a SERVER function
sent_nrc_server <- function(id, panel, glob, trigger){
  
  moduleServer(
    id,
    function(input, output, session){
      
      # Reactive server
      tweets.plotly <- reactive({
        
        #& Date<=glob$date_selected
        # Creating ggplot
        sentiment.nrc.plot<-sentiment.df.ls[[1]]%>%
          filter(candidate %in% c(glob$main_candidate,glob$comp_candidate))%>%
          filter(Date<=glob$date_selected )%>%
          #filter(sentiment %not_in% c("positive","negative"))%>%
          group_by(names_candidate)%>%
          mutate(nrow=sum(n,na.rm=T))%>%ungroup()%>%
          group_by(names_candidate,sentiment)%>%
          summarise(n=sum(n,na.rm=T),
                    nrow=first(nrow))%>%
          mutate(perc = n *100/ nrow) %>%
          ggplot(aes(x=sentiment, y=perc, fill=names_candidate))+
          geom_bar(stat="identity", position=position_dodge())+
          coord_flip()+
          theme(legend.position="bottom")+
          labs(y = "% of Words", x = "Sentiment")+
          guides(fill=guide_legend(title="Candidate"))+theme_bw() 

        # Converting to plotly element
        ggplotly(sentiment.nrc.plot, dynamicTicks = F) %>%
          layout(hovermode = "x") %>%
          config(displaylogo = F)
        
      }) %>%
        
      bindEvent(trigger())
       
      
      # Rendering output
      output$sentiment.nrc.plot <- renderPlotly(tweets.plotly())

    }
  )
}
