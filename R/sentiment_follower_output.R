## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Political Observatory Colombia: Shiny App
##                    Follower Sentiment Analysis
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

sentiment_follower_output <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("sentiment.follower.plot"))

  )
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                2.  Sentiment NRC SERVER                                                                       ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

'%not_in%' <- function(x,y)!('%in%'(x,y))
# Defining a SERVER function
sent_follower_server <- function(id, panel, glob, trigger){
  
  moduleServer(
    id,
    function(input, output, session){
      
      # Reactive server
      tweets.plotly <- reactive({
        
        #& Date<=glob$date_selected
        # Creating ggplot
        sentiment.follower.plot<-sentiment.df.ls[[3]]%>%
          filter(candidate %in% c(glob$main_candidate,glob$comp_candidate))%>%
          filter(Date<=glob$date_selected )%>%
          #filter(feeling %not_in% c("positive","negative"))%>%
          group_by(names_candidate,follower)%>%
          mutate(nrow=sum(n,na.rm=T))%>%ungroup()%>%
          group_by(names_candidate,feeling,follower)%>%
          summarise(n=sum(n,na.rm=T),
                    nrow=first(nrow))%>%
          mutate(perc = n *100/ nrow)%>%
          ggplot(.,aes(x=feeling,y=perc,fill=names_candidate))+
          geom_bar(stat="identity", position=position_dodge())+
          facet_wrap(~follower)+
          coord_flip()+
          theme(legend.position="bottom")+
          labs(y = "% of Words", x  = "Sentiment")+
          guides(fill=guide_legend(title="Candidate"))+
          theme_bw() 

        # Converting to plotly element
        ggplotly(sentiment.follower.plot, dynamicTicks = F) %>%
          layout(hovermode = "x") %>%
          config(displaylogo = F)
        
      }) %>%
        
      bindEvent(trigger())
       
      
      # Rendering output
      output$sentiment.follower.plot <- renderPlotly(tweets.plotly())

    }
  )
}
