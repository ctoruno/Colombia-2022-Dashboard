## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Political Observatory Colombia: Sentiment Analysis UI
##
## Author:            Carlos A. Toruño Paniagua   (carlos.toruno@gmail.com)
##                    David Granada Donato        (dagrado@gmail.com)
##
## Creation date:     February 1st, 2022
##
## This version:      February 8th, 2022
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                1.  Sentiment Analysis UI                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sent_analysis_UI <- function(id){
  ns <- NS(id)
  tagList(
    
    h3("Sentiment Trends"),
    h4(paste("In this section we present some data regarding the sentiments surrounding the",
             "different candidates that at some point had intentions to run",
             "for the 2022 presidential election in Colombia.")),
    br(),
    
    #h3("Content under development")
    
    fluidRow(
      box(title = "Filters",
          width = 4,
          height = 600,
          status = "warning",
          solidHeader = F,
          div(
            id = "filters4sentiment",
            candidates_input(id = "sentiment_candidate")
          ),
          date_input(id="sentiment_date"),
          fluidRow(
            actionBttn(
              inputId = "submit_sentiment",
              label   = "Submit",
              style   = "unite", 
              color   = "success",
              size    =  "sm"),
            align = "center"
          )
      ),
      CandidateINFO_UI("sentiment_cinfo")
    ),
    fluidRow(
      box(title = "Main Sentiments",
          width = 12,
          height = 700,
          status = "warning",
          solidHeader = F,
          plotlyOutput(ns("sentiment.nrc.plot"))
      )
    )
  )
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                2.  Sentiment analysis SERVER                                                                       ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'%not_in%' <- function(x,y)!('%in%'(x,y))

sent_analyisis_server <- function(id){
  
  moduleServer(
    id,
    function(input, output, session){
      
      trigger_sentiment <- reactive(input$submit_sentiment)
      # observeEvent(input$reset_speech, {
      #   updateBox(
      #     "speech_tseries_box",
      #     action = "toggle"
      #   )
      # })
      
      # Applying a module that updates the comparison candidates inputs according to the main candidate input
      candidates_server(id = "sentiment_candidate", 
                        glob = glob,
                        resetX = activePanel)
      
      
      date_server(id = "sentiment_date",
                  glob = glob,
                  resetX = activePanel)
      
      # Candidate information module
      CandidateINFO_SERVER("sentiment_cinfo",
                           glob = glob,
                           trigger = trigger_sentiment)
      
      # Reactive server
      tweets.plotly <- reactive({
        
        #& Date<=glob$date_selected
        # Creating ggplot
        sentiment.nrc.plot<-sentiment.nrc.ls[[1]]%>%
          filter(candidate %in% c(glob$main_candidate,glob$comp_candidate))%>%
          filter(Date<=glob$date_selected )%>%
          filter(sentiment %not_in% c("positive","negative"))%>%
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
          guides(fill=guide_legend(title="Candidate"))+theme_bw() +
          scale_color_manual(name = NULL, values = wes_palette("Darjeeling2", 5, type = "discrete"))
        
        
        # Converting to plotly element
        ggplotly(sentiment.nrc.plot, dynamicTicks = F) %>%
          layout(hovermode = "x") %>%
          config(displaylogo = F)
        
      }) %>%
        
        bindEvent(trigger())
      
      
      # Rendering output
      output$sentiment.nrc.plot <- renderPlotly(tweets.plotly())
      
    })
    
}
      