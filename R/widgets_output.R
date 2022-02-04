## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Political Observatory Colombia: Shiny App
##                    Tweets Widgets Module
##
## Author:            Carlos A. Toruño Paniagua   (carlos.toruno@gmail.com)
##
## Creation date:     January 14th, 2022
##
## This version:      February 2nd, 2022
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                1.  Widgets UI                                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

widgets_outputs <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(tags$script(async = NA, src = "https://platform.twitter.com/widgets.js")),
    column(width = 4,
           height = 500,
           uiOutput(ns("top_tweet1"))),
    column(width = 4,
           height = 500,
           uiOutput(ns("top_tweet2"))),
    column(width = 4,
           height = 500,
           uiOutput(ns("top_tweet3")))
  )
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                2.  Widgets SERVER                                                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

widgets_SERVER <- function(id, panel, glob){
  moduleServer(
    id,
    function(input, output, session){
      
      # Which data frame are we gonna work with?
      if (panel == "social"){
        URLdata <- twitter_widgets_urls.ls$`Social Monitoring`
      } else {
        URLdata <- twitter_widgets_urls.ls$`Speech Analysis`
      }
      
      # Reactive server
      urls2render <- reactive({
        
        candidate <- glob$main_candidate %>% str_sub(2)
        
        list(
          "top_tweet1" = URLdata %>% 
            select(tweet, starts_with(candidate)) %>%
            filter(tweet == "Top 1") %>%
            pull(),
          "top_tweet2" = URLdata %>% 
            select(tweet, starts_with(candidate)) %>%
            filter(tweet == "Top 2") %>%
            pull(),
          "top_tweet3" = URLdata %>% 
            select(tweet, starts_with(candidate)) %>%
            filter(tweet == "Top 3") %>%
            pull()
        )
      })
      
      # Rendering widgets
      output$top_tweet1 <- renderUI({
        tagList(
          tags$blockquote(class = "twitter-tweet",
                          tags$a(href = urls2render()[["top_tweet1"]])),
          tags$script('twttr.widgets.load(document.getElementById("tweet"));'),
        )
      })

      output$top_tweet2 <- renderUI({
        tagList(
          tags$blockquote(class = "twitter-tweet",
                          tags$a(href = urls2render()[["top_tweet2"]])),
          tags$script('twttr.widgets.load(document.getElementById("tweet"));')
        )
      })

      output$top_tweet3 <- renderUI({
        tagList(
          tags$blockquote(class = "twitter-tweet",
                          tags$a(href = urls2render()[["top_tweet3"]])),
          tags$script('twttr.widgets.load(document.getElementById("tweet"));')
        )
      })
      
    })
}


