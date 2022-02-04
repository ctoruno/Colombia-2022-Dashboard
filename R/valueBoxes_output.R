## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Political Observatory Colombia: Shiny App
##                    Candidates Twitter Value Boxes Module
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
##                1.  Twitter Boxes UI                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

valueBoxes_UI <- function(id){
  ns <- NS(id)
  tagList(
    valueBoxOutput(ns("user_since"), width = 4),
    valueBoxOutput(ns("tweets_count"), width = 4),
    valueBoxOutput(ns("followers_count"), width = 4)
  )
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                2.  Twitter Boxes SERVER                                                                  ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

valueBoxes_SERVER <- function(id, glob){
  moduleServer(
    id,
    function(input, output, session){
      
      # Filtering data  
      filtered_info <- reactive({user_info.df %>% 
          filter(screen_name == glob$main_candidate %>% str_sub(2))
      }) %>%
        bindEvent(glob$submitted())
      
      # Rendering outputs
      output$user_since <- renderValueBox({
        valueBox(
          subtitle = "Using Twitter since:",
          value = format(filtered_info() %>% pull(account_created_at), "%d-%b-%Y"),
          icon = icon("twitter"),
          color = "olive"
        )
      })
      
      output$tweets_count <- renderValueBox({
        valueBox(
          subtitle = "Tweets posted since June-2021:",
          value = formatC(filtered_info() %>% pull(tweets_count), 
                          format="f", big.mark = ",", digits=0),
          icon = icon("pen-fancy"),
          color = "yellow"
        )
      })
      
      output$followers_count <- renderValueBox({
        valueBox(
          subtitle = "Followers",
          value = formatC(filtered_info() %>% pull(followers_count), 
                          format="f", big.mark = ",", digits=0),
          icon = icon("users"),
          color = "navy"
        )
      })  
      
      
    })
}



