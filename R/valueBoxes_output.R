## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Political Observatory Colombia: Shiny App
##                    Candidates Twitter Value Boxes Module
##
## Author:            Carlos A. Toruño Paniagua   (carlos.toruno@gmail.com)
##
## Creation date:     December 25th, 2021
##
## This version:      February 15th, 2022
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                1.  Twitter Boxes UI                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

valueBoxes_UI <- function(id){
  ns <- NS(id)
  # if (panel == "speech"){
    tagList(
      valueBoxOutput(ns("valuebox1"), width = 4),
      valueBoxOutput(ns("valuebox22"), width = 4),
      valueBoxOutput(ns("valuebox3"), width = 4)
    )
  # }
  # if (panel == "social"){
  #   tagList(
  #     valueBoxOutput(ns("box4"), width = 4),
  #     valueBoxOutput(ns("box5"), width = 4),
  #     valueBoxOutput(ns("box6"), width = 4)
  #   )
  # }
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                2.  Twitter Boxes SERVER                                                                  ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

valueBoxes_SERVER <- function(id, glob, panel, trigger){
  moduleServer(
    id,
    function(input, output, session){
      
      # Filtering data  
      filtered_info <- reactive({user_info.df %>% 
          filter(screen_name == glob$main_candidate %>% str_sub(2))
      }) %>%
        bindEvent(trigger())
      
      # Speech Analysis panel
      if (panel == "speech") {
        
        # Rendering outputs
        output$valuebox1 <- renderValueBox({
          valueBox(
            subtitle = "Using Twitter since:",
            value = tags$p(format(filtered_info() %>% pull(account_created_at), "%b %d, %Y"), 
                           style = "font-size: 75%;"),
            icon = tags$i(class = "fa fa-twitter", style="font-size: 70%"),
            color = "olive"
          )
        })
        
        output$valuebox22 <- renderValueBox({
          valueBox(
            subtitle = "Tweets posted since June-2021:",
            value = tags$p(formatC(filtered_info() %>% pull(tweets_count), 
                                   format="f", big.mark = ",", digits=0),
                           style = "font-size: 75%;"),
            icon = tags$i(class = "fa fa-pen-fancy", style="font-size: 70%"),
            color = "yellow"
          )
        })
        
        output$valuebox3 <- renderValueBox({
          valueBox(
            subtitle = "Followers",
            value = tags$p(formatC(filtered_info() %>% pull(followers_count), 
                                   format="f", big.mark = ",", digits=0),
                           style = "font-size: 75%;"),
            icon = tags$i(class = "fa fa-users", style="font-size: 70%"),
            color = "navy"
          )
        })
      }
      
      # Social Monitoring panel
      if (panel == "social") {
        
        output$valuebox1 <- renderValueBox({
          valueBox(
            subtitle = "Total mentions since Nov-2021",
            value = tags$p(formatC(filtered_info() %>% pull(total_comm_tweets), 
                                   format="f", big.mark = ",", digits=0),
                           style = "font-size: 75%;"),
            icon = tags$i(class = "fa fa-twitter", style="font-size: 70%"),
            color = "olive"
          )
        })
        
        output$valuebox22 <- renderValueBox({
          valueBox(
            subtitle = "Amount of twitter users who mentioned him/her",
            value = tags$p(formatC(filtered_info() %>% pull(total_users), 
                                   format="f", big.mark = ",", digits=0),
                           style = "font-size: 75%;"),
            icon = tags$i(class = "fa fa-users", style="font-size: 70%"),
            color = "yellow"
          )
        })
        
        output$valuebox3 <- renderValueBox({
          valueBox(
            subtitle = paste("Day with the highest activity in Twitter with",
                             formatC(filtered_info() %>% pull(max_mentions_n), 
                                     format="f", big.mark = ",", digits=0),
                             "mentions"),
            value = tags$p(format(filtered_info() %>% pull(max_mentions_date), "%b %d, %Y"),
                           style = "font-size: 75%;"),
            icon = tags$i(class = "fa fa-calendar-alt", style="font-size: 70%"),
            color = "navy"
          )
        })
        
      }
    })
}




