## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Political Observatory Colombia: Shiny App
##                    Overview Module
##
## Author:            Carlos A. Toruño Paniagua   (carlos.toruno@gmail.com)
##
## Creation date:     February 24th, 2022
##
## This version:      February 25th, 2022
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                1.  CandidateINFO UI                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

CandidateINFO_UI <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("info_panel"))
  )
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                2.  CandidateINFO SERVER                                                                  ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

CandidateINFO_SERVER <- function(id, glob, trigger){
  moduleServer(
    id,
    function(input, output, session){
      
      # Filtering data  
      filtered_info <- reactive({candidateINFO.df %>% 
          filter(user_name == glob$main_candidate)
      }) %>%
        bindEvent(trigger())
      
      # Rendering output
      
      output$info_panel <- renderUI({
        tagList(
          userBox(
            title = userDescription(
              title = h2(filtered_info() %>% pull(name)),
              subtitle = filtered_info() %>% pull(origin),
              type = 1,
              image = filtered_info() %>% pull(image_url)),
            width = 8,
            height = 600,
            status = "navy",
            collapsible = F,
            br(),
            tags$div(
              tags$ul(
                tags$li(strong("Age:"), filtered_info() %>% pull(age)),
                br(),
                tags$li(strong("Affiliations:"), filtered_info() %>% pull(affiliations)),
                br(),
                tags$li(strong("Political Background:"), filtered_info() %>% pull(background))
              )
            )
          )
        )
      })
    })
}



