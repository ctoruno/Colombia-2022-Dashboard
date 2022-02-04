## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Political Observatory Colombia: Shiny App
##                    Topic Modelling Module
##
## Author:            Carlos A. Toruño Paniagua   (carlos.toruno@gmail.com)
##
## Creation date:     January 19th, 2022
##
## This version:      February 2nd, 2022
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                1.  Topic Modellling UI                                                                   ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


tmodelling_UI <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      title = "Topic Modelling",
      width = 12,
      height = 500,
      status = "warning",
      solidHeader = F,
      collapsible = T,
      collapsed = F,
      # sidebar = boxSidebar(
      #   id = "tmodels_sidebar",
      #   width = 25,
      #   tmodelling_UI("tmodels"),
      #   verbatimTextOutput("tmodels_fvalues")
      # ),
      accordion(
        id = ns("acc"),
        accordionItem(
          title = "Selected candidate",
          collapsed = F,
          width = 4,
          DTOutput(ns("tmodels_main"), width = "100%", height = 450)
        ),
        accordionItem(
          title = "Comparison candidate",
          collapsed = T,
          DTOutput(ns("tmodels_comp"), width = "100%", height = 450)
        )
      )
    )
  )
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                2.  Topic Modelling SERVER                                                                ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tmodelling_SERVER <- function(id, tokenized_data, glob){
  moduleServer(
    id,
    function(input, output, session){
      
      # Reactive server: Input data for rendering
      tmodels2render.ls <- reactive({
        
        list(
          "Topics Main" = tmodels.ls[[glob$main_candidate %>% str_sub(2)]],
          "Topics Comp" = tmodels.ls[[glob$comp_candidate %>% str_sub(2)]]
        )
        
      }) %>%
        bindEvent(glob$submitted())
      
      # Rendering output tables
      output$tmodels_main <- renderDT({
        datatable(tmodels2render.ls()[["Topics Main"]],
                  options = list(dom = 't',
                                 scrollX = T,      # This solves all the columns width issue
                                 autoWidth = T,    # Required to modify columns width
                                 columnDefs = list(list(width = '145px',
                                                        targets = "_all")))
        )
      })
      
      output$tmodels_comp <- renderDT({
        datatable(tmodels2render.ls()[["Topics Comp"]],
                  options = list(dom = 't',
                                 scrollX = T,      # This solves all the columns width issue
                                 autoWidth = T,    # Required to modify columns width
                                 columnDefs = list(list(width = '145px',
                                                        targets = "_all")))
        )
      })
      
    })
}



