## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Political Observatory Colombia: Shiny App
##                    Frequency Analysis Module
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
##                1.  Frequency Analysis UI                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

freqTables_output <- function(id) {
  ns <- NS(id)
  tagList(
    accordion(
      id = ns("acc"),
      accordionItem(
        title = "Selected candidate",
        collapsed = T,
        width = 4,
        DTOutput(ns("top_terms_mainX"), width = "100%", height = 450)
      ),
      accordionItem(
        title = "Comparison candidate",
        collapsed = T,
        DTOutput(ns("top_terms_compX"), width = "100%", height = 450)
      ),
      accordionItem(
        title = "Top used hashtags: selected candidate",
        collapsed = T,
        DTOutput(ns("top_hashtagsX"), width = "100%", height = 450)
      )
    )
  )
}

wordclouds_output <- function(id) {
  ns <- NS(id)
  tagList(
    wordcloud2Output(ns("wordcloud"),
                     width = "100%",
                     height = "600px")
  )
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                2.  Frequency Analysis SERVER                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Defining SERVER function
frequency_server <- function(id, panel, glob, trigger){
  
  moduleServer(
    id,
    function(input, output, session){
      
      # Which data frame are we gonna work with?
      if (panel == "social"){
        data2analyze <- freq_analysis.ls$`Social Monitoring`
      } else if (panel == "speech"){
        data2analyze <- freq_analysis.ls$`Speech Analysis`
      }
      
      # Reactive server
      data2render.ls <- reactive({
        
        # Top terms
        top_terms1 <- data2analyze$`Top Words` %>%
          select(starts_with(glob$main_candidate %>% str_sub(2)))
        
        top_terms2 <- data2analyze$`Top Words` %>%
          select(starts_with(glob$comp_candidate %>% str_sub(2)))
        
        # Top hashtags
        top_hashtags <- data2analyze$`Top Hashtags` %>%
          select(starts_with(glob$main_candidate %>% str_sub(2)))
        
        # Output list
        list("Top Terms1" = top_terms1,
             "Top Terms2" = top_terms2,
             "Top Hash"   = top_hashtags)
        
      }) %>%
        bindEvent(trigger())
      
      # Rendering wordcloud
      output$wordcloud <- renderWordcloud2({
        wordcloud2(data2render.ls()[["Top Terms1"]], size = 0.9,
                   color = rep_len(c("DarkRed", "CornflowerBlue", "DarkOrange"),
                                   nrow(data2render.ls()[["Top Terms1"]])),
                   ellipticity = 0.2, shuffle = F)
      })
      
      # Rendering tables
      output$top_terms_mainX <- renderDT({
        datatable(data2render.ls()[["Top Terms1"]],
                  options = list(dom = 'tp',
                                 scrollX = T,      # This solves all the columns width issue
                                 autoWidth = T,    # Required to modify columns width
                                 columnDefs = list(list(width = '50px',
                                                        targets = "_all")))
        )
      })
      
      output$top_terms_compX <- renderDT({
        datatable(data2render.ls()[["Top Terms2"]],
                  options = list(dom = 'tp',
                                 scrollX = T,      # This solves all the columns width issue
                                 autoWidth = T,    # Required to modify columns width
                                 columnDefs = list(list(width = '50px',
                                                        targets = "_all")))
        )
      })
      
      output$top_hashtagsX <- renderDT({
        datatable(data2render.ls()[["Top Hash"]],
                  options = list(dom = 't',
                                 scrollX = T,      # This solves all the columns width issue
                                 autoWidth = T,    # Required to modify columns width
                                 columnDefs = list(list(width = '50px',
                                                        targets = "_all")))
        )
      })
    })
}     

