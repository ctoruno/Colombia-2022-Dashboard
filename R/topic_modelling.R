## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Political Observatory Colombia: Shiny App
##                    Topic Modelling Module
##
## Author:            Carlos A. Toruño Paniagua   (carlos.toruno@gmail.com)
##
## Creation date:     January 19th, 2022
##
## This version:      January 26th, 2022
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
    sliderInput(
      inputId = ns("iter"), 
      label = "Interactions:",
      min = 100, 
      max = 1000,
      value = 500
      ),
    numericInput(
      inputId = ns("ntopics"), 
      label = "Number of Topics:", 
      value = 4,
      min = 1,
      max = 6
      ),
    numericInput(
      inputId =  ns("alpha"),
      label = "Value of alpha:", 
      value = 0.5,
      min = 0,
      max = 25
      ),
    actionBttn(
      inputId = ns("update"),
      label = "Update results",
      style = "jelly", 
      color = "success",
      size  =  "sm"
      )
    # ,
    # verbatimTextOutput(ns("fit_values"))
  )
}

# tmodelling_OUTPUT <- function(id) {
#   ns <- NS(id)
#   tagList(
#     DTOutput(ns("top_words"))
#   )
# }



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                2.  Topic Modelling SERVER                                                                ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tmodelling_SERVER <- function(id, tokenized_data, glob){
  moduleServer(
    id,
    function(input, output, session){
      
      eventReactive(list(glob$submitted, input$update), {
        
        data4tmodels <- tokenized_data()
        
        # Specifiying parameters
        ntopics <- input$ntopics
        alpha   <- input$alpha
        i       <- input$iter
        
        # Casting a Document-Term Matrix
        DTMatrix <- data4tmodels %>% 
          count(tweet_id, words) %>% 
          filter(!str_detect(words, "^@")) %>%
          cast_dtm(document = tweet_id, term = words, value = n)
        
        # Fitting a LDA model with specified parameters
        LDAmodel <- LDA(x = DTMatrix, 
                        k = ntopics, 
                        method = "Gibbs",
                        control = list(
                          alpha = alpha,
                          seed = 10005,
                          iter = i))
        
        # Saving fit values
        loglike <- logLik(LDAmodel)
        perxty  <- perplexity(object = LDAmodel, newdata = DTMatrix)
        
        # Top words per topic
        TOP_words <- as.data.frame(terms(LDAmodel, k = 10))
        
        # Rendering outputs
        # output$fit_values <- renderPrint({
        #   paste0("Model has a Log-Likelihood of ", loglike, " and a Perplexity value of ", perxty)
        # })
        # 
        # output$top_words <- renderDT(datatable(TOP_words))
        
        # Returning list of values
        list("Fit Values" = paste0("Model has a Log-Likelihood of ",
                                   loglike,
                                   " and a Perplexity value of ",
                                   perxty),
             "Top Words"  = TOP_words)
      })
    }
  )
}