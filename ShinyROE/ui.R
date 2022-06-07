if(sliderInputs){
  sidebar_args <- list(
    radioButtons(inputId = "type", label = "Plot Type", 
                 choices = list("Threshold"="thres","Probability"="prob")),
    sliderInput(inputId = "ee", label = "Effect Estimate", value = inits$ee,
                min = -10, max = 10, step = 0.1),
    sliderInput(inputId = "se", label = "Standard Error", value = inits$se,
                min = 0.1, max = 10, step = 0.1),
    br()
  )
  
  #sidebar elements | Plot Type == "Threshold"
  sidebar_args[[length(sidebar_args)+1]] <- uiOutput("slide_threshold_sidebar")
  for(i in 1:length(inits$delta)){
    sidebar_args[[length(sidebar_args)+1]] <- sliderInput(inputId = paste0("delta",i),
                                                          label = paste("Effect Threshold",i),
                                                          value = inits$delta[i],
                                                          min = -10, max = 10,
                                                          step = 0.1)
  }
} else {
  sidebar_args <- list(
    radioButtons(inputId = "type", label = "Plot Type", 
                 choices = list("Threshold"="thres","Probability"="prob")),
    numericInput(inputId = "ee", label = "Effect Estimate", value = inits$ee,
                 min = -10, max = 10, step = 0.01),
    numericInput(inputId = "se", label = "Standard Error", value = inits$se,
                 min = 0.1, max = 10, step = 0.01),
    br()
  )
  
  #sidebar elements | Plot Type == "Threshold"
  sidebar_args[[length(sidebar_args)+1]] <- uiOutput("num_threshold_sidebar")
  for(i in 1:length(inits$delta)){
    sidebar_args[[length(sidebar_args)+1]] <- numericInput(inputId = paste0("delta",i),
                                                           label = paste("Effect Threshold",i),
                                                           value = inits$delta[i],
                                                           min = -10, max = 10,
                                                           step = 0.01)
  }
}

sidebar_args[["width"]] <- 3


fluidPage(
  titlePanel("Bayesian Regions of Evidence"),
  
  sidebarLayout(
    do.call(sidebarPanel, args = sidebar_args),
    mainPanel(width = 9,
              wellPanel(
                plotOutput(outputId = "ROEplot")
              ),
              fluidRow(
                column(
                  wellPanel(
                    checkboxInput(inputId = "flip", label = "Flip Axes", value = FALSE),
                    colourInput(inputId = "col_lower", label = "Lower Colour Key", value = ref_cols$col_lower),
                    colourInput(inputId = "col_upper", label = "Upper Colour Key", value = ref_cols$col_upper)
                  ), width = 6),
                column(
                  wellPanel(
                    downloadButton(outputId = "downloadPDF",
                                   label = "Download PDF",
                                   width = "200px"),
                    br(),br(),
                    radioButtons(inputId = "format", label = "Format", 
                                 choices =  list("A4 (210 x 297 mm)"="a4r", "Legal (216 x 356 mm)"="USr"))
                  ), width = 6)
              )
          )
    )
)
