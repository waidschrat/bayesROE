if(sliderInputs){
  sidebar_args <- list(
    sliderInput(inputId = "ee", label = "Effect Estimate", value = inits$ee,
                min = -10, max = 10, step = 0.1),
    sliderInput(inputId = "se", label = "Standard Error", value = inits$se,
                min = 0.1, max = 10, step = 0.1),
    sliderInput(inputId = "alpha", label = "Alpha", value = inits$alpha*100,
                min = 0.5, max = 99.5, step = 0.5, post = " %")
  )
  for(i in 1:length(inits$delta)){
    sidebar_args[[length(sidebar_args)+1]] <- sliderInput(inputId = paste0("delta",i),
                                                          label = paste("Effect Threshold",i),
                                                          value = inits$delta[i],
                                                          min = -10, max = 10,
                                                          step = 0.1)
  }
} else {
  sidebar_args <- list(
    numericInput(inputId = "ee", label = "Effect Estimate", value = inits$ee,
                 min = -10, max = 10, step = 0.01),
    numericInput(inputId = "se", label = "Standard Error", value = inits$se,
                 min = 0.1, max = 10, step = 0.01),
    numericInput(inputId = "alpha", label = "Alpha (%)", value = inits$alpha*100,
                 min = 0.1, max = 99.9, step = 0.1)
  )
  for(i in 1:length(inits$delta)){
    sidebar_args[[length(sidebar_args)+1]] <- numericInput(inputId = paste0("delta",i),
                                                           label = paste("Effect Threshold",i),
                                                           value = inits$delta[i],
                                                           min = -10, max = 10,
                                                           step = 0.01)
  }
}
sidebar_args[[length(sidebar_args)+1]] <- bsTooltip(id = "alpha", trigger = "focus",
                                                    title = "Posterior probability that the effect size is less extreme than delta",
                                                    placement = "right", options = list(container = "body"))
sidebar_args[["width"]] <- 3


fluidPage(
  titlePanel("Bayesian Regions of Evidence"),
  
  sidebarLayout(
    do.call(sidebarPanel, args = sidebar_args),
    mainPanel(
      wellPanel(
        plotOutput(outputId = "ROEplot")
      ),
      wellPanel(
        actionButton(inputId = "Download", "Download Plot", width = "200px"),
        br(),br(),
        numericInput(inputId = "width", label = "Width (px)",
                     min = 640, max = 1600, value = 800, width = "200px"),
        numericInput(inputId = "heigth", label = "Height (px)", 
                     min = 480, max = 1200, value = 600, width = "200px"),
        colourInput(inputId = "col_lower", label = "Lower Colour Key", value = ref_cols$col_lower),
        colourInput(inputId = "col_upper", label = "Upper Colour Key", value = ref_cols$col_upper)
      ), width = 9)
  )
)
