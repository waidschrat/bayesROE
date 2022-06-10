#fundamental sidebar elements
sidebar_args <- list(
  fluidRow(
    column(radioButtons(inputId = "type", label = "Plot Type", 
                        choices = list("Threshold"="thres","Probability"="prob")),
           width = 6),
    column(sliderInput(inputId = "nregion", label = "Regions", 
                       min = 1, max = 5, value = length(inits$delta), step = 1, ticks = FALSE),
           width = 6)
  ),
  numericInput(inputId = "ee", label = "Effect Estimate", value = inits$ee,
               min = -10, max = 10, step = 0.01),
  numericInput(inputId = "se", label = "Standard Error", value = inits$se,
               min = 0.1, max = 10, step = 0.01),
  br()
)

#additional sidebar elements | Plot Type
sidebar_args[[length(sidebar_args)+1]] <- uiOutput("add_sidebar")
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
                    checkboxInput(inputId = "addData", label = "Add Data", value = FALSE),
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
