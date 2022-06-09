function(input, output, session) {
  output$add_sidebar <- renderUI({
    if(input$type == "thres"){
      sidebar_args <- tagList(
        numericInput(inputId = "alpha", label = "Alpha (%)",
                     value = inits$alpha*100,
                     min = 0.1, max = 99.9, step = 0.1),
        bsTooltip(id = "alpha", trigger = "focus", 
                  title = "Posterior probability that the effect is less extreme than threshold(s)",
                  placement = "right", options = list(container = "body"))
      )
      for(i in 1:length(inits$delta)){
        sidebar_args[[length(sidebar_args)+1]] <- numericInput(inputId = paste0("delta",i),
                                                               label = paste("Effect Threshold",i),
                                                               value = inits$delta[i],
                                                               min = -10, max = 10,
                                                               step = 0.01)
      }
    }
    if(input$type == "prob"){
      sidebar_args <- tagList(
        helpText("WARNING: Plot Type 'Probability' is currently not functional."),
        sliderInput(inputId = "alpha", label = "Threshold",
                    value = inits$alpha*100,
                    min = 0.5, max = 99.5, step = 0.5, post = " %"),
        bsTooltip(id = "alpha", trigger = "focus",
                  title = "Threshold representing the smallest clinically relevant effect.",
                  placement = "right", options = list(container = "body"))
      )

      for(i in 1:length(inits$delta)){
        sidebar_args[[length(sidebar_args)+1]] <- sliderInput(inputId = paste0("delta",i),
                                                              label = paste("Probability",i),
                                                              value = inits$delta[i],
                                                              min = -10, max = 10,
                                                              step = 0.1)
      }
    }
    
    sidebar_args
  })
  
  delta <- reactive({
    expr <- paste0("c(",paste(paste0("input$delta",1:length(inits$delta)), collapse = ", "),")")
    eval(parse(text = expr))
  })
  
  ROEfig <- reactive({
    
    if(length(input$alpha) == 1){
      ROE <- bayesROE(ee = input$ee, se = input$se, delta = delta(),
                      alpha = input$alpha/100, addData = input$addData,
                      meanLim = c(pmin(2*input$ee, 0), pmax(0, 2*input$ee)), sdLim = c(0, 3*input$se),
                      nGrid = 500, relative = TRUE, cols = c(input$col_lower, input$col_upper))
      
      if(!input$flip) ROE <- suppressMessages(ROE$plot + ggplot2::coord_flip())
    }else{
      ROE <- NULL
    }
    
    return(ROE)
  })
  
  output$ROEplot <- renderPlot({
    ROEfig()
  }, width = 640)
  
  output$downloadPDF <- downloadHandler(
    filename = function() { paste('BayesROE.pdf', sep='') },
    content = function(file) {
      pdf(file, paper = input$format)
      print(ROEfig())
      dev.off()
    }
  )
}