function(input, output, session) {
  delta <- reactive({
    expr <- paste0("c(",paste(paste0("input$delta",1:length(inits$delta)), collapse = ", "),")")
    eval(parse(text = expr))
  })
  
  ROEfig <- reactive({
    ROE <- bayesROE(ee = input$ee, se = input$se, delta = delta(), alpha = input$alpha/100,
                    meanLim = c(pmin(2*input$ee, 0), pmax(0, 2*input$ee)), sdLim = c(0, 3*input$se),
                    nGrid = 500, relative = TRUE, cols = c(input$col_lower, input$col_upper))
    
    if(!input$flip) ROE <- suppressMessages(ROE$plot + ggplot2::coord_flip())
    
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