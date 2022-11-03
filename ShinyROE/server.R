function(input, output, session) {
  output$add_sidebar <- renderUI({
    if(input$type == "thres"){
      sidebar_args <- tagList(
        numericInput(inputId = "alpha", label = "Alpha (%)",
                     value = inits$alpha[1]*100,
                     min = 0.1, max = 99.9, step = 0.1),
        bsTooltip(id = "alpha", trigger = "focus", 
                  title = "Posterior probability that effect size is less extreme than threshold(s)",
                  placement = "right", options = list(container = "body"))
      )
      
      for(i in 1:input$nregion){
        if(is.na(inits$delta[i])) inits$delta[i] <<- inits$delta[length(inits$delta)] + 1
        sidebar_args[[length(sidebar_args)+1]] <- numericInput(inputId = paste0("delta",i),
                                                               label = HTML(paste0("Delta", tags$sub(i))),
                                                               value = inits$delta[i],
                                                               min = -100, max = 100, step = 0.01)
      }
    }
    if(input$type == "prob"){
      sidebar_args <- tagList(
        #helpText("WARNING: Plot Type 'Probability' is currently not functional."),
        numericInput(inputId = "delta",
                     label = "Delta",
                     value = inits$delta[1],
                     min = -100, max = 100,
                     step = 0.01),
        bsTooltip(id = "delta", trigger = "focus", 
                  title = "Threshold representing the smallest relevant effect size.",
                  placement = "right", options = list(container = "body"))
      )
      
      for(i in 1:input$nregion){
        if(is.na(inits$alpha[i])) inits$alpha[i] <<- inits$alpha[length(inits$alpha)] / 2
        sidebar_args[[length(sidebar_args)+1]] <- numericInput(inputId = paste0("alpha",i),
                                                               label = HTML(paste0("Alpha", tags$sub(i) ," (%)")),
                                                               value = inits$alpha[i]*100,
                                                               min = 0.1, max = 99.9, step = 0.1)
      }
    }
    
    sidebar_args
  })
  
  output$plot_limits <- renderUI({
    tagList(
      sliderInput(inputId = "meanLim", label = "Limits x-Axis",
                  min = -3*abs(input$ee), max = 3*abs(input$ee), round = -1, ticks = FALSE,
                  value = c(pmin(2*input$ee, -0.5*input$ee), pmax(-0.5*input$ee, 2*input$ee)) ),
      sliderInput(inputId = "sdLim", label = "Limits y-Axis", 
                  min = 0, max = 5*input$se, round = -1, ticks = FALSE,
                  value = c(0, 3*input$se) )
    )
  })
  
  fig_height <- reactive({
    if(input$fig_aspect == "4:3"){
      return(round((input$fig_width/4) * 3))
    }else if(input$fig_aspect == "16:9"){
      return(round((input$fig_width/16) * 9))
    }else if(input$fig_aspect == "16:10"){
      return(round((input$fig_width/16) * 10))
    }
  })
  
  delta <- reactive({
    if(input$type == "thres"){
      expr <- paste0("c(",paste(paste0("input$delta",1:input$nregion), collapse = ", "),")")
    }else if(input$type == "prob"){
      expr <- paste0("input$delta")
    }
    eval(parse(text = expr))
  })
  
  alpha <- reactive({
    if(input$type == "thres"){
      expr <- paste0("input$alpha / 100")
    }else if(input$type == "prob"){
      expr <- paste0("c(",paste(paste0("input$alpha",1:input$nregion), collapse = ", "),") / 100")
    }
    eval(parse(text = expr))
  })
  
  ROEfig <- reactive({
    deltas <- delta()
    alphas <- alpha()
    if(length(alphas) >= 1 & length(deltas) >= 1){
      ROE <- ribbonROE(ee = input$ee, se = input$se, 
                      delta = deltas, alpha = alphas,
                      type = input$type, larger = TRUE,
                      meanLim = input$meanLim, sdLim = input$sdLim,
                      nGrid = 500, relative = TRUE, 
                      addRef = input$addRef, addEst = input$addEst,
                      cols = c(input$col_lower, input$col_upper), cols_alpha = input$col_alpha)
      
      if(!input$flip) ROE$plot <- suppressMessages(ROE$plot + coord_flip(ylim = input$meanLim, xlim = input$sdLim))
      return(ROE$plot)
    }else{
      return(NULL)
    }
  })
  
  output$ROEplot <- renderPlot({
    ROEfig()
  }, width = 640)
  
  output$fig_download <- downloadHandler(
    filename = function() {paste0('BayesROE_',input$fig_width,'mm.',input$fig_format)},
    content = function(file) {
      ggsave(file, device = input$fig_format, units = "mm", dpi = 300,
             width=input$fig_width, height=fig_height())
    }
  )
  
}