#' @title bayesROE: Bayesian Regions of Evidence package
#' 
#' @description The bayesROE package enables computation and visualization of
#'     the Bayesian Regions of Evidence with the function
#'     \code{\link{bayesROE}}.
#'
#' @references Pawel, S., Matthews, R. and Held, L. (2021). Comment on
#'     "Bayesian additional evidence for decision making under small sample uncertainty".
#'     Manuscript submitted for publication. Code available at
#'     \url{https://osf.io/ymx92/}
#'
#'  Höfler, M., Miller, R. (2022, April 04). Bayesian regions of evidence (for normal
#'  distributions). \doi{10.31234/osf.io/mg23h}
#'
#' 
#' @docType package
#' @name bayesROEpkg

NULL


#' @title Bayesian Region of Evidence
#'
#' @description This function computes and visualizes the Bayesian Region of
#'     Evidence, i.e. the set of normal priors for an effect size which - when
#'     combined with the observed data - lead to a specified posterior
#'     probability for the effect size being more extreme than a specified
#'     minimally relevant effect size.
#'
#' @param ee Effect estimate.
#' @param se Standard error of effect estimate.
#' @param delta Minimally relevant effect size. Defaults to zero. Can also be a
#'     vector of numerical values to separate different regions.
#' @param alpha Posterior probability that the effect size is less extreme than
#'     delta (i.e. smaller than delta for positive effect estimates, and larger
#'     than delta for negative effect estimates). Defaults to 0.025.
#' @param meanLim Limits of prior mean axis. Defaults to interval between zero
#'     and two times the effect estimate.
#' @param sdLim Limits of prior standard deviation axis. Defaults to interval
#'     between zero and three times the standard error.
#' @param nGrid Number of grid points (on the standard error axis). Defaults to
#'     500.
#' @param relative Logical indicating whether a second x-axis and y-axis with
#'     relative prior mean and relative prior variance should be displayed.
#'     Defaults to TRUE.
#' @param cols Character containing the HEX color code of the upper and lower
#'     region of evidence, respectively. Defaults to NULL, which triggers
#'     automated color picking by calling ggplot2:scale_fill_viridis_d()
#' @param addData Logical indicating if ee/sd are added to the plot. 
#'     Defaults to FALSE
#'
#' @return A bayesROE object (a list containing the ggplot object, the data for
#'     the plot, and the tipping point function)
#'
#' @references Pawel, S., Matthews, R. and Held, L. (2021). Comment on
#'     "Bayesian additional evidence for decision making under small sample uncertainty".
#'     Manuscript submitted for publication. Code available at
#'     \url{https://osf.io/ymx92/}
#'
#'  Höfler, M., Miller, R. (2022, April 04). Bayesian regions of evidence (for normal
#'  distributions). \doi{10.31234/osf.io/mg23h}
#'
#' @author Samuel Pawel
#'
#' @examples
#' ## data with p < 0.025 for H0: delta < 0, but p > 0.025 for H0: delta < 0.3
#' d <- 0.4
#' d_se <- 0.1
#' delta <- c(0, 0.3)
#' bayesROE(ee = d, se = d_se, delta = delta, meanLim = c(-1, 1))
#'
#' ## reproducing Figure 3 from Höfler (2021)
#' ee <- 9
#' se <- 3.9
#' delta <- c(0, 3.75)
#' bayesROE(ee = ee, se = se, delta = delta, meanLim = c(-5, 10),
#'          sdLim = c(0, 12), alpha = 0.05)$plot +
#'   ggplot2::coord_flip(xlim = c(0, 12), ylim = c(-5, 10))
#'
#' @export
bayesROE <- function(ee, se, delta = 0, alpha = 0.025,
                     meanLim = c(pmin(2*ee, 0), pmax(0, 2*ee)),
                     sdLim = c(0, 3*se), nGrid = 500, relative = TRUE,
                     cols = NULL, addData = FALSE) {
    ## input checks
    stopifnot(
        length(ee) == 1,
        is.numeric(ee),
        is.finite(ee),

        length(se) == 1,
        is.numeric(se),
        is.finite(se),
        0 < se,

        length(alpha) == 1,
        is.numeric(alpha),
        is.finite(alpha),
        0 < alpha, alpha < 1,

        length(delta) >= 1,
        !any(!is.numeric(delta)),
        !any(!is.finite(delta)),

        length(meanLim) == 2,
        !any(!is.numeric(meanLim)),
        !any(!is.finite(meanLim)),
        (meanLim[1] < meanLim[2]),

        length(sdLim) == 2,
        !any(!is.numeric(sdLim)),
        !any(!is.finite(sdLim)),
        !any(sdLim < 0),
        (sdLim[1] < sdLim[2]),

        length(nGrid) == 1,
        is.numeric(nGrid),
        is.finite(nGrid),
        nGrid > 0,

        length(relative) == 1,
        is.logical(relative),
        !is.na(relative),
        
        !xor(!is.null(cols), length(cols) == 2),
        !xor(!is.null(cols), is.character(cols)),
        
        length(addData) == 1,
        is.logical(addData),
        !is.na(addData)
    )


    ## define relative variance parameter grid
    gSeq <- seq(sdLim[1]/se, sdLim[2]/se, length.out = nGrid)^2

    ## define tipping point function for prior mean
    za <- stats::qnorm(p = 1 - alpha)
    muTP <- function(g, delta) {
        mu <- sign(ee)*za*se*sqrt(g*(1 + g)) - ee*g + delta*(1 + g)
        return(mu)
    }

    ## ## define tipping point function for relative prior variance
    ## gTP <- function(mu, delta) {
    ##     x <- (za*se + c(-1, 1)*sqrt(za^2*se^2 - 4*(ee - mu)*(mu - delta)))/
    ##         (2*(ee - mu))
    ##     g <- x^2/(1 - x^2)
    ##     return(g)
    ## }

    ## check posterior probability
    ## postP <- function(ee, se, mu, g, delta) {
    ##     postVar <- g/(1 + g)*se^2
    ##     postMean <- g/(1 + g)*ee + 1/(1 + g)*mu
    ##     p <- stats::pnorm(q = delta, mean = postMean, sd = sqrt(postVar),
    ##                       lower.tail = FALSE)
    ##     return(p)
    ## }

    ## compute tipping point for different deltas
    plotDF <- do.call("rbind", lapply(X = delta, FUN = function(d) {
        mu <- muTP(g = gSeq, delta = d)
        if (sign(ee) == -1) {
            lower <- -Inf
            upper <- mu
        } else {
            lower <- mu
            upper <- Inf
        }
        out <- data.frame(g = gSeq, sePrior = sqrt(gSeq)*se, mu = mu,
                          lower = lower, upper = upper, delta = d,
                          alpha = alpha)
    }))
    plotDF$deltaFormat <- factor(x = plotDF$delta,
                                 levels = delta[order(abs(delta))],
                                 labels = paste0("Delta == ",
                                                 signif(delta[order(abs(delta))], 3)))

    ## plot BRoE
    if (sign(ee) == -1) {
        legendString <- bquote({"Pr(effect size" < Delta * "| data, prior)"} >=
                               .(signif(100*(1 - alpha), 3)) * "%")
    } else {
        legendString <- bquote({"Pr(effect size" > Delta * "| data, prior)"} >=
                               .(signif(100*(1 - alpha), 3)) * "%")
    }
    ROEplot <- ggplot2::ggplot(data = plotDF) +
        ggplot2::geom_ribbon(ggplot2::aes_string(x = "sePrior",
                                                 ymin = "lower",
                                                 ymax = "upper",
                                                 fill = "deltaFormat"),
                             alpha = 0.5) +
        ggplot2::geom_line(ggplot2::aes_string(x = "sePrior", y = "mu",
                                               color = "deltaFormat"),
                           show.legend = FALSE) +
        ggplot2::coord_cartesian(ylim = meanLim, xlim = sdLim) +
        ggplot2::labs(fill = legendString) +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "top", panel.grid = ggplot2::element_blank(),
                       legend.text.align = 0)
    
    if(addData) ROEplot <- ROEplot + 
        ggplot2::annotate(geom = "point", x = se, y = ee, shape = "cross")
    
    if(is.null(cols)){
        ROEplot <- ROEplot +
            ggplot2::scale_fill_viridis_d(labels = scales::label_parse()) +
            ggplot2::scale_color_viridis_d(alpha = 1)
    }else{
        cols <- colorRampPalette(colors = cols, alpha = FALSE)(length(delta))
        names(cols) <- levels(ROEplot$data$deltaFormat)
        ROEplot <- ROEplot +
            ggplot2::scale_fill_manual(values = cols, labels = scales::label_parse()) +
            ggplot2::scale_color_manual(values = cols)
    }

    if (relative) {
        ROEplot <- ROEplot +
            ggplot2::scale_y_continuous(name = bquote("Prior mean"),
                                        sec.axis = ggplot2::sec_axis(trans = ~ ./ee,
                                                                     name = bquote("Relative prior mean")),
                                        expand = c(0, 0)) +
            ggplot2::scale_x_continuous(name = bquote("Prior standard deviation"),
                                        sec.axis = ggplot2::sec_axis(trans = ~ ./se,
                                                                     name = bquote("Relative prior standard deviation")),
                                        expand = c(0, 0))
    } else {
        ROEplot <- ROEplot +
            ggplot2::scale_y_continuous(name = bquote("Prior mean"),
                                        expand = c(0, 0)) +
            ggplot2::scale_x_continuous(name = bquote("Prior standard deviation"),
                                        expand = c(0, 0))
    }

    out <- list(plot = ROEplot, data = plotDF, meanFun = muTP)
    class(out) <- "bayesROE"
    return(out)

}

#' Print method for bayesROE object
#' @method print bayesROE
#' @param x A bayesROE object
#' @param ... Other arguments
#' @export
print.bayesROE <- function(x, ...) {
    print(x$plot)
    invisible(x)
}


#' @title Shiny UI to Visualize Bayesian Region of Evidence
#'
#' @description This function initializes and executes a Shiny session to interactively 
#'     visualize and explore the Bayesian Region of Evidence.
#'     Parameters entries from the sidebar are passed to the bayesROE function.
#'
#' @param init. List containing the arguments that are passed to the bayesROE function: ee, se, delta, alpha. 
#'     If delta is a numeric vector of length n, the Shiny app plot n adjustable regions of evidence.
#' 
#'
#' @return A bayesROE object (a list containing the ggplot object, the data for
#'     the plot, and the tipping point function)
#'
#' @references Pawel, S., Matthews, R. and Held, L. (2021). Comment on
#'     "Bayesian additional evidence for decision making under small sample uncertainty".
#'     Manuscript submitted for publication. Code available at
#'     \url{https://osf.io/ymx92/}
#'
#'  Höfler, M., Miller, R. (2022, April 04). Bayesian regions of evidence (for normal
#'  distributions). \doi{10.31234/osf.io/mg23h}
#'
#' @author Robert Miller
#'
#' @examples
#' ## reproducing Figure 3 from Höfler (2021)
#' inits <- list(ee = 9, se = 3.9, delta = c(0, 3.75), alpha = 0.025)
#' shinyROE(init = inits)
#'   
#'
#' @export
shinyROE <- function(init=NULL){
    library(shiny)
    library(shinyBS)
    library(colourpicker)
    
    inits <- list(ee = 6, se = 3.9, delta = 0, alpha = 0.05)
    if(!is.null(init)){
        inits[match.arg(names(init),names(inits), several.ok = TRUE)] <- init
    }
    ref_cols <- list(col_lower="#807096", col_upper="#3D3548", col_rope="#FF0000", col_conflict="#ABA545")
    
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
    
    ## SLIDERS TO BE ADDED TO sidebar_args
    #' @param meanLim Limits of prior mean axis. Defaults to interval between zero
    #'     and two times the effect estimate.
    #' @param sdLim Limits of prior standard deviation axis. Defaults to interval
    #'     between zero and three times the standard error.
    #' @param nGrid Number of grid points (on the standard error axis). Defaults to
    #'     500.
    #' @param relative Logical indicating whether a second x-axis and y-axis with
    #'     relative prior mean and relative prior variance should be displayed.
    #'     Defaults to TRUE.
    
    
    ui <- fluidPage(
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
        
    server <- function(input, output, session) {
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
                
                for(i in 1:input$nregion){
                    if(is.na(inits$delta[i])) inits$delta[i] <- inits$delta[length(inits$delta)] + 1
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
                
                for(i in 1:input$nregion){
                    if(is.na(inits$delta[i])) inits$delta[i] <- inits$delta[length(inits$delta)] + 1
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
            expr <- paste0("c(",paste(paste0("input$delta",1:input$nregion), collapse = ", "),")")
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
    
    shinyApp(ui, server)
}
