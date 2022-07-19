library(shinyBS)
library(ggplot2)
library(colourpicker)

if(!exists("inits")) inits <- list(ee = 6, se = 3.9, delta = c(0,-1,1), alpha = c(0.025,0.05,0.01))
ref_cols <- list(col_lower="#807096", col_upper="#3D3548", col_rope="#FF0000", col_conflict="#ABA545")

bayesROE <- function(ee, se, delta = 0, alpha = 0.025,
                     type="threshold", larger = TRUE,
                     meanLim = c(pmin(2*ee, 0), pmax(0, 2*ee)),
                     sdLim = c(0, 3*se), nGrid = 500, relative = TRUE,
                     cols = NULL, cols_alpha = 1, addRef = TRUE) {
  ## input checks
  stopifnot(
    length(ee) == 1,
    is.numeric(ee),
    is.finite(ee),
    
    length(se) == 1,
    is.numeric(se),
    is.finite(se),
    0 < se,
    
    length(alpha) >= 1,
    !any(!is.numeric(alpha)),
    !any(!is.finite(alpha)),
    !any(!0 < alpha), !any(!alpha < 1),
    
    length(delta) >= 1,
    !any(!is.numeric(delta)),
    !any(!is.finite(delta)),
    
    length(type) == 1,
    is.character(type),
    !is.null(type),
    
    length(larger) == 1,
    is.logical(larger),
    !is.na(larger),
    
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
    
    length(cols_alpha) == 1,
    is.numeric(cols_alpha),
    is.finite(cols_alpha),
    0 <= cols_alpha, cols_alpha <= 1,
    
    length(addRef) == 1,
    is.logical(addRef),
    !is.na(addRef)
  )
  
  
  ## define relative variance parameter grid
  gSeq <- seq(sdLim[1]/se, sdLim[2]/se, length.out = nGrid)^2
  
  ## define tipping point function for prior mean
  asign <- ifelse(larger, 1, -1)
  muTP <- function(g, delta, za) {
    mu <- asign*za*se*sqrt(g*(1 + g)) - ee*g + delta*(1 + g)
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
  
  ## compute tipping point for different deltas / alphas
  if(grepl(type, "threshold")){
    plotDF <- do.call("rbind", lapply(X = delta, FUN = function(x) {
      za <- stats::qnorm(p = 1 - alpha[1])
      mu <- muTP(g = gSeq, delta = x, za = za)
      if (!larger) {
        lower <- -Inf
        upper <- mu
      } else {
        lower <- mu
        upper <- Inf
      }
      out <- data.frame(g = gSeq, sePrior = sqrt(gSeq)*se, mu = mu,
                        lower = lower, upper = upper, delta = x,
                        alpha = alpha[1])
    }))
    plotDF$xFormat <- factor(x = plotDF$delta,
                             levels = delta[order(delta)],
                             labels = paste0("Delta == ",
                                             signif(delta[order(delta)], 3)))
  }else if(grepl(type, "probability")){
    plotDF <- do.call("rbind", lapply(X = alpha, FUN = function(x) {
      za <- stats::qnorm(p = 1 - x)
      mu <- muTP(g = gSeq, delta = delta[1], za = za)
      if (!larger) {
        lower <- -Inf
        upper <- mu
      } else {
        lower <- mu
        upper <- Inf
      }
      out <- data.frame(g = gSeq, sePrior = sqrt(gSeq)*se, mu = mu,
                        lower = lower, upper = upper, delta = delta[1],
                        alpha = x)
    }))
    plotDF$xFormat <- factor(x = plotDF$alpha,
                             levels = alpha[order(alpha, decreasing = TRUE)],
                             labels = paste0("alpha == ",
                                             signif(alpha[order(alpha, decreasing = TRUE)], 3)))
  }else{
    stop(paste("argument type =",type,"is unknown"))
  }
  
  ## plot BRoE
  if (!larger) {
    if(grepl(type, "threshold")){
      legendString <- bquote({"Pr(effect size" < Delta * "| data, prior)"} >=
                               .(signif(100*(1 - alpha[1]), 3)) * "%")
    }else{
      legendString <- bquote({"Pr(effect size" < .(signif(delta[1], 3)) * "| data, prior)"} >=
                               1 - alpha * " ")
    }
  } else {
    if(grepl(type, "threshold")){
      legendString <- bquote({"Pr(effect size" > Delta * "| data, prior)"} >=
                               .(signif(100*(1 - alpha[1]), 3)) * "%")
    }else{
      legendString <- bquote({"Pr(effect size" > .(signif(delta[1], 3)) * "| data, prior)"} >=
                               1 - alpha * " ")
    }
  }
  ROEplot <- ggplot2::ggplot(data = plotDF) +
    ggplot2::geom_ribbon(ggplot2::aes_string(x = "sePrior",
                                             ymin = "lower",
                                             ymax = "upper",
                                             fill = "xFormat"),
                         alpha = cols_alpha) +
    ggplot2::geom_line(ggplot2::aes_string(x = "sePrior", y = "mu",
                                           color = "xFormat"),
                       show.legend = FALSE) +
    ggplot2::coord_cartesian(ylim = meanLim, xlim = sdLim) +
    ggplot2::labs(fill = legendString) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top", panel.grid = ggplot2::element_blank(),
                   legend.text.align = 0)
  
  if(addRef) {
    ref <- with(plotDF[plotDF$alpha == alpha[1] & plotDF$delta == delta[1],], approx(mu, sePrior, 0))
    ROEplot <- ROEplot + 
      ggplot2::geom_vline(xintercept = ref$y, lty = 2, lwd = 0.5) + 
      ggplot2::geom_hline(yintercept = ref$x, lty = 2, lwd = 0.5)
      #ggplot2::annotate(geom = "point", x = se, y = ee, shape = "cross")
  }
  
  if(is.null(cols)){
    ROEplot <- ROEplot +
      ggplot2::scale_fill_viridis_d(labels = scales::label_parse()) +
      ggplot2::scale_color_viridis_d(alpha = 1)
  }else{
    nregion <- length(levels(ROEplot$data$xFormat))
    cols <- colorRampPalette(colors = cols, alpha = FALSE)(nregion)
    names(cols) <- levels(ROEplot$data$xFormat)
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

print.bayesROE <- function(x, ...) {
  print(x$plot)
  invisible(x)
}