library(shinyBS)
library(ggplot2)
library(colourpicker)

if(!exists("inits")) inits <- list(ee = 6, se = 3.9, delta = c(0,-1,1), alpha = c(0.025,0.05,0.01))
ref_cols <- list(col_lower="#F5FF82", col_upper="#27CC1E", col_rope="#FF0000", col_conflict="#ABA545") #Default Color Palette
#ref_cols <- list(col_lower="#807096", col_upper="#3D3548", col_rope="#FF0000", col_conflict="#ABA545") #Alternative Color Palette



#' Print method for bayesROE object
#' @method print bayesROE
#' @param x A bayesROE object
#' @param ... Other arguments
#' @export
print.bayesROE <- function(x, ...) {
  print(x$plot)
  invisible(x)
}


rasterROE <- function(ee, se, delta = 0, alpha = 0.025,
                      type="threshold", larger = TRUE,
                      meanLim = c(pmin(2*ee, 0), pmax(0, 2*ee)),
                      sdLim = c(0, 3*se), nGrid = 100,
                      cols = NULL, cols_alpha = 1) {
  
  ## auxiliary functions
  posterior <- function(obs_pars, prior_pars){
    posterior_pars <- rep(NA,2) #posterior container
    obs_pars[2] <- obs_pars[2]^2 #transform to variance
    prior_pars[2] <- prior_pars[2]^2 #transform to variance
    
    if(prior_pars[2] == 0){
      posterior_pars <- prior_pars
    }else{
      posterior_pars[1] <- (prior_pars[1]/prior_pars[2] + obs_pars[1]/obs_pars[2]) / (1/prior_pars[2] + 1/obs_pars[2]) #posterior mean
      posterior_pars[2] <- sqrt(1 / (1/prior_pars[2] + 1/obs_pars[2])) #posterior standard deviation
    }
    return(posterior_pars)
  }
  
  posterior_grid <- Vectorize(function(obs_pars, prior_mu, prior_sd){
    posterior(obs_pars, c(prior_mu, prior_sd))
  }, vectorize.args = c("prior_mu","prior_sd"))
  
  prob_grid <- Vectorize(function(posterior_pars, delta=0, larger=TRUE){
    if(posterior_pars[2] == 0){
      if(posterior_pars[1] != delta){
        return(0)
      }else{
        return(1)
      }
    }
    
    zval <- (posterior_pars[1] - delta)/posterior_pars[2]
    if(larger){
      return(1 - pnorm(zval))
    }else{
      return(pnorm(zval))
    }
  }, vectorize.args = c("delta"))
  
  ## define parameter grid
  gSeq <- seq(sdLim[1]/se, sdLim[2]/se, length.out = nGrid)^2
  muSeq <- seq(meanLim[1], meanLim[2], length.out = nGrid)
  
  plotDF <- expand.grid("g"=gSeq, "mu"=muSeq)
  plotDF$sePrior = sqrt(plotDF$g)*se
  
  ## calculate posterior parameters
  posteriorPars <- posterior_grid(obs_pars = c(ee,se),
                                  prior_mu = plotDF$mu,
                                  prior_sd = plotDF$sePrior)
  
  ## determine posterior probability and RoE
  if(grepl(type, "threshold")){
    
    for(i in delta[order(delta)]){
      plotDF$Prob <- apply(posteriorPars, 2, prob_grid, delta = i, larger = larger)
      plotDF$RoE[plotDF$Prob <= alpha[1]] <- i
    }
    
    plotDF$xFormat <- factor(x = plotDF$RoE,
                             levels = delta[order(delta)],
                             labels = paste0("Delta == ",
                                             signif(delta[order(delta)], 3)))
  }else if(grepl(type, "probability")){
    
    for(i in alpha[order(alpha, decreasing = TRUE)]){
      plotDF$Prob <- apply(posteriorPars, 2, prob_grid, delta = delta[1], larger = larger)
      plotDF$RoE[plotDF$Prob <= i] <- i
    }
    
    plotDF$xFormat <- factor(x = plotDF$RoE,
                             levels = alpha[order(alpha, decreasing = TRUE)],
                             labels = paste0("alpha == ",
                                             signif(alpha[order(alpha, decreasing = TRUE)], 3)))
  }else{
    stop(paste("argument type =",type,"is unknown"))
  }
  
  
  ## plot RoE
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
    ggplot2::geom_raster(ggplot2::aes_string(x = "sePrior",
                                             y = "mu",
                                             fill = "xFormat"),
                         alpha = cols_alpha,
                         na.rm = TRUE,
                         interpolate = TRUE,
                         show.legend = TRUE) + 
    ggplot2::coord_cartesian(ylim = meanLim, xlim = sdLim) +
    ggplot2::labs(fill = legendString) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top", panel.grid = ggplot2::element_blank(),
                   legend.text.align = 0)
  
  if(is.null(cols)){
    ROEplot <- ROEplot +
      ggplot2::scale_fill_viridis_d(labels = scales::label_parse(), na.translate = F) +
      ggplot2::scale_color_viridis_d(alpha = 1)
  }else{
    nregion <- length(levels(ROEplot$data$xFormat))
    cols <- colorRampPalette(colors = cols, alpha = FALSE)(nregion)
    names(cols) <- levels(ROEplot$data$xFormat)
    ROEplot <- ROEplot +
      ggplot2::scale_fill_manual(values = cols, labels = scales::label_parse(), na.translate = F) +
      ggplot2::scale_color_manual(values = cols)
  }
  
  
  ROEplot <- ROEplot +
    ggplot2::scale_y_continuous(name = bquote("Prior mean"),
                                sec.axis = ggplot2::sec_axis(trans = ~ ./ee,
                                                             name = bquote("Relative prior mean")),
                                expand = c(0, 0)) +
    ggplot2::scale_x_continuous(name = bquote("Prior standard deviation"),
                                sec.axis = ggplot2::sec_axis(trans = ~ ./se,
                                                             name = bquote("Relative prior standard deviation")),
                                expand = c(0, 0))
  
  out <- list(plot = ROEplot, data = plotDF, meanFun = NULL)
  class(out) <- "bayesROE"
  return(out)
}

ribbonROE <- function(ee, se, delta = 0, alpha = 0.025,
                      type="threshold", larger = TRUE,
                      meanLim = c(pmin(2*ee, 0), pmax(0, 2*ee)),
                      sdLim = c(0, 3*se), nGrid = 500, relative = TRUE,
                      cols = NULL, cols_alpha = 1, addRef = TRUE) {

  ## define relative variance parameter grid
  gSeq <- seq(sdLim[1]/se, sdLim[2]/se, length.out = nGrid)^2
  
  ## define tipping point function for prior mean
  asign <- ifelse(larger, 1, -1)
  muTP <- function(g, delta, za) {
    mu <- asign*za*se*sqrt(g*(1 + g)) - ee*g + delta*(1 + g)
    return(mu)
  }
  
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
  
  ## plot RoE
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
    ref <- with(plotDF[plotDF$alpha == alpha[1] & plotDF$delta == delta[1],], approx(mu, sePrior, xout = 0))
    ROEplot <- ROEplot + 
      ggplot2::geom_vline(xintercept = ref$y, lty = 2, lwd = 0.5) + 
      ggplot2::geom_hline(yintercept = ref$x, lty = 2, lwd = 0.5) +
      ggplot2::annotate(geom = "text", x = ref$y, y = meanLim[1], label = paste(round(ref$y,2)), hjust = -0.5 )
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