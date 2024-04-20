#' @title Bayesian Regions of Evidence Raster Plot
#'
#' @description Compute and visualize the Bayesian Regions of Evidence (Raster),
#'     that is, the set of normal priors for an effect size which - 
#'     when combined with the observed data - lead to a specified posterior
#'     probability for the effect size being more extreme than a specified
#'     minimally relevant effect size.
#'     
#' @param ee Effect estimate.
#' @param se Standard error of effect estimate.
#' @param delta Minimally relevant effect size. Defaults to zero. Can also be a
#'     vector of numerical values to representing different regions.
#' @param alpha Posterior probability that the effect size is less extreme than
#'     delta. Defaults to 0.025. Can also be a vector of numerical values 
#'     representing different regions.
#' @param type Character indicating if regions of evidence should be constructed 
#'     for a non-inferiority claim using the first element of delta and all 
#'     elements of alpha ("threshold"), for a non-inferiority claim using the 
#'     all elements of delta and the first element of alpha ("probability"), 
#'     for an equivalence claim using the first two elements of delta 
#'     and all elements of alpha ("equivalence"), or for a prior-data 
#'     conflict using only the first element of alpha ("conflict").
#'     Defaults to "threshold".
#' @param larger Logical indicating if effect size should be larger (TRUE) or
#'     smaller (FALSE) than delta. Ignored when type = "equivalence" or 
#'     type = "conflict". Defaults to TRUE.
#' @param meanLim Limits of prior mean axis.
#' @param sdLim Limits of prior standard deviation axis.
#' @param nGrid Resolution of grid points (on both axes). Defaults to 200.
#' @param cols Character containing the HEX color code of the upper and lower
#'     region of evidence, respectively. Defaults to NULL, which triggers
#'     automated color picking by calling ggplot2:scale_fill_viridis_d()
#' @param cols_alpha Numeric value indicating the relative opacity of any
#'     region of evidence (alpha channel). Defaults to 1 (no transparency).
#' @param add Logical indicating if a separate geom_raster layer should be 
#'     created that can be added to an existing plot (TRUE), or if an entire 
#'     regions of plot should be created (FALSE).
#'     Defaults to FALSE.
#'
#' @return A bayesROE object (a list containing the ggplot object, the data for
#'     the plot, and the empty tipping point function)
#'
#' @references Hoefler, M., Miller, R. (2022, April 04). Bayesian regions of evidence (for normal
#'  distributions). \doi{10.31234/osf.io/mg23h}
#'
#'
#' @examples
#' ## data with p < 0.025 for H0: delta < 0, but p > 0.025 for H0: delta < 0.3
#' d <- 0.4
#' d_se <- 0.1
#' delta <- c(0, 0.3)
#' \donttest{
#' rasterROE(ee = d, se = d_se, delta = delta, meanLim = c(-1, 1))
#' }
#'
#' ## reproducing Figure 3 from Hoefler & Miller (2023)
#' ee <- 9
#' se <- 3.9
#' delta <- c(0, 3.75)
#' \donttest{
#' rasterROE(ee = ee, se = se, delta = delta, alpha = 0.05)$plot +
#'   ggplot2::annotate(geom = "point", y = ee, x = se, shape = 4)
#'   ggplot2::coord_flip(xlim = c(0, 12), ylim = c(-5, 10))
#' }
#'
#' @export
#' @importFrom grDevices colorRampPalette
#' @importFrom stats pnorm
rasterROE <- function(ee, se, delta = 0, alpha = 0.025,
                      type="threshold", larger = TRUE,
                      meanLim = c(-3*abs(ee), 3*abs(ee)),
                      sdLim = c(0, 5*se), nGrid = 200,
                      cols = NULL, cols_alpha = 1, add=FALSE) {
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
    
    !xor(!is.null(cols), length(cols) == 2),
    !xor(!is.null(cols), is.character(cols)),
    
    length(cols_alpha) == 1,
    is.numeric(cols_alpha),
    is.finite(cols_alpha),
    0 <= cols_alpha, cols_alpha <= 1
  )
  
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
  plotDF <- data.frame(plotDF, 
                       t(posterior_grid(obs_pars = c(ee, se), 
                                        prior_mu = plotDF$mu, #prior_mu
                                        prior_sd = plotDF$sePrior) #prior_sd
                       )
  )
  names(plotDF)[grep("X",names(plotDF))] <- c("posterior_mu","posterior_sd")
  
  
  ## determine posterior probability and RoE
  if(grepl(type, "threshold")){
    
    for(i in delta[order(delta)]){
      plotDF$Prob <- apply(plotDF, 1, 
                           function(x) prob_grid(x[c("posterior_mu","posterior_sd")],
                                                 delta = i, larger = larger)
      )
      #plotDF$Prob <- apply(posteriorPars, 2, prob_grid, delta = i, larger = larger)
      plotDF$RoE[plotDF$Prob <= alpha[1]] <- i
    }
    
    plotDF$xFormat <- factor(x = plotDF$RoE,
                             levels = delta[order(delta)],
                             labels = paste0("Delta == ",
                                             signif(delta[order(delta)], 3), " "))
  }else if(grepl(type, "probability")){
    
    for(i in alpha[order(alpha, decreasing = TRUE)]){
      plotDF$Prob <- apply(plotDF, 1, 
                           function(x) prob_grid(x[c("posterior_mu","posterior_sd")],
                                                 delta = delta[1], larger = larger)
      )
      #plotDF$Prob <- apply(posteriorPars, 2, prob_grid, delta = delta[1], larger = larger)
      plotDF$RoE[plotDF$Prob <= i] <- i
    }
    
    plotDF$xFormat <- factor(x = plotDF$RoE,
                             levels = alpha[order(alpha, decreasing = TRUE)],
                             labels = paste0("alpha == ",
                                             signif(alpha[order(alpha, decreasing = TRUE)], 3)))
    
  }else if(grepl(type, "equivalence")){
    
    if(length(delta) != 2 | delta[1] == delta[2]) stop("type = 'equivalence' requires delta to contain exactly 2 non-identical values: lower and upper margin of equivalence")
    for(i in alpha[order(alpha, decreasing = TRUE)]){
      
      plotDF$Prob1 <- apply(plotDF, 1, 
                            function(x) prob_grid(x[c("posterior_mu","posterior_sd")],
                                                  delta = min(delta), larger = TRUE)
      )
      plotDF$Prob2 <- apply(plotDF, 1, 
                            function(x) prob_grid(x[c("posterior_mu","posterior_sd")],
                                                  delta = max(delta), larger = FALSE)
      )
      plotDF$Prob <- with(plotDF, pmin(Prob1,Prob2))*2
      plotDF$RoE[plotDF$Prob <= i] <- i
    }
    
    plotDF$xFormat <- factor(x = plotDF$RoE,
                             levels = alpha[order(alpha, decreasing = TRUE)],
                             labels = paste0("alpha == ",
                                             signif(alpha[order(alpha, decreasing = TRUE)], 3)))
    
  }else if(grepl(type, "conflict")){
    
    plotDF$Prob1 <- apply(plotDF, 1, 
                          function(x) prob_grid(c(ee, sqrt(se^2+x["sePrior"]^2)),
                                                delta = x["mu"], larger = TRUE)
    )
    plotDF$Prob2 <- apply(plotDF, 1, 
                          function(x) prob_grid(c(ee, sqrt(se^2+x["sePrior"]^2)),
                                                delta = x["mu"], larger = FALSE)
    )
    plotDF$Prob <- with(plotDF, pmin(Prob1,Prob2))*2
    
    plotDF$RoE[plotDF$Prob <= alpha[1]] <- alpha[1]*2
    plotDF$xFormat <- factor(x = plotDF$RoE,
                             levels = alpha[1],
                             labels = paste0("conflict"))
    
  }else{
    stop(paste("argument type =",type,"is unknown"))
  }
  
  if (!larger) {
    if(grepl(type, "threshold")){
      legendString <- bquote({"Pr(effect size" < Delta * "| data, prior)"} >=
                               .(signif(100*(1 - alpha[1]), 3)) * "%   ")
    }else{
      legendString <- bquote({"Pr(effect size" < .(signif(delta[1], 3)) * "| data, prior)"} >=
                               1 - alpha * "   ")
    }
  } else {
    if(grepl(type, "threshold")){
      legendString <- bquote({"Pr(effect size" > Delta * "| data, prior)"} >=
                               .(signif(100*(1 - alpha[1]), 3)) * "%   ")
    }else{
      legendString <- bquote({"Pr(effect size" > .(signif(delta[1], 3)) * "| data, prior)"} >=
                               1 - alpha * "   ")
    }
  }
  
  ## plot region(s) of evidence
  if(add){
    ROEplot <- ggplot2::geom_contour(
      mapping = ggplot2::aes_string(x = "sePrior", y = "mu", z = "Prob"),
      data = plotDF,
      show.legend = FALSE,
      na.rm = TRUE,
      col = "red", lty = 2,
      breaks= alpha[1]*2)
    
    # ROEplot <- geom_contour_filled(
    #   ggplot2::aes_string(x = "sePrior", y = "mu", z = "Prob", fill = "xFormat"),
    #   data = plotDF, alpha = 0.5,
    #   show.legend = FALSE)
    
  }else{
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
      ggplot2::theme(legend.position = "top",
                     panel.grid = ggplot2::element_blank(),
                     legend.text.align = 0)
    
    
    ROEplot <- ROEplot +
      ggplot2::scale_y_continuous(name = bquote("Prior mean"),
                                  sec.axis = ggplot2::sec_axis(trans = ~ ./ee,
                                                               name = bquote("Relative prior mean")),
                                  expand = c(0, 0)) +
      ggplot2::scale_x_continuous(name = bquote("Prior standard deviation"),
                                  sec.axis = ggplot2::sec_axis(trans = ~ ./se,
                                                               name = bquote("Relative prior standard deviation")),
                                  expand = c(0, 0))
    
    if(is.null(cols)){
      ROEplot <- ROEplot +
        ggplot2::scale_fill_viridis_d(labels = scales::label_parse(), 
                                      na.translate = FALSE, na.value = NA) +
        ggplot2::scale_color_viridis_d(alpha = 1)
    }else{
      nregion <- length(levels(ROEplot$data$xFormat))
      cols <- colorRampPalette(colors = cols, alpha = FALSE)(nregion)
      names(cols) <- levels(ROEplot$data$xFormat)
      ROEplot <- ROEplot +
        ggplot2::scale_fill_manual(values = cols, labels = scales::label_parse(), 
                                   na.translate = FALSE, na.value = NA) +
        ggplot2::scale_color_manual(values = cols)
    }
  }
  
  out <- list(plot = ROEplot, data = plotDF, meanFun = NULL)
  class(out) <- "bayesROE"
  return(out)
}
