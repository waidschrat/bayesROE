#' @title Bayesian Regions of Evidence Ribbon Plot
#'
#' @description Compute and visualize the Bayesian Regions of Evidence (Ribbon),
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
#'     elements of alpha ("threshold") or for a non-inferiority claim using the 
#'     all elements of delta and the first element of alpha ("probability").
#'     Defaults to "threshold".
#' @param larger Logical indicating if effect size should be larger (TRUE) or
#'     smaller (FALSE) than delta. Defaults to TRUE.
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
#' @param cols_alpha Numeric value indicating the relative opacity of any
#'     region of evidence (alpha channel). Defaults to 1 (no transparency).
#' @param addRef Logical indicating if a reference cross representing the minimum 
#'     sceptical prior is added to the plot. If delta or alpha are vectors, only 
#'     their first element(s) will be processed. 
#'     Defaults to TRUE.
#' @param addEst Logical indicating if a point symbol representing the mean and 
#'     standard error of the effect estimate (ee, se) is added to the plot.
#'     Defaults to FALSE.
#'
#' @return A bayesROE object (a list containing the ggplot object, the data for
#'     the plot, and the tipping point function)
#'
#' @references Pawel, S., Matthews, R. and Held, L. (2021). Comment on
#'     "Bayesian additional evidence for decision making under small sample uncertainty".
#'     Manuscript submitted for publication. Code available at
#'     \url{https://osf.io/ymx92/}
#'
#'
#' @examples
#' ## data with p < 0.025 for H0: delta < 0, but p > 0.025 for H0: delta < 0.3
#' d <- 0.4
#' d_se <- 0.1
#' delta <- c(0, 0.3)
#' ribbonROE(ee = d, se = d_se, delta = delta, meanLim = c(-1, 1))
#'
#' ## reproducing Figure 1 from Hoefler & Miller (2023)
#' ee <- 3.07
#' se <- 1.19
#' ribbonROE(ee = ee, se = se, delta = c(0,3), alpha = 0.025, 
#'   cols = c("#F5FF82", "#27CC1E"))$plot + 
#'   ggplot2::annotate(geom = "point", y = ee, x = se, shape = 4) +
#'   ggplot2::coord_flip(ylim = c(-5, 15))
#'
#' @export
#' @importFrom grDevices colorRampPalette
#' @importFrom stats pnorm
ribbonROE <- function(ee, se, delta = 0, alpha = 0.025,
                      type="threshold", larger = TRUE,
                      meanLim = c(pmin(2*ee, 0), pmax(0, 2*ee)),
                      sdLim = c(0, 3*se), nGrid = 500, relative = TRUE,
                      cols = NULL, cols_alpha = 1, 
                      addRef = TRUE, addEst = FALSE) {
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
    !is.na(addRef),
    
    length(addEst) == 1,
    is.logical(addEst),
    !is.na(addEst)
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
                                             signif(delta[order(delta)], 3)," "))
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
  
  ## plot region(s) of evidence
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
    ref <- with(plotDF[plotDF$alpha == alpha[1] & plotDF$delta == delta[1],], 
                approx(x = sePrior, y = mu, n = nGrid*10))
    ref <- list("x"=with(ref, x[y > 0][which.max(x[y > 0])]), "y"=0)
    
    ROEplot <- ROEplot + 
      ggplot2::geom_hline(yintercept = ref$y, lty = 2, lwd = 0.5)
    
    if(length(ref$x) > 0){
      ROEplot <- ROEplot + 
        ggplot2::geom_vline(xintercept = ref$x, lty = 2, lwd = 0.5) +
        ggplot2::annotate(geom = "text", x = ref$x, y = ref$y, 
                          label = paste(round(ref$x,2)), 
                          hjust = -0.1, vjust = -0.1)
    }
  }
  if(addEst){
    ROEplot <- ROEplot +
      ggplot2::annotate(geom = "point", y = ee, x = se, shape = 4)
  }
  
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