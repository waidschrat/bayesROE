#' @title Shiny Application to Visualize Bayesian Regions of Evidence
#'
#' @description Initialize and execute a local Shiny session to 
#'     interactively visualize and explore the Bayesian Regions of Evidence.
#'     Parameters entries from the sidebar are passed to the bayesROE function.
#'
#' @param init. List containing the arguments that are passed to the bayesROE 
#'     function: ee, se, delta, alpha. 
#'
#' @return Launches the Shiny App.
#'
#' @references Höfler, M., Miller, R. (2022, April 04). Bayesian regions of evidence (for normal 
#'     distributions). \doi{10.31234/osf.io/mg23h}
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
  
  inits <<- list(ee = 6, se = 3.9, delta = c(0,-1,1), alpha = c(0.025,0.05,0.01))
  if(!is.null(init)){
    inits[match.arg(names(init),names(inits), several.ok = TRUE)] <<- init
  }
  
  shinyAppDir(appDir = "./ShinyROE/",
              options = list(
                launch.browser = TRUE
              ))
}
