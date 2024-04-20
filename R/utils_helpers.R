#' Print method for bayesROE object
#' 
#' @description Print bayesROE
#' 
#' @return Return ggplot2 object
#' 
#' @noRd
print.bayesROE <- function(x, ...) {
  print(x$plot)
  invisible(x)
}


#' Shiny Application to Visualize Bayesian Regions of Evidence (deprecated interface)
#'
#' @description Initialize and execute a local Shiny session to 
#'     interactively visualize and explore the Bayesian Regions of Evidence.
#'     Parameters entries from the sidebar are passed to the bayesROE function.
#'     The function has been deprecated in favor of bayesROE::run_app() and is
#'     only retained for downward compatibility.
#'     
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @param init Named list containing the arguments that are passed to the bayesROE 
#'     function: ee, se, delta, alpha.
#' 
#' @examples
#' # reproducing Figure 3 from Hoefler and Miller (2023)
#' init <- list(ee = 9, se = 3.9, delta = c(0, 3.75), alpha = 0.025)
#' if(interactive()){
#'    shinyROE(init = init)
#' }
#' 
#' @export
shinyROE <- function(init = NULL, ...) {
  warning("Function deprecated. Please use bayesROE::run_app()")
  run_app(init = init, ...)
}
