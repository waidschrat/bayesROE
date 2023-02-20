#' @title Shiny Application to Visualize Bayesian Regions of Evidence
#'
#' @description Initialize and execute a local Shiny session to 
#'     interactively visualize and explore the Bayesian Regions of Evidence.
#'     Parameters entries from the sidebar are passed to the bayesROE function.
#'     
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @param init Named list containing the arguments that are passed to the bayesROE 
#'     function: ee, se, delta, alpha.
#' @param cols Named list of RGB hexadecimal color keys.
#' @inheritParams shiny::shinyApp
#' 
#' @examples
#' # reproducing Figure 3 from Hoefler and Miller (2023)
#' init <- list(ee = 9, se = 3.9, delta = c(0, 3.75), alpha = 0.025)
#' cols <- list(col_lower = "#F5FF82", col_upper = "#27CC1E")
#' if(interactive()){
#'    run_app(init = init, cols = cols)
#' }
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  onStart = NULL,
  options = list(
    launch.browser = TRUE
  ),
  enableBookmarking = NULL,
  uiPattern = "/",
  init = NULL,
  cols = NULL,
  ...) {
  
  #set default golem_opts and update based on user input
  inits <- list(ee = 6, se = 3.9, delta = c(0,-1,1), alpha = c(0.025,0.05,0.01)) #Default Parameter Set
  if(!is.null(init)) inits[match.arg(names(init),names(inits), several.ok = TRUE)] <- init
  ref_cols <- list(col_lower = "#F5FF82", col_upper = "#27CC1E", col_rope = "#FF0000", col_conflict = "#ABA545") #Default Col Palette
  if(!is.null(cols)) ref_cols[match.arg(names(cols),names(ref_cols), several.ok = TRUE)] <- cols
  
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(inits = inits, ref_cols = ref_cols, ...)
  )
}
