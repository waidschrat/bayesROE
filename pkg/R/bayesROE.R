#' Print method for bayesROE object
#' @method print bayesROE
#' @param x A bayesROE object
#' @param ... Other arguments
#' @export
print.bayesROE <- function(x, ...) {
    print(x$plot)
    invisible(x)
}

#' Input Check for ROE functions
#' @param ... Arguments
#' @export
ArgCheck <- function(...) {
  
}