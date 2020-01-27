#------------------------------------------------
# overload print() function for model_params
#' @method print model_params
#' @export
print.model_params <- function(x, ...) {

  # print summary
  summary(x)

  # return invisibly
  invisible(x)
}

#------------------------------------------------
# overload summary() function for model_params
#' @method summary model_params
#' @export
summary.model_params <- function(object, ...) {

  # print as dataframe
  x_print <- as.data.frame(cbind(unclass(object)))
  names(x_print) <- "value"
  print(x_print)

}

#------------------------------------------------
# overload head() function for model_params
#' @method head model_params
#' @importFrom utils head
#' @export
head.model_params <- function(x, ...) {
  
  # print head as dataframe
  x_print <- as.data.frame(cbind(unclass(x)))
  names(x_print) <- "value"
  head(x_print)
  
}
