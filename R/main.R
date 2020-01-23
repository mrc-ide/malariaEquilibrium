#------------------------------------------------
#' @title Square a vector of values
#'
#' @description Simple test function that demonstrates some of the features of
#'   this package by squaring an input vector of values.
#'
#' @param x vector of values.
#'
#' @export
#' @examples
#' # Find square of first 100 values
#' square(1:100)

square <- function(x = 1:5) {

  # print message to console
  message("running R square function")

  # do something
  ret <- x^2

  # return
  return(ret)
}
