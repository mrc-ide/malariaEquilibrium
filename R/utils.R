#------------------------------------------------
#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#------------------------------------------------
#' @title Load sytem file for this package
#'
#' @description Load and return file from within the inst folder of this
#'   package.
#'
#' @param name name of file
#'
#' @export

malariaEq_file <- function(name) {
  
  # load file from inst/extdata folder
  name_full <- system.file("extdata/", name, package = 'malariaEquilibrium', mustWork = TRUE)
  ret <- readRDS(name_full)
}

# -----------------------------------
#' @title Gaussian quadrature of normal density
#'
#' @description Return node values and weights from Gaussian quadrature of
#'   normal distribution with n nodes.
#'
#' @param n number of nodes
#'
#' @importFrom statmod gauss.quad.prob
#' @export

gq_normal <- function(n) {
  statmod::gauss.quad.prob(n, dist = "normal")
}

