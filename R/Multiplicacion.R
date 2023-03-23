
#' Multiplication function
#'
#' @param x numeric vector
#' @param y numeric vector
#'
#' @return A number or numeric vector of the same length as x
#' @export
#'
#' @examples
#' multiply(2,3)
#' multiply(c(2,3),c(10,20))
#'
#'
multiply<- function (x = NULL, y = NULL) {x*y}



#' Multiplication function with 10 by default
#'
#' @param x numeric vector
#' @param y numeric vector, by default is 10
#'
#' @return A number or numeric vector of the same length as x
#' @export
#'
#' @examples
#'
#' multiply_by(2,3)
#' multiply_by(3)
#' multiply_by(c(2,3),c(10))
#'
multiply_by<- function (x = NULL, y = 10) {x*y}
