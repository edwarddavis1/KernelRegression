#' Linear Kernel Function
#' @params x_i ith row of a matrix 
#' @params x_j jth row of a matrix
linear <- function(x_i, x_j) t(x_i) %*% x_j

#' Polynomial Kernel Function
#' @params x_i ith row of a matrix 
#' @params x_j jth row of a matrix
#' @params b Degree of polynomial
poly <- function(x_i, x_j, b) (t(x_i) %*% x_j + 1)^b

#' Radial Basis Kernel Function
#' @params x_i ith row of a matrix 
#' @params x_j jth row of a matrix
#' @params sigma RBF parameter
RBF <- function(x_i, x_j, sigma) exp(-(norm(x_i - x_j)^2)/(2*sigma^2))