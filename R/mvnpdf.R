#' Density of a multivariate normal distribution
#'
#' We want to compute the value of the density of a
#' multivariate normal distribution on R p at n points.
#' Our function must be applicable for any multivariate
#' normal distribution (i.e. any mean vector in R p and
#' variance-covariance matrix of any order of p ), and
#' we wish to compute all the values of the density evaluated
#' at the n points in a single call of the function.
#'
#' @param x a matrix, with \code{n} columns \code{the observations} and
#' \code{p} rows
#' @param mean a vector of means
#' @param varcovM a variance-covariance matrix
#' @param Log a logical parameter, with default value to \code{TRUE}.
#'
#' @return a list containing the input matrix x and y the multivariate-normal
#'
#' @export
#'
#' @examples
#'
#' if(interactive()){
#' k <- 50
#' p <- 50
#' varcovM <- matrix(NA, p, k)
#' x <- matrix(NA, p, k)
#' for(k in 1:k) {
#' varcovM[,k] <- abs(rnorm(p,.5,1))
#' x[,k] <- rnorm(p,.5,1)
#' }
#' mean <- as.array(rnorm(ncol(varcovM), .5, 1))
#' mvnpdf(x = x,mean = mean, varcovM = varcovM, Log=FALSE)}
#'


mvnpdf <- function(x, mean = rep(0,nrow(x)),
                   varcovM = diag(nrow(x)),
                   Log=TRUE){

  stopifnot(is.matrix(x))

  n <- ncol(x)
  p <- nrow(x)
  x0 <- x-mean
  Rinv <- solve(varcovM)
  logDetvarcovM <- log(det(varcovM))

  results <- rep(NA, n)
  for (j in 1:n){
    resf <- -p/2*log(2*pi)-.5*logDetvarcovM-
      .5*t(x0[,j]) %*% Rinv %*% x0[,j]
    results[j] <- resf
  }
  if(!Log){
    results <- exp(results)
  }
  res <- list(x = x, y = results)
  class(res) <- "mvnpdf"
  return(res)
}

