#' Discrete Logarithm (brute force)
#'
#' Computes a discrete logarithm by brute force.
#'
#' @param alpha The base, assumed to be a primitive root modulo p.
#' @param beta An element of Z_p
#' @param p A prime modulus.
#'
#' @return Computes L_alpha(beta), the smallest nonnegative integer x such that beta = alpha^x, modulo p.

#' @import gmp
#'
#' @examples
#' discreteLogBrute(2,9,11) # should be 6

#' @export
discreteLogBrute <- function(alpha, beta, p) {
  i <- 1
  while(1){
    if(beta == powm(alpha,i,p)){
      return(i)
    }
    i<- i + 1
  }
}
#' Next Primitive Root
#'
#' Finds the next primitive root after \code{after} modulo \code{p}.
#'
#' @param p A prime number. Could be \code{bigz}.
#' @param after Number after which to start testing. Defaults to 1.
#'
#' @return The smallest primitive root modulo \code{p} greater than \code{after}. Applies
#' the procedure described in Exercise 21 on p. 107 of [Trappe].
#' @export
#' @import gmp
#'
#' @examples
#' nextPrimRoot(601) # See page 107, Problem 21
#' nextPrimRoot(nextprime(as.bigz("1203481092840918409408098")), 200) # should be 203
#' 
nextPrimRoot <- function(p, after = 1){
  vector <- factorize(p-1)
  uvec <- (p-1)/unique(vector)
  vec1 <- vector() 
  vec2 <- vector()
  i <- after+1
  while(1){
    for(j in 1:length(uvec)){
      vec1[j] <- i
    }
    vec2 <- powm(vec1, uvec, p)
    if(any(vec2 == 1) != TRUE){
      return(i)
    }
    i <- i + 1
  }
}

#' Solve a system using the Chinese Remainder Theorem
#'
#' Given a system of i congruences of the form x = a_i mod m_i, returns
#' the smallest positive x that satisifies all the congruences. Applies the
#' Chinese Remainder Theorem, following the procedure outlined
#' in [Trappe], p. 108, Problem 24. Works for bigz integers.
#'
#' @param a A vector of i integers (or bigz integers)
#' @param m A vector of i moduli (as integers or as bigz)
#'
#' @return A bigz integer solutions to the system.
#' @export
#' @import gmp
#'
#' @examples
#' crtSolve(c(2,1,3), c(5,6,7))
crtSolve <- function(a, m){
  M <- prod(m)
  zvec <- M/m
  yvec <- inv.bigz(zvec, m)
  newvec <- a*yvec*zvec
  return(sum(newvec)%%M)
}