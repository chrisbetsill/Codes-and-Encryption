#' Plot a real elliptic curve
#'
#' Creates a plot of the elliptic curve y^2 = x^3 + bx + c. Uses the \code{contour} plot
#' function.
#'
#' @param b A real number representing the coefficient b in the equation of the curve.
#' @param c A real number representing the coefficient c in the equation of the curve.
#' @param xmin Lower limit for x
#' @param xmax Upper limit for x
#' @param ymin Lower limit for y
#' @param ymax Upper limit for y
#' @param asp Aspect ratio
#'
#' @return A plot of the ellptic curve.
#' @export
#'
#' @examples
#' ecPlotReal(-5,1)
ecPlotReal <- function(b, c, xmin=-5, xmax=10, ymin=-30, ymax=30, asp=1) {
  x<-seq(xmin, xmax, length=1000)
  y<-seq(ymin, ymax, length=1000)
  z<-outer(x,y,function(x,y) -y^2 + x^3+b*x+c )
  contour(x,y,z,levels=0, labels="", labcex=0.1, asp=asp)
}

#' Addition in Real Elliptic Curves
#'
#' Adds two points on the real elliptic curve y^2 = x^3 + bx + c. Uses the Addition Law
#' given on page 352 of [Trappe]. All points on elliptic curves are represented as length
#' two numeric vectors: the point at NAinity is represented as \code{c(NA,NA)}. This function
#' does no error trapping, so \code{p1} and \code{p2} are assumed to be valid points on
#' the elliptic curve.
#'
#' @param b A real number representing the coefficient b in the equation of the curve.
#' @param c A real number representing the coefficient c in the equation of the curve.
#' @param p1 A length 2 numeric vector, representing a point on the curve.
#' @param p2 A length 2 numeric vector, representing a point on the curve.
#'
#' @return A length 2 numeric vector, representing the sum p1 + p2.
#' @export
#'
#' @examples
#' ecAddReal(0, 73, c(2,9), c(3,10)) # see page 350
#' ecAddReal(0, 73, c(2,9), c(NA,NA)) # c(NA,NA) is the additive identity
#' ecAddReal(0, 73, c(2,9), c(2,-9))
ecAddReal <- function(b, c, p1=c(NA,NA), p2=c(NA,NA)) {
  if(all(is.na(p1)))
    return(p2)
  if(all(is.na(p2)))
    return(p1)
  if(all(p1 == p2)){
    m <- (3*(p1[1]^2)+b)/(2*p1[2])
    if(p1[2] == 0){
      return(c(NA, NA))
    }
  }
  else{
    m <- (p2[2]-p1[2])/((p2[1]-p1[1]))
    if((p2[1]-p1[1] == 0))
       return(c(NA, NA))
  }
  x3 <- (m^2)-p1[1]-p2[1]
  y3 <- m*(p1[1]-x3)-p1[2]
  return(c(x3,y3))
}

#' Elliptic curve additive inverse
#'
#' Computes the additive inverse (negative) of a point on an elliptic curve. Note that
#' the output of this function does not depend on the choice of elliptic curve, or even
#' on the field.
#'
#' @param p A length 2 vector representing a point on an elliptic curve.
#'
#' @return A length 2 vector representing the additive inverse of \code{p}.
#' @export
#'
#' @examples
#' ecNeg(c(3,4)) # see page 351
ecNeg <- function(p){
  return(c(p[1], -p[2]))
}

#' Real elliptic curve "exponentiation"
#'
#' Give a point \code{p} on a real elliptic curve y^2 = x^3 + bx + c and an integer \code{n},
#' returns the point np, that is, p added to itself n times. If n is negative, returns -p added
#' to itself n times. If n is zero, returns the point at NAinity. If p is the point at NAinity, 
#' also returns the point at NAinity. We call this operation "exponentiation", but since we 
#' represent elliptic curves with additive notation, "exponentiation" is repeated addition.
#'
#' @param b A real number representing the coefficient b in the equation of the curve.
#' @param c A real number representing the coefficient c in the equation of the curve.
#' @param p A length 2 numeric vector, representing a point on the curve.
#' @param n An integer
#'
#' @return A length 2 vector representing \code{n} times \code{p} (on the elliptic curve).
#' @export
#'
#' @examples
#' ecPowReal(0, 73, c(-4, -3), 2) # See page 351
ecPowReal <- function(b, c, p, n) {
  if(n < 0)
    p <- ecNeg(p)
  if(n == 0)
    return(c(NA, NA))
  if(all(is.na(p)))
    return(c(NA, NA))
  if(n == 1)
    return(p)
  if(n == 2)
    return(ecAddReal(b, c, p, p))
  else{
    n <- abs(n)
    pc <- c(NA, NA)
    for(i in 1:n){
      ans <-  ecAddReal(b, c, pc, p)
      pc <- ans
    }
  }
  return(pc)
}
