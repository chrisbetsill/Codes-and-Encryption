#' Addition in mod p Elliptic Curves
#'
#' Adds two points on the mod p elliptic curve y^2 = x^3 + bx + c. Uses the Addition Law
#' given on page 352 of [Trappe]. All points on elliptic curves are represented as length
#' two bigz vectors: the point at infinity is represented as \code{c(NA,NA)}. While \code{p1}
#' and \code{p2} are assumed to be valid points on the elliptic curve (not checked), the program
#' should return a warning (using the \code{warning} command) if any of the required inverses
#' in the addition rule fail to exist, returning the value that failed to be invertible.
#'
#' @param b A bigz or integer representing the coefficient b in the equation of the curve.
#' @param c A bigz or integer representing the coefficient c in the equation of the curve.
#' @param modulus The modulus of the curve (integer or bigz).
#' @param p1 A length 2 integer or bigz vector, representing a point on the curve.
#' @param p2 A length 2 integer or bigz vector, representing a point on the curve.
#'
#' @return A length 2 bigz vector, representing the sum p1 + p2.
#' @export
#' @import gmp
#'
#' @examples
#' ecAddModp(4, 4, 5, c(1,2), c(4,3)) # see page 353
#' ecAddModp(4, 4, 2773, c(1,3), c(1,3)) # see pp. 353-4
#' ecAddModp(4, 4, 2773, c(1771,705), c(1,3)) # see pp. 356-7
#' # Should return a warning message (or two messages):
#' # Warning messages:
#' # 1: In ecAddModp(4, 4, 2773, c(1771, 705), c(1, 3)) :
#' #   -1770 is not invertible mod 2773
#' # 2: In inv.bigz((x2 - x1), modulus) :
#' #   inv(x,m) returning NA as x has no inverse modulo m
ecAddModp <- function(b, c, modulus, p1=c(NA,NA), p2=c(NA,NA)) {
  if(all(is.na(p1)))
    return(p2)
  if(all(is.na(p2)))
    return(p1)
  if(all(p1 == p2)){
    if(p1[2] == 0)
      return(c(NA, NA))
    if(gcd(mul.bigz(p1[2], 2), modulus) != 1){
      warning(mul.bigz(p1[2], 2), " is not invertible mod ", modulus)
    }
    denom <- inv.bigz(mul.bigz(p1[2], 2), modulus)
    m <- mod.bigz(mul.bigz(add.bigz(mul.bigz(3, powm(p1[1], 2, modulus)), b),denom), modulus)
  }
  else{
    if(gcd(sub.bigz(p2[1], p1[1]), modulus) != 1){
      warning(p2[1]-p1[1], " is not invertible mod ", modulus)
    }
    num <- mod.bigz(sub.bigz(p2[2], p1[2]), modulus)
    denom <- mod.bigz(sub.bigz(p2[1], p1[1]), modulus)
    m <- mod.bigz(mul.bigz(num, inv.bigz(denom, modulus)), modulus)
  }
  x3 <- mod.bigz(sub.bigz(sub.bigz(powm(m, 2, modulus), p1[1]), p2[1]), modulus) 
  y3 <- mod.bigz(sub.bigz(mul.bigz(m, sub.bigz(p1[1], x3)), p1[2]), modulus) 
  return(c(x3,y3))
}


#' Efficient "exponentiation" in mod p elliptic curves
#'
#' Given a point \code{P} on an elliptic curve mod \code{modulus} and an integer/bigz \code{n},
#' computes the "power" nP. (Since elliptic curves form an additive group, we express "powers"
#' as integer multiples.) Does so using repeated "squaring" (doubling). (See the QPower function
#' in the notes.)
#'
#' @param b A bigz or integer representing the coefficient b in the equation of the curve.
#' @param c A bigz or integer representing the coefficient c in the equation of the curve.
#' @param modulus The modulus of the curve (bigz or integer).
#' @param P A length 2 integer or bigz vector, representing a point on the curve.
#' @param n A bigz or integer representing the "exponent".
#'
#' @return A length 2 bigz vector, representing the "power" nP.
#' @export
#' @import gmp
#'
#' @examples
#' ecPowModp(3, 45, 8831, c(4,11), 8) # see p. 364 (this is kG)
#' library(gmp)
#' ecPowModp(3, 45, 8831, c(4,11), as.bigz("2349089023472938409283490823")) # 6863 449
ecPowModp <- function(b, c, modulus, P, n) {
  if(n < 0){
    P <- ecNeg(P)
    n <- abs(n)
  }
  if(n == 1)
    return(P)
  if(n %% 2 == 0){
    return(ecPowModp(b, c, modulus, ecAddModp(b, c, modulus, P, P), n%/%2))
  }
  else{
    return(ecAddModp(b, c, modulus, P, ecPowModp(b, c, modulus, ecAddModp(b, c, modulus, P, P), n%/%2)))
  }
}