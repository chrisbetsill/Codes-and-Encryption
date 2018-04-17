#' Convert string to big integer
#'
#' Converts a string to a bigz integer. First the characters of the string are converted to raw, then the
#' raw (hexadecimal) vector is converted to an integer, where the place values of this vector are assigned
#' from right to left.
#'
#' @param txt A character string
#'
#' @return A `bigz` integer
#' @export
#' @import gmp
#'
#' @examples
#' stringToBigz("A string, which may contain punctuation.")
stringToBigz <- function(txt) {
  nraw <- charToRaw(txt)
  l <- length(nraw)
  return(sum(as.bigz(256)^(l-(1:l))*as.numeric(nraw)))
}

#' Convert (some) big integers to strings
#'
#' This function is intended to serve as an inverse to the \code{\link{stringToBigz}} function.
#' If the raw representation of `n` contains 00's, this function will produce an
#' `embedded nul in string` error. Thus it is not suitable for all integers.
#'
#' @param n A `bigz` integer, whose raw representation must not contain 00's
#'
#' @return A character string
#' @export
#' @import gmp
#'
#' @examples
#' stringAsInt <- stringToBigz("Any string.")
#' bigzToString(stringAsInt)
#' # bigzToString(256^3+256) # embedded nul error!
bigzToString <- function(n) {
  numbytes <- ceiling(log2.bigz(n)/8)
  nnumeric <- numeric(numbytes)
  for(i in 0:(numbytes-1)) {
    b <- as.numeric(mod.bigz(n, 256))
    n <- divq.bigz(n, 256)
    nnumeric[numbytes-i] <- b
  }
  return(rawToChar(as.raw(nnumeric)))
}
#' @export
bigPrime <- as.bigz("982451653")

#' @export
bigNotPrime<- as.bigz("2759906923170465870349284572513444495119639631")
#' This tests to see if a given number (n) is a psedoprime for the given base (a).
#' @param n The number you are testing.
#' @param a The base for the Fermat Test
#' @return TRUE, if the number is a psedoprime for the given base, else FALSE
#' @examples
#' fermatTest(bigPrime, 17)
#' fermatTest(bigNotPrime, 20)
#' fermatTest(bigPrime, bigNotPrime)
#' @export
fermatTest <- function(n, a){
  if(a > n){
    print("Error, a must be smaller than n");
    return(FALSE)
  }
  if(powm(a,(n-1),n) == 1){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}
