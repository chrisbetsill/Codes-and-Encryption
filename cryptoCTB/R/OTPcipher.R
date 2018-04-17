#'Hill Cipher
#'
#'  Encode plaintext using the Hill cipher
#' @export
#'
#' @param txt the plaintext
#' @param keyMatrix the matrix containing the key used for encryption
#' @examples
#' hillCipher("thisisanexample",matrix(c(1,3, 4,2, 9,5),nrow=3,ncol=2))
#'
#' @return The encrypted text
#'
hillCipher <- function(txt, keyMatrix) {
  pt <- stringToMod26(txt)
  n <- attributes(keyMatrix)$dim[2]
  suppressWarnings(
    mPtxt <- matrix(pt,nrow=n) # repeats text so length is a multiple of n
  )
  mCtxt <- (keyMatrix %*% mPtxt) %% 26
  return(mod26ToString(as.vector(mCtxt)))
}

#' One-Time-Pad Cipher
#'
#'   Encrypt a message using a one time pad
#' @export
#'
#' @param txt The plaintext that will be encoded
#' @param padKey The key one time pad
#' @examples
#' oneTimePad("test", "abcd")
#'
#' @return The encrypted text
#'
oneTimePad <- function(txt, padKey){
  val <- rawToChar(as.raw(
    (as.integer(xor(charToRaw(txt), charToRaw(padKey))) %% 26)+97
  ))
  return (val)
}


#' @export
linRec <- function(){
  seedVec <-c(1,0,0,1,0,1,1)
  x <- numeric(100)
  x[1:7] <- seedVec
  for(i in 8:100){
    x[i] <- (x[i-7] + x[i-5] + x[i-3] + x[i-1]) %% 2
  }
  for(i in 8:100){
    testVec <- x[i:(i+6)]
    if(isTRUE(all.equal(testVec, seedVec))){
      print("Period is less than 100")
      break
    }
    else if(i == 100) print("Period is greater than 100")
  }
}



