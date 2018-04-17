#' Affine Cipher
#'
#'  Encrypt a string using an affine cipher
#'
#' @param plainText A string of lowercase letters
#' @param alpha An integer (mod 26) to scale by
#' @param beta An integer (mod 26) to shift by
#'
#' @return An encrypted (or decrypted) string, where each letter has been scaled by \code{alpha}, and shifted by \code{beta} places.
#' @examples affineCipher("examplemessage", 9, 7)
#' @export
affineCipher <- function(plainText, alpha, beta){
  pt <- stringToMod26(plainText)
  ct <- (pt*alpha + beta) %% 26  # encrypt by scaling and shifting
  return(mod26ToString(ct))
}
#' @export
tryAll <- function(){
  for (i in 1:26){
    for (j in 1:26){
      print(affineCipher("oinsnhompeelofmmpybphifopolkplyhlvhlbn",i, j))
    }
  }
}

#' @export
findKey <- function(plaintext, ciphertext){
  for (i in 1:26){
    for (j in 1:26){
      if(affineCipher(plaintext, i, j) == ciphertext){
        print(i)
        print(j)
      }
    }
  }
}

