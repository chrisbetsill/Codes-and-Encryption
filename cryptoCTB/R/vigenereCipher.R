

cipher <- "ytklmxlrjjljnfkddjelugyaezjxlryapjrxyaeluhycjyxywjlmyxxjlkaklxjzwufruwlgjdrjxjatjuwxlryapjrxyxlgjxlrjjlxuwxfttjxxwfztklmajkpgqurguuexyzbymxeuhfxlgyijlgrjjhykanfyzklkjxwkrxllgjrjhfxlqjytzjyrejhyrtylkuaqjlbjjabgylkxdfqzktxdytjyaebgylkxdrkiyljxdytjdfqzktyaedrkiyljxdytjxtyaauluusjkalujytgulgjryxlgjmeulmdktyzzmkaxfqfrqyaxjllkapxurkadruvjtlxxjtuaelgjrjhfxlqjjmjxfdualgjxlrjjljmjxqjzuapkaplulguxjbjhkpgltyzzlgjaylfryzdrudrkjlurxuwlgjxlrjjllgjqfkzekapxuayxlrjjljnfkddjelugyaezjxlryapjrxyaelukaxfrjlgjxywjlmuwqulgrjxkejalxyaexlryapjrxhfxlqjurkjaljelulgjxlrjjllgjmtyaaullfralgjkrqytcxurqzyacxkejxuaklyaezjyijklqzkaeyaelgkrelgjxkejbyzchfxlgyijfxjrxuaklwykrzmtualkafufxzmqulgluyeelulgjafhqjruwjwwjtlkijjmjxualgjxlrjjlyaelukaeftjlgjdjudzjkaqfkzekapxyzuaplgjxlrjjllubyltglgjxkejbyzcxkaxfwwktkjalafhqjrxauquemjavumxxkllkapuayxluudurzuuckapuflybkaeubylyajhdlmxlrjjlyzhuxlauquemeujxxftgylgkapzyrpjafhqjrxuwdjudzjjaljrlykalgjhxjzijxuwwyaeuaqmbyltgkapxlrjjlytlkiklm"
#' Get Letter Counts
#'
#' Gets a count of the frequencies of each letter
#' @param txt The text which you are analyzing
#' @examples letterCounts("thisisan@examplesforlettercounts")
#' @export
letterCounts <- function(txt)
{
  return(sort(table(unlist(strsplit(txt,""))),decreasing=TRUE))
}
#' Build Digram Table
#'
#' Builds a table displaying thee frequencies of digrams
#' @param txt The text which you are analyzing
#' @examples digramTable("thisisan@examplesforbuildingadigramtablealongertextwouldbemoreuseful")
#' @export
digramTable <- function(txt)
  # returns a table with the numbers of digrams of each possible type
{
  l <- unlist(strsplit(txt,""))
  dgs <- data.frame(l,c(l[2:length(l)],NA))
  names(dgs) <- c("first","second")
  table(dgs)
}
#' Vigenere Cipher
#'
#' Encrypt a message using a vigenere cipher
#'
#' @param txt The text which you are encrypting
#' @param keyVector A numerical vector used as the encryption key
#' @examples vigenere("thisistheplaintext", stringToMod26("thiswillbeavector"))
#' @export
vigenere <- function(txt, keyVector)
{
  pt <- stringToMod26(txt)
  suppressWarnings(
    ct <- (pt + keyVector) %% 26
  )
  return(mod26ToString(ct))
}
#' Decrypt Vigenere
#'
#' Decrypt a message encoded with a vigenere cipher with a known key
#'
#' @param txt The text which you are decrypting
#' @param keyVector A numerical vector used as the encryption key
#' @examples devigenere("moqkeaesftlvmpmsom", stringToMod26("thiswillbeavector"))
#' @export
devigenere <- function(txt, keyVector)
{
  pt <- stringToMod26(txt)
  suppressWarnings(
    ct <- (pt - keyVector) %% 26
  )
  return(mod26ToString(ct))
}

#' Find Relative Frequencies
#'
#' Compute the relative frequencies of the letters in a string of lowercase letters.
#'
#' @param txt The text which you are analyzing
#' @examples letterFreq("thisisan@examples")
#' @export
letterFreq <- function(txt) {
  l <- unlist(c(strsplit(txt,""),letters))
  t <- as.vector(table(l))-1
  return(t/sum(t))
}
#'  Shift Vector
#'  Shift a vector by n places
#' @param v The vec
#' @param n The shift

#' @examples shiftVec(c(0,1,2,3), 5)
#' @export
shiftVec <- function(v, n) {
  v[(seq_along(v) - (n+1)) %% length(v) + 1]
}
#' Skip String
#'
#' Given a string txt and integers n and r, returns a string consisting of the characters in positions n,n+r,n+2r,n+3r,…n,n+r,n+2r,n+3r,…, etc
#' @param txt The string that is being shifted
#' @param n The shift
#' @param r The scale
#' @examples skipString("thisisan@examples", 4, 2)
#' @export
skipString <- function(txt, n, r) {
  l <- unlist(strsplit(txt,""))
  ss <- l[seq(n,length(l),r)]
  return(paste0(ss,collapse=""))
}
#' Find Vigenere Key
#'
#' This finds the Vigenere key for a given ciphertext when the keylength is known.
#' @param ciphertext The encrypted text
#' @param keyLength The length of the ecryption key
#' @export
#'
findVigKey <- function(ciphertext, keyLength) {
  vKey <- numeric(keyLength) # preallocate a vector of the desired length
  englishFreqs <- c(0.082,0.015,0.028,0.043,0.127,0.022,0.020,0.061,0.070, 0.002,0.008,0.040,0.024,
                    0.067,0.075,0.019,0.001,0.060,0.063,0.091,0.028,0.010,0.023,0.001,0.020,0.001)
for(i in 1:keyLength){
  v <- letterFreq(skipString(ciphertext, keyLength))
  matchFreqs <- sapply(0:25, function(i){v %*% shiftVec(englishFreqs, i)})
  vKey[i] <- which.max(matchFreqs);
}
  return(vKey)
}
#' @export
test <- "comeonmanyouresmartyoumadepoisonoutofbeansyolookwegotwegotanentirelabrightherealrighthowaboutyoupicksomeofthesechemicalsandmixupsomerocketfuelthatwayyoucouldjustsendupasignalflareoryoumakesomekindofrobottogetushelporahomingdeviceorbuildanewbatteryorwaitnowhatifwejusttakesomestuffoffofthervandbuilditintosomethingcompletelydifferentyouknowlikealikeadunebuggythatwaywecanjustdunebuggyorwhatheywhatisitwhatcomeonmanyouresmartyoumadepoisonoutofbeansyolookwegotwegotanentirelabrightherealrighthowaboutyoupicksomeofthesechemicalsandmixupsomerocketfuelthatwayyoucouldjustsendupasignalflareoryoumakesomekindofrobottogetushelporahomingdeviceorbuildanewbatteryorwaitnowhatifwejusttakesomestuffoffofthervandbuilditintosomethingcompletelydifferentyouknowlikealikeadunebuggythatwaywecanjustdunebuggyorwhatheywhatisitwhatcomeonmanyouresmartyoumadepoisonoutofbeansyolookwegotwegotanentirelabrightherealrighthowaboutyoupicksomeofthesechemicalsandmixupsomerocketfuelthatwayyoucouldjustsendupasignalflareoryoumakesomekindofrobottogetushelporahomingdeviceorbuildanewbatteryorwaitnowhatifwejusttakesomestuffoffofthervandbuilditintosomethingcompletelydifferentyouknowlikealikeadunebuggythatwaywecanjustdunebuggyorwhatheywhatisitwhatcomeonmanyouresmartyoumadepoisonoutofbeansyolookwegotwegotanentirelabrightherealrighthowaboutyoupicksomeofthesechemicalsandmixupsomerocketfuelthatwayyoucouldjustsendupasignalflareoryoumakesomekindofrobottogetushelporahomingdeviceorbuildanewbatteryorwaitnowhatifwejusttakesomestuffoffofthervandbuilditintosomethingcompletelydifferentyouknowlikealikeadunebuggythatwaywecanjustdunebuggyorwhatheywhatisitwhatcomeonmanyouresmartyoumadepoisonoutofbeansyolookwegotwegotanentirelabrightherealrighthowaboutyoupicksomeofthesechemicalsandmixupsomerocketfuelthatwayyoucouldjustsendupasignalflareoryoumakesomekindofrobottogetushelporahomingdeviceorbuildanewbatteryorwaitnowhatifwejusttakesomestuffoffofthervandbuilditintosomethingcompletelydifferentyouknowlikealikeadunebuggythatwaywecanjustdunebuggyorwhatheywhatisitwhatcomeonmanyouresmartyoumadepoisonoutofbeansyolookwegotwegotanentirelabrightherealrighthowaboutyoupicksomeofthesechemicalsandmixupsomerocketfuelthatwayyoucouldjustsendupasignalflareoryoumakesomekindofrobottogetushelporahomingdeviceorbuildanewbatteryorwaitnowhatifwejusttakesomestuffoffofthervandbuilditintosomethingcompletelydifferentyouknowlikealikeadunebuggythatwaywecanjustdunebuggyorwhatheywhatisitwhat"
#'@export
findLength <- function(text){
ctv <- stringToMod26(text)
matches <- sapply(1:30, function(x){sum(ctv-shiftVec(ctv, x)==0)})
names(matches) <- 1:30
matches
}
#' @export
newtext <- vigenere(test, c(1,2))
#' @export
doubletext <- vigenere(newtext, c(1,2,3))
