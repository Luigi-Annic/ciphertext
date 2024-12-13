#' @title trancipher_wordkey
#'
#' @description transposition ciphertext with a word as key
#'
#' @param word Word or phrase to be encrypted
#' @param key numeric key
#' 
#' @return a string
#' @export
#'
#' @examples
#' trancipher_wordkey("This is wikipedia", "cipher")
#'

trancipher_wordkey <- function(word, key) {
  
  w0 <- PrepCyp.w(word)
  
  k2 <- unique(unlist(strsplit(key,"")))
  k3 <- numeric(length(k2))
  
  for  (i in (1:length(k2))) {
    k3[i] <- which(k2[i] == sort(k2))
  }
  
  mat <- matrix("",nrow = length(k2), ncol = ceiling(length(w0)/length(k2)))
  mat[1:length(w0)] <- w0
  mat <- t(mat)
  
  out <- c()
  for (i in (1:length(k3))) {
    j <- which(i == k3)
    out <- c(out, paste(mat[,j]))
  }
  
  final <- paste(out, collapse = "")
  return(final)
}