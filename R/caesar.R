#' @title caesar
#'
#' @description caesar encryption
#'
#' @param word Word or phrase to be encrypted
#' @param key numeric key
#' @param mode One of "decrypt" or "encrypt". Sets whether the program encrypts or decrypts the input word
#' 
#' @return a string
#' @export
#'
#' @examples
#' caesar("Hello", 1, "encrypt")
#'

caesar <- function(word, key, mode = "encrypt") {
  
  w0 <- PrepCyp.w(word)
  
  out <- character(length(w0))
  for (i in (1: length(w0))) {
    pos <- which(w0[i] == letters)
    newpos<- pos+ ifelse(mode == "encrypt" , key,
                         ifelse(mode == "decrypt", -key, "No mode"))
    finalpos0 <- ifelse(newpos>length(letters), newpos - length(letters), newpos) # correcting values >26
    finalpos <- ifelse(finalpos0 < 1, finalpos0+length(letters), finalpos0)       # correcting values <0
    out[i] = letters[finalpos]
  }
  
  final <- paste(out, collapse = "")
  return(final)
}

  