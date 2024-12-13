#' @title PrepCyp.w
#'
#' @description Internal function for preparing the input word (enforcing lowercase, ASCII standard, and no blanks)
#'
#' @param word Word or phrase to be preprocessed
#' 
#' @return a string
#' @export
#'
#' @examples
#' PrepCyp.w("HèllO Wòrld")
#' 

PrepCyp.w <- function(word) {
  w0a <- gsub(" ", "", word)             # Remove blanks 
  w0b <- tolower(w0a)                    # Everything lowercase
  w0c <- iconv(w0b,to="ASCII//TRANSLIT") # Remove strange characters and accents
  w0 <- unlist(strsplit(w0c, ""))        # split in letters
  return(w0)
}
