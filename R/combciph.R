#' @title combciph
#'
#' @description Use combinations of the ciphers. Once you provide the initial input word,
#'   the program applies the first cipher to the initial word and saves a first encryption,
#'   which is used as input word for the second cipher. This process repeats until the last 
#'   cipher, and the final output is returned.
#'
#' @param word Word or phrase to be encrypted or decrypted
#' @param funcs Vector with the functions that are to be used for the sequential cipher.
#'     At the moment, only ciphers with a single key or no key are allowed (i.e., ciphers like
#'     affine which need 2 keys cannot be used)
#' @param keys Single key to be used for each of the ciphers. Each key refers to the cipher in the
#'  same position in funcs argument. If no key needs to be added 
#' (or if you want to use the default value for key), use NA
#' 
#' 
#' @return a string
#' @export
#'
#' @examples
#' combciph("hello", funcs = c(caesar, atbash, polybius), keys = c(3, NA, NA))
#' 
#' #' # which is equivalent to
#' polybius(atbash(caesar("hello", key = 3)))
#' 


combciph <- function(word, funcs, keys = rep(NA, length(funcs))) {
  
  for (i in 1:length(funcs)) {
    
    input <- ifelse(i== 1, word, out)
    
    if (is.na(keys[i])) {
      out <- funcs[[i]](input)
    } else {
      out <- funcs[[i]](input, keys[i])
    }
    
  }
  
  out
}
