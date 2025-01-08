#' @title bifid_delastelle
#'
#' @description The bifid cipher is an encryption method that combines a substitution with a Polybius square and a transposition, and uses fractionation to achieve diffusion. 
#' It was invented by Felix Delastelle.
#' 
#' @param input Word or phrase to be encrypted,
#'  or character vector with the sequence of coordinate numbers if we need to decrypt
#' @param key key Word for creating the modified Polybius square
#' @param period period length for splitting the input phrase. If greater or equal to the length of the input then the split is not executed
#' 
#' @return a string
#' @export
#' 
#' @examples
#' bifid_delastelle("dcode", key = "secret", period = 3)
#' 
#' @references https://en.wikipedia.org/wiki/Bifid_cipher
#'

bifid_delastelle <- function(input, key = "", period = 100) {
  
w0a <- PrepCyp.w(input)
w0 <- ifelse(w0a == "j", "i", w0a) #transform j in i in input word

# allow for block periods (set period >= length(w0) to avoid creating blocks)
splitw0 <- c()
for (k in seq(1, length(w0), by = period)) {
  split<- w0[k:(k+period-1)]
  splitw0 <- rbind(splitw0, split)
}

# Exclude j from both letter and alphabet
key0j <- PrepCyp.w(key)
key0 <- ifelse(key0j == "j", "i", key0j)
letters0 <- subset(letters, letters != "j")

# create square
shuffledletters <- unique(c(key0, letters0))
matpol <- matrix(shuffledletters, nrow = 5, byrow = TRUE)

allblocks <- c()
for (w in c(1:nrow(splitw0))) {
  
w0 <- splitw0[w,]
    
coordinit <- c()

for (i in 1:length(w0)) {
  coordinit_temp <- which(w0[i] == matpol, arr.ind = T)
  coordinit <- rbind(coordinit, coordinit_temp)
}

coordfin <- as.data.frame(matrix(as.numeric(coordinit), ncol = 2, byrow = TRUE))
out<- c()

for (i in 1:nrow(coordfin)) {
 out_temp<- matpol[coordfin$V1[i],coordfin$V2[i]]
 out <- c(out, out_temp)
}

allblocks[w] <- paste(out, collapse = "")

}

paste(allblocks, collapse = "")

}