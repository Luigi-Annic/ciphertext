PrepCyp.w <- function(word) {
  w0a <- gsub(" ", "", word)             # Remove blanks 
  w0b <- tolower(w0a)                    # Everything lowercase
  w0c <- iconv(w0b,to="ASCII//TRANSLIT") # Remove strange characters and accents
  w0 <- unlist(strsplit(w0c, ""))        # split in letters
  return(w0)
}

# caesar

word = "alpha tauri"
key = 2

w0 <- PrepCyp.w(word)

out <- character(length(w0))
for (i in (1: length(w0))) {
  pos <- which(w0[i] == letters)
  out[i] = letters[pos+key]
}

final <- paste(out, collapse = "")

# naive transpo
key = 3
word = "penitentiagite"

w0 <- PrepCyp.w(word)

mat <- matrix("",nrow = key, ncol = ceiling(length(w0)/key))
mat[1:length(w0)] <- w0

out <- c()
for (i in 1: key) {
  out <- c(out, paste(mat[i,]))
}

final <- paste(out, collapse = "")

# transpo with word as key
word = "this is wikipedia"
key = "cipher"

w0 <- PrepCyp.w(word)

k2 <- unlist(strsplit(key,""))
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

# Rail Fence Cypher

word = 'WE ARE DISCOVERED FLEE AT ONCE'
key = 3

w0 <- PrepCyp.w(word)

length(w0)

mat <- matrix("",nrow = length(w0), ncol = key)
mat <- t(mat)

j<-1
aim <- key
for (i in (1: length(w0))) {
  mat[j,i] = w0[i]
  aim <- ifelse(j == key, 1,
          ifelse(j==aim, key, aim))
  j<- ifelse(aim == key, j+1, j-1)
}
