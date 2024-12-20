% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scytale.R
\name{scytale}
\alias{scytale}
\title{scytale}
\usage{
scytale(word, key, decrypt = FALSE)
}
\arguments{
\item{word}{Word or phrase to be encrypted or decrypted}

\item{key}{Number of turns of the band}

\item{decrypt}{If `FALSE` (default), the program ciphers the input word, If `TRUE`, the program decrypts it.}
}
\value{
a string
}
\description{
The Scytale is a transposition cipher
 The diameter of the Scytale (the number of turns) can be regarded as the key of the cipher.
}
\examples{
scytale('we are discovered flee at once',3)

}
\references{
https://en.wikipedia.org/wiki/Scytale
}
