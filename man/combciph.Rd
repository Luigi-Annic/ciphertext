% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/combciph.R
\name{combciph}
\alias{combciph}
\title{combciph}
\usage{
combciph(word, funcs, keys = rep(NA, length(funcs)))
}
\arguments{
\item{word}{Word or phrase to be encrypted or decrypted}

\item{funcs}{Vector with the functions that are to be used for the sequential cipher.
At the moment, only ciphers with a single key or no key are allowed (i.e., ciphers like
affine which need 2 keys cannot be used)}

\item{keys}{Single key to be used for each of the ciphers. Each key refers to the cipher in the
 same position in funcs argument. If no key needs to be added 
(or if you want to use the default value for key), use NA}
}
\value{
a string
}
\description{
Use combinations of the ciphers. Once you provide the initial input word,
  the program applies the first cipher to the initial word and saves a first encryption,
  which is used as input word for the second cipher. This process repeats until the last 
  cipher, and the final output is returned.
}
\examples{
combciph("hello", funcs = c(caesar, atbash, polybius), keys = c(3, NA, NA))

#' # which is equivalent to
polybius(atbash(caesar("hello", key = 3)))

}
