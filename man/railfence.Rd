% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/railfence.R
\name{railfence}
\alias{railfence}
\title{railfence}
\usage{
railfence(word, key = 3)
}
\arguments{
\item{word}{Word or phrase to be encrypted}

\item{key}{numeric key (number of rails)}
}
\value{
a string
}
\description{
The rail fence is a transposition cipher where the text is 
written upwards and downwards diagonally (zigzag) on the rails of the fence
}
\examples{
railfence('we are discovered flee at once',3)

}
\references{
https://en.wikipedia.org/wiki/Rail_fence_cipher
}
