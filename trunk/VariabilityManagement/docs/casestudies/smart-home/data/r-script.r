sqr <- function (x) {
  x*x
}

sigma2 <- function(v) {
 sum <- 0
 for (i in 1:length(v)) {
  sum <- sum + sqr ( (v[i] - mean(v)) )
 }
 sum / (length (v) -1)
}

sigma <- function (v) {
 sqrt (sigma2 (v))
}

pnormal <- function (n,s,y0)
 (y0-n)/s
 