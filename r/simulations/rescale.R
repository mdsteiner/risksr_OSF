rescale <- function(x, from, to) {
  maxx <- max(x)
  minx <- min(x)
  out <- (to - from) * (x - minx)
  out <- out / (maxx - minx)
  out + from
}
