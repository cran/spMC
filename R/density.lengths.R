density.lengths <-
function (x, ..., log = FALSE, zeros.rm = TRUE) {
  zrs <- x$zeros
  if (zeros.rm & x$zeros) {
    idx <- x$length != 0
    x$categories <- x$categories[idx]
    x$length <- x$length[idx]
    zrs <- FALSE
  }
  res <- tapply(x$length, x$categories, function (Lengths) {
                xx <- log(Lengths + .Machine$double.eps * zrs)
                from <- min(xx)
                to <- max(xx)
                res <- density(xx, ..., from = from, to = to)
                if (!log) {
                  res$x <- (exp(res$x) - .Machine$double.eps * zrs)
                  res$y <- res$y / res$x
                }
                return(res)})
  res$direction <- x$direction
  res$log <- log
  class(res) <- "density.lengths"
  return(res)
}

