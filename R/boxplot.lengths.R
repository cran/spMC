boxplot.lengths <-
function (x, ..., log = FALSE, zeros.rm = TRUE) {
  if (zeros.rm & x$zeros) {
    idx <- x$length != 0
    x$categories <- x$categories[idx]
    x$length <- x$length[idx]
  }
  if (log) x$length <- log(x$length)
  boxplot(x$length ~ x$categories, ...)
}

