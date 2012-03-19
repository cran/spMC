summary.lengths <-
function (object, ..., zeros.rm = TRUE) {
  if (zeros.rm & object$zeros) {
    idx <- object$length != 0
    object$categories <- object$categories[idx]
    object$length <- object$length[idx]
  }
  res <- tapply(object$length, object$categories, function(p) summary(p, ...))
  class(res) <- "summary.lengths"
  return(res)
}

