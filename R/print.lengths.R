print.lengths <-
function(x, ...) {
  cat("Direction (")
  cat(x$direction, sep = ", ")
  cat(")\n")
  lvl <- levels(x$categories)
  last <- rev(lvl)[1]
  for (i in lvl) {  
    cat("Stratum lengths of category \"", i, "\"\n", sep = "")
    print(x$length[i == x$categories], ...)
    if (i != last) cat("\n")
  }
}

