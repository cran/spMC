setCores <-
function(n) {
  # Set number of CPU cores which will be used by the package
  #
  #     n number of CPU cores

  if (!missing(n)) {
    if (is.numeric(n)) {
      n <- as.integer(ceiling(n))
      n <- .C('setNumSlaves', n = as.integer(n), DUP = FALSE, PACKAGE = "spMC")$n
    }
  }
  n <- 0L
  crTot <- 0L
  n <- .C('getNumSlaves', n = as.integer(n), DUP = FALSE, PACKAGE = "spMC")$n
  if (n == 1L) {
    message("Parallel computation will not perform. CPU cores in use: 1.")
  }
  else if (n > 1L){
    crTot <- .C('getNumCores', n = as.integer(crTot), DUP = FALSE, PACKAGE = "spMC")$n
    message("Parallel computation will perform.")
    message("  Total CPU cores available: ", crTot, ".", sep = "")
    message("  CPU cores in use: ", n, ".", sep = "")
  }
  invisible(n)
}
