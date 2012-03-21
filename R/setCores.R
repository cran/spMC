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
  if (n <= 1L) {
    cat("Parallel computation will not perform. CPU cores in use: 1.\n")
  }
  else {
    crTot <- .C('getNumCores', n = as.integer(crTot), DUP = FALSE, PACKAGE = "spMC")$n
    cat("Parallel computation will perform.\n")
    cat("  Total CPU cores available: ", crTot, ".\n", sep = "")
    cat("  CPU cores in use: ", n, ".\n", sep = "")
  }
}
