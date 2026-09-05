setCores <-
function(n) {
  # Set number of CPU cores which will be used by the package
  #
  #     n number of CPU cores

  if (!missing(n)) {
    if (is.numeric(n)) {
      n <- as.integer(ceiling(n))
      n <- .C('setNumSlaves', n = as.integer(n), PACKAGE = "spMC")$n
    }
  }
  n <- 0L
  crTot <- 0L
  n <- .C('getNumSlaves', n = as.integer(n), PACKAGE = "spMC")$n
  if (n == 1L) {
    if (.Call("isOmp", PACKAGE = "spMC")) packageStartupMessage("Parallel computation will not perform. CPU cores in use: 1.")
  }
  else if (n > 1L){
    crTot <- .C('getNumCores', n = as.integer(crTot), PACKAGE = "spMC")$n
    packageStartupMessage("Parallel computation will perform.")
    packageStartupMessage("  Total CPU cores available: ", crTot, ".", sep = "")
    packageStartupMessage("  CPU cores in use: ", n, ".", sep = "")
  }
  invisible(n)
}
