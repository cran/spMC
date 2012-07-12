mlen <-
function(data, coords, loc.id, direction, mle = FALSE) {
  # Empirical estimation mean-lengths (for embeded data)
  #
  #      data vector of data or 
  #    coords matrix of coordinates
  #    loc.id location Id (which.lines output)
  # direction vector (or versor) of choosen direction
  #       mle logical value, if TRUE the MLEs will be returned (log-normal distro assumed)
  
  if (mle) {
    gl <- getlen(data, coords, loc.id, direction, TRUE)
    nk <- nlevels(data)
    if (length(data) < nk) stop("there are not enough data to estimate the parameters")
    param <- vector("numeric", 2 * nk)
    gl$categories <- as.integer(gl$categories)

    NegLik <- function(param) {
      pus <- plnorm(gl$length + gl$maxcens, meanlog = param[gl$categories],
                    sdlog = exp(param[gl$categories + nk]))
      pls <- plnorm(gl$length, meanlog = param[gl$categories],
                    sdlog = exp(param[gl$categories + nk]))
      return(- sum(log(abs(pus - pls))))
    }

    res <- nlminb(param, NegLik, lower = -Inf, upper = Inf)
    message("Optimization message: ", res$message, sep = "")
    meanlen <- exp(res$par[1:nk] + 0.5 * exp(2 * res$par[(nk + 1):(2 * nk)]))
  }
  else {
    if (!is.matrix(coords)) coords <- as.matrix(coords)
    nc <- dim(coords)[2]
    if (length(direction) != nc) stop("wrong length of direction vector")
    if (!is.factor(data)) data <- as.factor(data)
    nk <- nlevels(data)
    n <- length(data)
    if (n < nk) stop("there are not enough data to estimate the parameters")
    if (n != dim(coords)[1]) stop("the number of data is not equal to the number of coordinates")
    if (n != length(loc.id)) stop("length of \"loc.id\" must be equal to the data length")
    storage.mode(coords) <- "double"
    storage.mode(loc.id) <- "integer"

    ord <- order(abs(direction), decreasing = TRUE)
    ord <- cbind(loc.id, coords[, ord])
    ord <- lapply(apply(ord, 2, list), unlist)
    ord <- do.call("order", ord)
    data <- data[ord]
    loc.id <- loc.id[ord]
    coords <- coords[ord, ]

    meanlen <- .C('cEmbedOc', n = as.integer(n), nc = as.integer(nc), nk = as.integer(nk),
                  coords = as.double(coords), locId = as.integer(loc.id),
                  data = as.integer(data), emoc = as.integer(vector("integer", nk)),
                  tlen = as.double(vector("numeric", nk)), DUP = FALSE, PACKAGE = "spMC")$tlen
  }
  names(meanlen) <- levels(data)
  return(meanlen)
}
