transiogram <-
function(data, coords, direction, max.dist = Inf, mpoints = 20, tolerance = pi/8, reverse = FALSE) {
  # Empirical transition probabilities estimated by points
  #
  #       data vector of data
  #     coords coordinates matrix
  #  direction vector (or versor) of choosen direction
  #   max.dist maximum distance for counting
  #    mpoints number of lags
  #  tolerance angle tolerance (in radians)
  #    reverse logical, if TRUE compute probabilities also for the reversible chain

  if (!is.logical(reverse)) stop("\"reverse\" must be a logical value")
  reverse <- reverse[1]
  if (!is.numeric(max.dist) || max.dist < 0) stop("\"max.dist\" must be numeric and non negative")
  if (!is.factor(data)) data <- as.factor(data)
  if (!is.matrix(coords)) coords <- as.matrix(coords)
  if (!is.numeric(mpoints) || mpoints <= 0) stop("\"mpoints\" must be positive number")
  mpoints <- as.integer(ceiling(mpoints))
  nk <- nlevels(data) # number of categories
  n <- length(data)   # sample size
  if (n != dim(coords)[1]) stop("the number of data is not equal to the number of coordinates")
  nc <- dim(coords)[2]
  if (length(direction) != nc) stop("wrong length of direction vector")
  labels <- levels(data)

  storage.mode(coords) <- "double"
  storage.mode(direction) <- "double"

  # definition of bins through I(x <= vDeltaH)
  direction <- direction / sqrt(sum(direction^2))
  RNG <- apply(coords, 2, range)
  dr <- sqrt(sum(diff(RNG)^2))
  deltah <- min(dr, max.dist) / (mpoints * 2^reverse)
  vDeltaH <- cumsum(rep(deltah, mpoints))

  # count transition occurences
  Tcount <- .C('transCount', n = as.integer(n), data = as.integer(data), 
               nc = as.integer(nc), coords = as.double(coords),
               dire = as.double(direction), tolerance = as.double(tolerance), 
               mpoints = as.integer(mpoints), bins = as.double(vDeltaH), 
               nk = as.integer(nk), trans = as.double(vector("numeric", nk^2 * mpoints)),
               PACKAGE = "spMC")$trans
  Tcount <- array(Tcount, dim = c(nk, nk, mpoints))
  mtSum <- apply(Tcount, 3, sum)
  nonComputable <- mtSum == 0
  if (all(nonComputable)) stop("\"max.dist\" is lower than the minimum distance")
  # compute transition probabilities
  rwSum <- apply(Tcount, 3, function(x) apply(x, 1, sum))
  Tcount <- .C('transProbs', mpoints = as.integer(mpoints), nk = as.integer(nk), 
               rwsum = as.double(rwSum), empTR = as.double(Tcount),
               PACKAGE = "spMC")$empTR
  res <- list()
  res$Tmat <- array(Tcount, dim = c(nk, nk, mpoints))
  if (reverse) {
    revTprobs <- res$Tmat[, , !nonComputable]
    revTprobs <- .C('revtProbs', Tmat = as.double(revTprobs),
                    dim = as.integer(dim(revTprobs)), PACKAGE = "spMC")$Tmat
    res$Tmat <- array(c(revTprobs, diag(, nk), res$Tmat[, , !nonComputable]),
                      dim = c(nk, nk,  2 * (mpoints - sum(nonComputable)) + 1))
  }
  else {
    res$Tmat <- array(c(diag(, nk), res$Tmat[, , !nonComputable]),
                      dim = c(nk, nk, mpoints - sum(nonComputable) + 1))
  }
  colnames(res$Tmat) <- rownames(res$Tmat) <- labels

  res$lags <- apply(cbind(c(0, vDeltaH[-mpoints]), vDeltaH), 1, mean)[!nonComputable]
  res$lags <- c(0, res$lags)
  if (reverse) res$lags <- c(-rev(res$lags[-1]), res$lags)
  res$type <- "Empirical"
  class(res) <- "transiogram"
  return(res)
}

