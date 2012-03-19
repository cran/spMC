transiogram <-
function(data, coords, direction, max.dist = Inf, mpoints = 20, tolerance = pi/8) {
  # Empirical transition probabilities estimated by points
  #
  #       data vector of data
  #     coords coordinates matrix
  #  direction vector (or versor) of choosen direction
  #   max.dist maximum distance for counting
  #    mpoints number of lags
  #  tolerance angle tolerance (in radians)

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
  RNG <- range(coords)
  deltah <- min(diff(RNG), max.dist) / mpoints
  vDeltaH <- cumsum(rep(deltah, mpoints))

  # count transition occurences
  Tcount <- .C('transCount', n = as.integer(n), data = as.integer(data), 
               nc = as.integer(nc), coords = as.double(coords),
               dire = as.double(direction), tolerance = as.double(tolerance), 
               mpoints = as.integer(mpoints), bins = as.double(vDeltaH), 
               nk = as.integer(nk), trans = as.double(vector("numeric", nk^2 * mpoints)),
               DUP = FALSE, PACKAGE = "spMC")$trans
  Tcount <- array(Tcount, dim = c(nk, nk, mpoints))
  rwSum <- apply(Tcount, 3, function(x) apply(x, 1, sum))
  mtSum <- apply(Tcount, 3, sum)
  nonComputable <- mtSum == 0
  if (all(nonComputable)) stop("\"max.dist\" is lower than the minimum distance")
  # compute transition probabilities
  Tcount <- .C('transProbs', mpoints = as.integer(mpoints), nk = as.integer(nk), 
               rwsum = as.double(rwSum), empTR = as.double(Tcount),
               DUP = FALSE, PACKAGE = "spMC")$empTR
  res <- list()
  res$Tmat <- array(Tcount, dim = c(nk, nk, mpoints))
  res$Tmat <- array(res$Tmat[, , !nonComputable], dim = c(nk, nk, mpoints - sum(nonComputable)))
  colnames(res$Tmat) <- rownames(res$Tmat) <- labels

  res$lags <- apply(cbind(c(0, vDeltaH[-mpoints]), vDeltaH), 1, mean)[!nonComputable]
  res$type <- "Empirical"
  class(res) <- "transiogram"
  return(res)
}

