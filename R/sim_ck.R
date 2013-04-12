"sim_ck" <- 
function(x, data, coords, grid, knn = 12, ordinary = TRUE, GA = FALSE, optype = c("param", "fullprobs", "semiprobs", "coordprobs"), max.it = 1000) {
  # Generation of conditional simulation based on coKriging
  #
  #        x a multi_tpfit object
  #     data vector of data
  #   coords coordinates matrix
  #     grid simulation points
  #      knn number of k-nearest neighbours
  # ordinary boolean (if TRUE ordinary coKriging is applied rather than simple coKriging)
  #       GA boolean (if TRUE genetic algorithm is applied rather than simulated annealing)
  #   optype character with the objective function to minimize after the simulation
  #   max.it maximum number of iteration for the optimization method

  ordinary <- as.logical(ordinary)
  if(!is.multi_tpfit(x)) stop("argument \"x\" must be a 'multi_tpfit' object.")

  if (missing(grid)) stop("simulation grid is missing.")
  if (!is.factor(data)) data <- as.factor(data)
  if (!is.matrix(coords)) coords <- as.matrix(coords)
  if (!is.matrix(grid)) grid <- as.matrix(grid)
  nc <- dim(coords)[2]
  nr <- dim(coords)[1]
  if (is.null(knn)) {knn <- nr} else {knn <- as.integer(knn)}
  if (length(data) != nr) stop("the number of data is not equal to the number of coordinates")
  if (nc != dim(grid)[2]) stop("coordinates and simulation grid must have the same number of columns")
  if (nr < knn) knn <- nr
  nrs <- dim(grid)[1]
  nk <- nlevels(data)
  levLabels <- levels(data)
  data <- as.integer(data)
  storage.mode(coords) <- "double"
  storage.mode(grid) <- "double"
  if (length(optype) > 1) optype <- optype[1]

  dire.mat <- diag(, nc)
  new.coords <- coords
  new.grid <- grid
  if (!is.null(x$rotation)) {
    dire.mat <- .C('rotaxes', nc = as.integer(nc), ang = as.double(x$rotation), 
                   res = as.double(dire.mat), DUP = FALSE, PACKAGE = "spMC")$res
    dire.mat <- matrix(dire.mat, nc, nc)
    new.coords <- matrix(.C('fastMatProd', nr = as.integer(nr), ni = as.integer(nc),
                          mat1 = as.double(coords), nc = as.integer(nc),
                          mat2 = as.double(dire.mat), res = as.double(new.coords),
                          DUP = FALSE, PACKAGE = "spMC")$res, nrow = nr, ncol = nc)
    new.grid <- matrix(.C('fastMatProd', nr = as.integer(nrs), ni = as.integer(nc),
                          mat1 = as.double(grid), nc = as.integer(nc),
                          mat2 = as.double(dire.mat), res = as.double(new.grid),
                          DUP = FALSE, PACKAGE = "spMC")$res, nrow = nrs, ncol = nc)
  }

  # FINDING THE k-NEAREST NEIGHBOURS #
  indices <- matrix(0L, nrow = knn, ncol = nrs)
  indices <- matrix(.C('knear', nc = as.integer(nc), nr = as.integer(nr),
                       coords = as.double(new.coords), nrs = as.integer(nrs),
                       grid = as.double(new.grid), knn = as.integer(knn),
                       indices = as.integer(indices), DUP = FALSE, PACKAGE = "spMC")$indices,
                    nrow = knn, ncol = nrs)

  # SORTING SIMULATION GRID #
  path <- do.call("order", as.data.frame(t(indices)))
  indices <- indices[, path]
  grid <- grid[path, ]
  new.grid <- new.grid[path, ]
  groups <- !duplicated(t(indices))
  groups <- cumsum(groups)

  # KRIGING PROCEDURE #
  probs <- matrix(0, nrow = nrs, ncol = nk)
  probs <- matrix(.C('getCKPrbs', ordinary = as.integer(ordinary),
                     indices = as.integer(indices), groups = as.integer(groups), 
                     knn = as.integer(knn), nc = as.integer(nc), nr = as.integer(nr),
                     nrs = as.integer(nrs), data = as.integer(data),
                     coords = as.double(new.coords), grid = as.double(new.grid),
                     nk = as.integer(nk), coef = as.double(unlist(x$coefficients)),
                     prop = as.double(x$prop), probs = as.double(probs),
                     DUP = FALSE, PACKAGE = "spMC")$probs,
                  nrow = nrs, ncol = nk)           
  # PREDICTION AND SIMULATION PROCEDURE #
  pred <- apply(probs, 1, which.max)
  initSim <- vector("integer", nrs)
  initSim <- .C('tsimCate', nk = as.integer(nk), n = as.integer(nrs), prhat = as.double(probs),
     initSim = as.integer(initSim), DUP = FALSE, PACKAGE = "spMC")$initSim

  # OPTIMIZATION PROCEDURE #
  toOptim <- function(x, mySim, grid) {
    xnew <- list()
    xnew$coefficients <- lapply(1:nc, function(i) {
      ml <- mlen(mySim, grid, loc.id[, i], dire.mat[i, ])
      Rmat <- embed_MC(mySim, grid, loc.id[, i], dire.mat[i, ])
      diag(Rmat) <- -1
      Rmat <- diag(1 / ml) %*% Rmat
      return(Rmat)
    })
    xnew$prop <- table(mySim)
    xnew$prop <- xnew$prop / sum(xnew$prop)
    if(length(unlist(x$coefficients)) != length(unlist(xnew$coefficients))) return(Inf)
    if(length(unlist(x$prop)) != length(unlist(xnew$prop))) return(Inf)
    return(sum((unlist(xnew$coefficients) - unlist(x$coefficients))^2) + sum((xnew$prop - x$prop)^2))
  }
  if(optype == "fullprobs") {
    toOptim <- function(x, mySim, grid) {
      res <- 0
      res <- .C('objfun', nrs = as.integer(nrs), nk = as.integer(nk), nc = as.integer(nc),
         mySim = as.integer(mySim), grid = as.double(grid),
         coef = as.double(unlist(x$coefficients)), prop = as.double(x$prop),
         res = as.double(res), DUP = FALSE, PACKAGE = "spMC")$res
      return(res)
    }
  }
  if(optype == "semiprobs") {
    sknn <- knn + 1L
    storage.mode(sknn) <- "integer"
    if(nrs < sknn) sknn <- nrs
    indicesim <- matrix(0L, nrow = sknn, ncol = nrs)
    indicesim <- matrix(.C('knear', nc = as.integer(nc), nr = as.integer(nrs),
                         coords = as.double(new.grid), nrs = as.integer(nrs),
                         grid = as.double(new.grid), knn = as.integer(sknn),
                         indices = as.integer(indicesim), DUP = FALSE,
                         PACKAGE = "spMC")$indices,
                      nrow = sknn, ncol = nrs)
    sknn <- sknn - 1L
    toOptim <- function(x, mySim, grid) {
      res <- 0
      res <- .C('fastobjfun', knn = as.integer(sknn),
         indices = as.integer(indicesim[-1L, ]), nrs = as.integer(nrs), nk = as.integer(nk),
         nc = as.integer(nc), nr = as.integer(nrs), mySim = as.integer(mySim),
         grid = as.double(grid), coef = as.double(unlist(x$coefficients)),
         prop = as.double(x$prop), data = as.integer(mySim), coords = as.double(grid),
         res = as.double(res), DUP = FALSE, PACKAGE = "spMC")$res
      return(res)
    }
  }
  if(optype == "coordprobs") {
    toOptim <- function(x, mySim, grid) {
      res <- 0
      res <- .C('fastobjfun', knn = as.integer(knn), indices = as.integer(indices),
         nrs = as.integer(nrs), nk = as.integer(nk), nc = as.integer(nc), nr = as.integer(nr),
         mySim = as.integer(mySim), grid = as.double(grid),
         coef = as.double(unlist(x$coefficients)), prop = as.double(x$prop),
         data = as.integer(data), coords = as.double(coords), res = as.double(res),
         DUP = FALSE, PACKAGE = "spMC")$res
      return(res)
    }
  }
  if (max.it > 0 & !(optype %in% c("fullprobs", "semiprobs", "coordprobs"))) {
    loc.id <- apply(dire.mat, 1, function(d) which_lines(grid, d, tolerance = x$tolerance))
  }
  storage.mode(initSim) <- "integer"
  storage.mode(max.it) <- "integer"
  Rnv <- new.env()
  Rnv <- parent.env(Rnv)

  if (!GA) { # SIMULATED ANNEALING #
    old <- .Call("annealingSIM", max.it, initSim, x, grid, quote(toOptim(x, pp, grid)), Rnv, PACKAGE = "spMC")
  }
  else {     # GENETIC ALGORITHM #
    old <- .Call("geneticSIM", max.it, initSim, x, grid, quote(toOptim(x, pp, grid)), Rnv, PACKAGE = "spMC")
  }
  tmpfct <- 1:nk
  tmpfct <- factor(tmpfct, labels = levLabels)
  old <- tmpfct[old]
  pred <- tmpfct[pred]
  res <- data.frame(grid, old, pred, probs)
  names(res) <- c(colnames(coords), "Simulation", "Prediction", levLabels)
  res[path, ] <- res
  attr(res, "type") <- paste(ifelse(ordinary, "Ordinary", "Simple"), 
                             "Indicator coKriging Simulation")
  return(res)
}
