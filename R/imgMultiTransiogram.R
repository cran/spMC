imgMultiTransiogram <-
function(data, coords, mpoints, which.dire, max.dist, tolerance = pi/8,
         main, mar, ask = TRUE, ..., nlevels = 10, contour = FALSE, rotation = NULL,
         mle = FALSE) {
  # Plot transition probabilities matrixes 2D
  # through no ellispoidal interpolation
  #
  #       data vector of data
  #     coords coordinates matrix
  #    mpoints number of points per axes
  # which.dire two choosen 1D directions
  #   max.dist vector of maximum distances
  #  tolerance angle tolerance (in radians)
  #       main title string
  #        mar vector to pass to par()  
  #        ask boolean to pass to par()
  #        ... other args to pass to image()
  #    nlevels number of levels to pass to contour()
  #    contour boolean values that permits to draw contour lines
  #   rotation vector of rotation angles (in radians) to pass to multi.tpfit
  #        mle logical value to pass to the function tpfit

  if (!is.factor(data)) data <- as.factor(data)
  if (!is.matrix(coords)) coords <- as.matrix(coords)
  coordsnames <- colnames(coords)
  n <- dim(coords)[1]
  nc <- dim(coords)[2]
  nk <- nlevels(data)

  if (missing(main) || !is.character(main)) main <- "Pseudoempirical transiogram"
  if (missing(mpoints)) stop("\"mpoints\" is missing")
  if (length(mpoints) != 1) mpoints <- mpoints[1]
  all <- FALSE
  if (missing(which.dire)) all <- TRUE
  if (missing(max.dist)) max.dist <- 1
  if (length(max.dist) == 1) max.dist <- rep(max.dist, nc)
  if (length(max.dist) != nc) 
    stop("\"max.dist\" must be a scalar or vector with the same dimension of coordinates")
  max.dist <- abs(max.dist)

  rawLags <- sapply(max.dist, function(h)
    seq(-h, h, length = mpoints)
  )
  prop <- table(data)
  prop <- prop / sum(prop)

  which.dire <- if (all) combn(1:nc, 2) else matrix(which.dire)
  nimg <- dim(which.dire)
  if (nimg[1] != 2) stop("wrong length of \"which.dire\"")
  if (!is.null(rotation)) {
    if (length(rotation) != nc - 1) stop("wrong length of rotation vector; the right length is ", nc - 1)
  }

  ly <- matrix(c(rep(1, nk), rep(0, nk), 2:(nk^2+1), rep(0, nk), rep(nk^2+2, nk)), ncol = nk, byrow = TRUE)
  yl <- c(rep(0, nk + 4))
  ly <- cbind(yl, yl, ly, yl, yl)
  widths <- c(rep(4 - 0.5 * nk, 2), rep(30/nk, nk), rep(4 - 0.5 * nk, 2)) / 4
  heights <- c(0.75, 1/3.5, rep(7.5/nk, nk), 2/3.5, 1)
  ly <- layout(ly, widths = widths, heights = heights, respect = TRUE)
  
  nomi <- levels(data)
  if (missing(mar) || !is.numeric(mar)) {
    mar <- sum(c(0, 1, 0, -0.5, -0.2, rep(0, 4), 0.2, rep(c(-0.1, rep(0, 3)), 2), 0)[1:nk])
    mar <- rep(mar, 4)
  }
  else {
    if (length(mar) < 4) {
      mar <- rep(mar[1], 4)
    }
    else {
      mar <- mar[1:4]
    }    
  }

  oldcontour <- contour
  storage.mode(which.dire) <- "integer"

  for (i in 1:nimg[2]) {
    lagsMat <- as.list(rep(0, nc))
    lagsMat[[which.dire[1, i]]] <- rawLags[, which.dire[1, i]]
    lagsMat[[which.dire[2, i]]] <- rawLags[, which.dire[2, i]]
    lagsMat <- as.matrix(expand.grid(lagsMat))
    nl <- dim(lagsMat)[1]
    
    contour <- oldcontour
    if (contour) {
      x <- multi.tpfit(data, coords, tolerance, rotation)
      if (all(is.finite(unlist(x$coefficients)))) {
        Tprobs <- predict(x, lagsMat)
      }
      else {
        warning("Contour lines cannot be computed.")
        contour <- FALSE
      }
    }

    lagsMat <- t(lagsMat)
    same.dire <- vector("integer", nl)
    same.dire <- .C('wd', lags = as.double(lagsMat), nc = as.integer(nc),
                    nr = as.integer(nl), res = as.integer(same.dire),
                    DUP = FALSE, PACKAGE = "spMC")$res
    wsd <- !duplicated(same.dire)
    storage.mode(wsd) <- "integer"
    coefs <- apply(lagsMat[, same.dire[wsd]], 2,
                   function(d) {
                     res <- tpfit(data, coords, d, tolerance, mle)$coefficients
                     if(prod(is.finite(res)) == 0) res <- matrix(NaN, nk, nk)
                     return(res)
                   })
    nmat <- sum(wsd)
    coefs <- array(unlist(coefs), dim = c(nk, nk, nmat))
    wsd <- as.integer(as.factor(same.dire))
    Eprobs <- array(0, dim = c(nk, nk, nl))
    Eprobs <- .C('predPSEUDO', coefs = as.double(coefs), prop = as.double(prop),
                 lags = as.double(lagsMat), nk = as.integer(nk), nc = as.integer(nc),
                 nr = as.integer(nl), nmat = as.integer(nmat), wsd = as.integer(wsd),
                 whichd = as.integer(which.dire[2, i]), mypred = as.double(Eprobs),
                 NAOK = TRUE, DUP = FALSE, PACKAGE = "spMC")$mypred
    Eprobs <- array(Eprobs, dim = c(nk, nk, nl))

    if (ask && all && (nimg[2] > 1)) {
      devAskNewPage(TRUE)
      on.exit(devAskNewPage(FALSE))
    }

    par(mar = c(0.3, 0.1, 0.3, 0.1))
    plot.new()
    title <- paste(main, " (", coordsnames[which.dire[1, i]], ", ", coordsnames[which.dire[2, i]], ")", sep = "")
    text(0.5, 0.5, labels = title, cex = 2)

    par(mar = mar)
    for (j in 1:nk) {
      for (k in 1:nk) {
        image(rawLags[, which.dire[1, i]], rawLags[, which.dire[2, i]],
              t(matrix(rev(Eprobs[j, k, ]), mpoints, mpoints)),
              xlab = paste("Coord", which.dire[1, i]),
              ylab = paste("Coord", which.dire[2, i]), ..., axes = FALSE)
        box()
        if (k == 1) axis(2)
        if (k == nk) axis(4, 0, labels = nomi[j], tick = FALSE, font = 3)
        if (j == nk) axis(1)
        if (j == 1) axis(3, 0, labels = nomi[k], tick = FALSE, font = 3)
        if (contour) {
          contour(rawLags[, which.dire[1, i]], rawLags[, which.dire[2, i]],
                  t(matrix(rev(Tprobs$Tmat[j, k, ]), mpoints, mpoints)),
                  nlevels, add = TRUE)
        }
      }
    }

    par(mar = c(2.2, 0.1, 1.2, 0.1))
    image(matrix(0:500/500, ncol=1), ..., main="Legend", axes = FALSE)
    box()
    axis(1)

  }

}
