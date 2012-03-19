pkgname <- "spMC"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('spMC')
set.seed(0)

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("All-spMC")
flush(stderr()); flush(stdout())

### Name: setCores
### Title: Set the number of CPU cores for HPC
### Aliases: setCores
### Keywords: programming

### ** Examples

#Set 2 CPU cores for parallel computation
setCores(2)

### Name: ACM
### Title: ACM Data
### Aliases: ACM
### Keywords: datasets

### ** Examples

data(ACM)
str(ACM)
summary(ACM)

### Name: boxplot.lengths
### Title: Stratum Lengths Boxplot
### Aliases: boxplot.lengths
### Keywords: spatial distribution hplot

### ** Examples

direction <- c(0,0,1)

# Compute the appertaining directional line for each location
loc.id <- which.lines(ACM[, 1:3], direction)

# Estimate stratum lengths
gl <- getlen(ACM$MAT3, ACM[, 1:3], loc.id, direction)

# Make the boxplot of the object gl
par(mfrow = c(1, 1))
boxplot(gl)
graphics::par(get("par.postscript", pos = 'CheckExEnv'))

### Name: ck.sim
### Title: Conditional Simulation Based on Indicator Cokriging
### Aliases: ck.sim
### Keywords: spatial distribution

### ** Examples

data(ACM)
# Estimate transition rates matrices and 
# proportions for the categorical variable MAT5
x <- multi.tpfit(ACM$MAT5, ACM[, 1:3])

# Generate the simulation grid
mygrid <- list()
mygrid$X <- seq(min(ACM$X), max(ACM$X), length = 3)
mygrid$Y <- seq(min(ACM$Y), max(ACM$Y), length = 3)
mygrid$Z <- -180 * 0:2 - 1
mygrid <- as.matrix(expand.grid(mygrid$X, mygrid$Y, mygrid$Z))

# Simulate the random field through
# Simple Indicator Cokriging algorithm and
# optimize by Simulated Annealing
myANSim <- ck.sim(x, ACM$MAT5, ACM[, 1:3], mygrid, ordinary = FALSE, optype = "param", max.it = 2)
myANSim <- ck.sim(x, ACM$MAT5, ACM[, 1:3], mygrid, ordinary = FALSE, optype = "fullprobs", max.it = 2)

# Simulate the random field through
# Ordinary Indicator Cokriging algorithm and
# optimize by Genetic Algorithm
myGASim <- ck.sim(x, ACM$MAT5, ACM[, 1:3], mygrid, GA = TRUE, optype = "semiprobs", max.it = 2)
myGASim <- ck.sim(x, ACM$MAT5, ACM[, 1:3], mygrid, GA = TRUE, optype = "coordprobs", max.it = 2)

### Name: density.lengths
### Title: Empirical Densities Estimation of Stratum Lengths
### Aliases: density.lengths
### Keywords: spatial distribution

### ** Examples

# Compute the empirical densities of stratum lengths
dgl <- density(gl)

### Name: embed.MC
### Tested in multi.tpfit() during the boxplot.lengths() example

### Name: getlen
### Tested during the boxplot.lengths() example

### Name: hist.lengths
### Title: Histograms of Stratum Lengths for Each Observed Category
### Aliases: hist.lengths
### Keywords: spatial distribution hplot

### ** Examples

# Plot the histograms
hist(gl)

### Name: ik.sim
### Title: Conditional Simulation Based on Indicator Kriging
### Aliases: ik.sim
### Keywords: spatial distribution

### ** Examples

# Simulate the random field through
# Simple Indicator Kriging algorithm and
# optimize by Simulated Annealing
myANSim <- ik.sim(x, ACM$MAT5, ACM[, 1:3], mygrid, ordinary = FALSE, optype = "param", max.it = 2)
myANSim <- ik.sim(x, ACM$MAT5, ACM[, 1:3], mygrid, ordinary = FALSE, optype = "fullprobs", max.it = 2)

# Simulate the random field through
# Ordinary Indicator Kriging algorithm and
# optimize by Genetic Algorithm
myGASim <- ik.sim(x, ACM$MAT5, ACM[, 1:3], mygrid, GA = TRUE, optype = "semiprobs", max.it = 2)
myGASim <- ik.sim(x, ACM$MAT5, ACM[, 1:3], mygrid, GA = TRUE, optype = "coordprobs", max.it = 2)

### Name: ilstpfit
### It will be tested during the multi.ilstpfit() example

### Name: image.multi.tpfit
### Title: Images with Multidimensional Transiograms
### Aliases: image.multi.tpfit
### Keywords: spatial distribution hplot

### ** Examples

# Set short names for categories 3 and 4
names(x$prop)[3:4] <- c("Clay and Sand", "Gravel and Sand")

# Plot 2-D theoretical sections of
# a multidimensional transiogram
image(x, 3, max.dist=c(20,10,5), which.dire=2:3,
      mar = .7, col=rev(heat.colors(500)),
      breaks=0:500/500, nlevels = 5)

### Name: imgMultiTransiogram
### Title: Images with Pseudo-empirical Multidimensional Transiograms
### Aliases: imgMultiTransiogram
### Keywords: spatial distribution hplot

### ** Examples

# Plot 2-D pseudo-empirical sections of
# a multidimensional transiogram
imgMultiTransiogram(ACM$MAT3, ACM[, 1:3], 3, 
      max.dist=c(20, 10, 5), which.dire=c(1, 3),
      col = rev(heat.colors(500)), breaks = 0:500 / 500,
      mar = .7, mle = TRUE)

### Name: is.lengths
### Title: Object test for lengths class
### Aliases: is.lengths
### Keywords: spatial attribute

### ** Examples

# Test the object gl
is.lengths(gl)

### Name: is.multi.tpfit
### Title: Object test for multi.tpfit class
### Aliases: is.multi.tpfit
### Keywords: spatial attribute

### ** Examples

# Test the object x
is.multi.tpfit(x)

### Name: is.multi.transiogram
### Title: Object test for multi.transiogram class
### Aliases: is.multi.transiogram
### Keywords: spatial attribute

### ** Examples

# Generate the matrix of 
# multidimensional lags
lags <- expand.grid(X=-1:1, Y=-1:1, Z=-1:1)
lags <- as.matrix(lags)

# Compute transition probabilities 
# from the multidimensional MC model
TrPr <- predict(x, lags)

# Test the object TrPr
is.multi.transiogram(TrPr)

### Name: is.tpfit
### Title: Object test for tpfit class
### Aliases: is.tpfit
### Keywords: spatial attribute

### ** Examples

# Estimate the parameters of a 
# one-dimensional MC model
MoPa <- tpfit(ACM$PERM, ACM[, 1:3], c(0, 0, 1))

# Test the object MoPa
is.tpfit(MoPa)

### Name: is.transiogram
### Title: Object test for transiogram class
### Aliases: is.transiogram
### Keywords: spatial attribute

### ** Examples

# Compute theoretical transition probabilities 
# from the one-dimensional MC model
TTPr <- predict(MoPa, lags = 0:2/2)

# Compute empirical transition probabilities 
ETPr <- transiogram(ACM$MAT5, ACM[, 1:3], c(0, 0, 1), 200, 20)

# Test the objects TTPr and ETPr
is.transiogram(TTPr)
is.transiogram(ETPr)

### Name: mcs.sim
### Title: Multinomial Categorical Simulation
### Aliases: mcs.sim
### Keywords: spatial distribution

### ** Examples

# Simulate the random field
myMCSim <- mcs.sim(x, ACM$MAT5, ACM[, 1:3], mygrid)

### Name: metpfit
### It will be tested during the multi.metpfit() example

### Name: mixplot
### Title: Plot of Multiple One-dimensional Transiograms
### Aliases: mixplot
### Keywords: spatial distribution hplot

### ** Examples

# Estimate empirical transition 
# probabilities by points
ETr <- transiogram(ACM$MAT3, ACM[, 1:3], c(0, 0, 1), 100)

# Estimate the transition rate matrix
RTm <- tpfit(ACM$MAT3, ACM[, 1:3], c(0, 0, 1))

# Compute transition probabilities 
# from the one-dimensional MC model
TPr <- predict(RTm, lags = ETr$lags)

# Plot empirical vs. theoretical transition probabilities
mixplot(list(ETr, TPr), type = c("p", "l"), pch = "+", col = c(3, 1))

### Name: mlen
### Tested in multi.tpfit() during the boxplot.lengths() example

### Name: multi.ilstpfit
### Title: Iterated Least Squares Method for Multidimensional Model
###   Parameters Estimation
### Aliases: multi.ilstpfit
### Keywords: spatial distribution models

### ** Examples

# Estimate the parameters of a 
# multidimensional MC model
multi.ilstpfit(ACM$MAT3, ACM[, 1:3], 100)

### Name: multi.metpfit
### Title: Maximum Entropy Method for Multidimensional Model Parameters
###   Estimation
### Aliases: multi.metpfit
### Keywords: spatial distribution models

### ** Examples

# Estimate transition rates matrices and
# proportions for the categorical variable MAT3
multi.metpfit(ACM$MAT3, ACM[, 1:3])

### Name: multi.tpfit
### Tested during the ck.sim() example

### Name: path.sim
### Title: Conditional Simulation Based on Path Algorithms
### Aliases: path.sim
### Keywords: spatial distribution

### ** Examples

# Simulate the random field through
# the fixed path algorithm
myFixPathSim <- path.sim(x, ACM$MAT5, ACM[, 1:3], mygrid,
                         radius = 50, fixed = TRUE)

# Simulate the random field through
# the random path algorithm
myRndPathSim <- path.sim(x, ACM$MAT5, ACM[, 1:3], mygrid, radius = 50)

### Name: plot.density.lengths
### Title: Plot Empirical Densities Estimates of Stratum Lengths
### Aliases: plot.density.lengths
### Keywords: spatial distribution hplot

### ** Examples

# Plot the empirical densities of stratum log-lengths
plot(dgl)

### Name: plot.hist.lengths
### Title: Plot Histograms of Stratum Lengths
### Aliases: plot.hist.lengths
### Keywords: spatial distribution hplot

### ** Examples

# Compute the histograms
hgl <- hist(gl, plot = FALSE)

# Plot the histograms
plot(hgl, col = "#efffef")

### Name: plot.lengths
### Title: Plot Stratum Lengths
### Aliases: plot.lengths
### Keywords: spatial distribution hplot

### ** Examples

# Plot the object gl
par(mfrow = c(1,1))
plot(gl)
graphics::par(get("par.postscript", pos = 'CheckExEnv'))

### Name: plot.transiogram
### Title: Plot One-dimensional Transiograms
### Aliases: plot.transiogram
### Keywords: spatial distribution hplot

### ** Examples

# Plot empirical transition probabilities
plot(ETr, type = "l")

# Plot theoretical transition probabilities
plot(TPr, type = "l")

### Name: predict.multi.tpfit
### Tested during the is.multi.transiogram() example

### Name: predict.tpfit
### Tested during the is.transiogram() example

### Name: print.density.lengths
### Title: Printing Empirical Densities Estimates of Stratum Lengths
### Aliases: print.density.lengths
### Keywords: spatial distribution

### ** Examples

# Print the empirical densities of stratum lengths
print(dgl)

### Name: print.lengths
### Title: Printing Stratum Lengths for Each Observed Category
### Aliases: print.lengths
### Keywords: spatial

### ** Examples

# Print stratum lengths
print(gl)

### Name: print.multi.tpfit
### Title: Printing Model Parameters for Multidimensional Continuos Lag
###   Spatial MC
### Aliases: print.multi.tpfit
### Keywords: spatial

### ** Examples

# Print results
print(x)

### Name: print.multi.transiogram
### Title: Printing Theoretical Multidimensional Transiograms
### Aliases: print.multi.transiogram
### Keywords: spatial

### ** Examples

# Print results
print(TrPr)

### Name: print.summary.lengths
### Title: Printing Stratum Lengths Summary for Each Observed Category
### Aliases: print.summary.lengths
### Keywords: spatial distribution

### ** Examples

# Summarize the stratum lengths
sgl <- summary(gl)

# Print the summary of stratum lengths
print(sgl)

### Name: print.tpfit
### Title: Printing Model Parameters for One-dimensional Continuos Lag
###   Spatial MC
### Aliases: print.tpfit
### Keywords: spatial

### ** Examples

# Print results
print(MoPa)

### Name: print.transiogram
### Title: Printing Theoretical or Empirical One-dimensional Transiograms
### Aliases: print.transiogram
### Keywords: spatial

### ** Examples

# Print results
print(TTPr)
print(ETPr)

### Name: summary.lengths
### Tested during the print.summary.lengths() example

### Name: tpfit
### Tested in multi.tpfit() during the boxplot.lengths() example

### Name: transiogram
### Tested during the is.transiogram() example

### Name: which.lines
### Tested during the boxplot.lengths() example

### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
