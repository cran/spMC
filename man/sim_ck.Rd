\name{sim_ck}
\Rdversion{1.1}
\alias{sim_ck}

\title{Conditional Simulation Based on Indicator Cokriging}

\description{
The function simulates a random field through the Indicator Cokriging technique.
}

\usage{sim_ck(x, data, coords, grid, knn = 12, ordinary = TRUE,
       GA = FALSE, optype = c("param", "fullprobs", "semiprobs", 
       "coordprobs"), max.it = 1000)}

\arguments{
%%        x a multi_tpfit object
%%     data vector of data
%%   coords coordinates matrix
%%     grid simulation points
%%      knn number of k-nearest neighbours
%% ordinary boolean (if TRUE ordinary coKriging is applied rather than simple Kriging)
%%       GA boolean (if TRUE genetic algorithm is applied rather than simulated annealing)
%%   optype character with the objective function to minimize after the simulation
%%   max.it maximum number of iteration for the optimization method
  \item{x}{an object of the class \code{multi_tpfit}, typically with the output of the function \code{\link{multi_tpfit}}.}
  \item{data}{a categorical data vector of length \eqn{n}{n}.}
  \item{coords}{an \eqn{n \times d}{n x d} matrix where each row denotes the \eqn{d}{d}-D coordinates of data locations.}
  \item{grid}{an \eqn{m \times d}{m x d} matrix where each row denotes the \eqn{d}{d}-D coordinates in the simulation grid.}
  \item{knn}{an integer value which specifies the number of k-nearest neighbours for each simulation point. An optimal number is between 4 and 12. If \code{NULL} all observations are considered (just for very small dataset!!). It is \code{12} by default.}
  \item{ordinary}{a logical value; if \code{FALSE}, the probabilities are computed through the Simple coKriging technique, otherwise the Ordinary coKriging method is used.}
  \item{GA}{a logical value; if \code{TRUE}, the function performs the Genetic Algorithm instead of the Simulated Annealing.}
  \item{optype}{a character which denotes the objective function to compute when the optimization is performed.}
  \item{max.it}{a numerical value which specifies the maximum number of iterations to stop the optimization algorithm.}
}

\details{
This method computes an approximation of posterior probabilities
\deqn{\Pr\left(Z(\mathbf{s}_0) = z_k \left\vert \bigcap_{i = 1}^n Z(\mathbf{s}_i) = z(\mathbf{s}_i)\right.\right).}{Pr(Z(s_0) = z_k | Z(s_i) = z(s_i)),}
\eqn{\hspace{0cm}}{where i = 1, ..., n.}

The probability is calculated as the weighted sum of indicator variables which denote the presence of the \eqn{k}{k}-th category in observed points \eqn{\mathbf{s}_i}{s_i}. Weights involved in the sum are the solution of a system of equations.

Probabilities approximated are usually truncated and normalized with respect to the probability constraints, because such probabilities might lie outside the interval \eqn{[0, 1]}{[0, 1]}. The normalization procedure is designed such that it is not possible to obtain vectors such that the sum of their probabilities is always equal to one.

When an initial configuration is simulated, it is modified to reach a pattern similar to the sample by the use of simulated annealing or genetic algorithm. This procedure may be skipped by setting \code{max.it} equal to zero. There are several objective functions for this purpose, by setting \code{optype} equal to \code{"param"} the optimization is performed through parametric methods. The alternatives \code{"fullprobs"} and \code{"semiprobs"} are based on transition probabilities computed among simulation points, while the option \code{"coordprobs"} is based on transition probabilities calculated among observation and simulation points.
}

\value{A data frame containing the simulation grid, the simulated random field, predicted values and the approximated probabilities.}

\references{Carle, S. F., Fogg, G. E. (1996) Transition Probability-Based Indicator Geostatistics. \emph{Mathematical Geosciences}, \bold{28}(4), 453-476.

Carle, S. F. (1999) T-PROGS: Transition Probability Geostatistical Software. University of California, Davis.

Sartore, L. (2010) Geostatistical models for 3-D data. M.Phil. thesis, Ca' Foscari University of Venice.

%Tolosana-Delgado, R., Pawlowsky-Glahn, V., Egozcue, J.J. (2008) Indicator Kriging without Order Relation Violations. \emph{Mathematical Geosciences}, \bold{40}(3), 327-347.

Weise, T. (2009) \emph{Global Optimization Algorithms - Theory and Application}. \url{http://www.it-weise.de/}.}

\author{Luca Sartore \email{drwolf85@gmail.com}}
%%\note{
%%%%  ~~further notes~~
%%}

\seealso{\code{\link{sim_ik}}, \code{\link{sim_mcs}}, \code{\link{sim_path}}}

\examples{\donttest{
data(ACM)

# Model parameters estimation for the
# multinomial categorical simulation
x <- multi_tpfit(ACM$MAT5, ACM[, 1:3])

# Generate the simulation grid
mygrid <- list()
mygrid$X <- seq(min(ACM$X), max(ACM$X), length = 20)
mygrid$Y <- seq(min(ACM$Y), max(ACM$Y), length = 20)
mygrid$Z <- -40 * 0:9 - 1
mygrid <- as.matrix(expand.grid(mygrid$X, mygrid$Y, mygrid$Z))

# Simulate the random field through
# Simple Indicator Cokriging algorithm and
# optimize by Simulated Annealing
myANSim <- sim_ck(x, ACM$MAT5, ACM[, 1:3], mygrid, ordinary = FALSE)


# Simulate the random field through
# Ordinary Indicator Cokriging algorithm and
# optimize by Genetic Algorithm
myGASim <- sim_ck(x, ACM$MAT5, ACM[, 1:3], mygrid, GA = TRUE)
}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}
\keyword{distribution}