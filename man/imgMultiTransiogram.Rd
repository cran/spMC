\name{imgMultiTransiogram}
\Rdversion{1.1}
\alias{imgMultiTransiogram}

\title{Images with Pseudo-empirical Multidimensional Transiograms}

\description{
The function plots \eqn{2}{2}-D sections of a pseudo-empirical multidimensional transiogram computed without any ellipsoidal interpolation.}

\usage{
imgMultiTransiogram(data, coords, mpoints, which.dire, max.dist,
                    tolerance = pi/8, main, mar, ask = TRUE, ...,
                    nlevels = 10, contour = FALSE, rotation = NULL,
                    mle = FALSE)
}

\arguments{
%%         data vector of data
%%       coords coordinates matrix
%%      mpoints number of points per axes
%%   which.dire two chosen 1D directions
%%     max.dist vector of maximum distances
%%    tolerance angle tolerance (in radians)
%%         main title string
%%          mar vector to pass to par()  
%%          ask boolean to pass to par()
%%          ... other args to pass to image()
%%      nlevels number of levels to pass to contour()
%%      contour boolean values that permits to draw contour lines
%%     rotation vector of rotation angles (in radians) to pass to multi.tpfit
%%          mle logical value to pass to the function tpfit
  \item{data}{a categorical data vector of length \eqn{n}{n}.}
  \item{coords}{an \eqn{n \times d}{n x d} matrix where each row denotes the \eqn{d}{d}-D coordinates of data locations.}
  \item{mpoints}{the number of points per axes. It controls the accuracy of images to plot.}
  \item{which.dire}{a vector with two chosen axial directions. If omitted, all \eqn{2}{2}-D sections are plotted.}
  \item{max.dist}{a scalar or a vector of maximum length for the chosen axial directions.}
  \item{tolerance}{a numerical value for the tolerance angle (in radians). It's \code{pi/8} by default.}
  \item{main}{the main title (on top) whose font and size are fixed.}
  \item{mar}{a scalar or a numerical vector of the form \code{c(bottom, left, top, right)} which gives the number of margin lines to be specified on the four sides of image to plot. See \code{\link{par}}(\code{mar=}.).}
  \item{ask}{a logical value; if \code{TRUE}, the user is asked for input, before each plot. See \code{\link{par}}(ask=.).}
  \item{\dots}{other arguments to pass to the function \code{\link{image}}.}
  \item{nlevels}{the number of levels to pass to the function \code{\link{contour}}.}
  \item{contour}{logical. If \code{TRUE}, the function \code{\link{contour}} is used to draw contour lines of the theoretical multidimensional transiogram over images. This permits to compare the model probabilities with the pseudo-empirical ones. Defaults to \code{FALSE}.}
  \item{rotation}{a numerical vector of length \eqn{d - 1}{d - 1} with rotation angles (in radians), in order to perform the main axes rotation when multidimensional transiogram is estimated. No rotation is performed by default. See \code{\link{multi.tpfit}}.}
  \item{mle}{a logical value to pass to the function \code{\link{tpfit}}. It is \code{FALSE} by default.}
}

\details{
A multidimensional transiogram is a diagram which shows the transition probabilities for a single pair of categories. The probability is computed for any lag vector \eqn{h}{h} through 
\deqn{\mbox{expm} (\Vert h \Vert R_h),}{expm(||h|| R),}
where entries of \eqn{R_h}{R} are not ellipsoidally interpolated, but they are estimated for the direction specified by the vector \eqn{h}{h}.

In particular cases, some entries of the estimated matrix \eqn{R_h}{R} might be not finite, so that the exponential matrix is computable and the resulting transition probabilities are set to be \code{NaN}. If \code{mle = TRUE}, this problem may be partially solved.

The exponential matrix is evaluated by the scaling and squaring algorithm.
}

\value{An image is produced on the current graphics device. No values are returned.}

\references{Carle, S. F., Fogg, G. E. (1997) Modelling Spatial Variability with One and Multidimensional Continuous-Lag Markov Chains. \emph{Mathematical Geology}, \bold{29}(7), 891-918.

Higham, N. J. (2008) \emph{Functions of Matrices: Theory and Computation}. Society for Industrial and Applied Mathematics.

Sartore, L. (2010) Geostatistical models for 3-D data. M.Phil. thesis, Ca' Foscari University of Venice.}

\author{Luca Sartore \email{drwolf85@gmail.com}}

\seealso{\code{\link{image.multi.tpfit}}, \code{\link{image}}, \code{\link{plot.transiogram}}}

\examples{\donttest{
data(ACM)

# Plot 2-D pseudo-empirical sections of
# a multidimensional transiogram
imgMultiTransiogram(ACM$MAT3, ACM[, 1:3], 10, 
    max.dist=c(20, 10, 5), which.dire=c(1, 3),
    col = rev(heat.colors(500)), breaks = 0:500 / 500,
    mar = .7, mle = TRUE)
}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}
\keyword{distribution}
\keyword{hplot}
