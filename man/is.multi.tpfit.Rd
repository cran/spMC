\name{is.multi.tpfit}
\Rdversion{1.1}
\alias{is.multi.tpfit}

\title{Object test for multi.tpfit class}

\description{Function to test if an object is of the class \code{multi.tpfit}.}

\usage{is.multi.tpfit(object)}

\arguments{
\item{object}{object to be tested.}
}

\details{The function returns \code{TRUE} if and only if its argument is a \code{multi.tpfit} object.}

\value{A logical value.}

\author{Luca Sartore \email{drwolf85@gmail.com}}

\seealso{\code{\link{multi.tpfit}}}

\examples{\donttest{
data(ACM)

# Estimate the parameters of a 
# multidimensional MC models
MoPa <- multi.tpfit(ACM$MAT5, ACM[, 1:3])

# Test the object MoPa
is.multi.tpfit(MoPa)
}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}
\keyword{attribute}
