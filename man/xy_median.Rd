\name{xy_median}
\alias{xy_median}
\alias{median_cw}
\alias{median_pc}
\alias{median_sp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The  Median in  Two Dimensions 
}
\description{
The function \code{xy_median(x, y)} uses the functions \code{median_cw(x, y)}, \code{median_pc(x, y)} and 
\code{median_sp(x, y)} to calculate the component wise,  principle componet and spatial median, respectively, in 2-Dimensions. 
}
\usage{
xy_median(x, y, method = c("cw", "pc", "sp"))

median_cw(x, y)

median_pc(x, y)

median_sp(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the first variable}
  \item{y}{the second variable}
  \item{method}{The choises are \code{c("cw", "pc", "sp")} for component wise,  principle componet and spatial median, respectively.}
}
\details{
The function \code{xy_median(x, y)} uses the other three functions to calculate the mediam in 2-Dimentions.
}
\value{ The coordinats of the median}
\references{
Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC. An older version can be found in \url{https://www.gamlss.com/}.

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{https://www.jstatsoft.org/v23/i07/}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  Chapman and Hall/CRC.  

Stasinopoulos, M.D., Kneib, T., Klein, N., Mayr, A. and Heller, G.Z., (2024). \emph{Generalized Additive Models for Location, Scale and Shape: A Distributional Regression Approach, with Applications} (Vol. \bold{56}). Cambridge University Press.

(see also \url{https://www.gamlss.com/}).
}
\author{
Mikis Stasinopoulos
}
\note{
The median in two dimensions need two pieces of information e.g.  two coordinates or one cooridate and one direction. 
}



\seealso{
\code{\link{median}}
}
\examples{
library(gamlss)
median_cw(rent99$rent, rent99$rentsqm)
median_pc(rent99$rent, rent99$rentsqm)
median_sp(rent99$rent, rent99$rentsqm)
xy_median(rent99$rent, rent99$rentsqm)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
 \keyword{regression}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
