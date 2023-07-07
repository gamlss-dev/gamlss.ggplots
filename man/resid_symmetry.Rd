\name{resid_symmetry}
\alias{resid_symmetry}
\alias{y_symmetry}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Symmetry plots}
\description{
The functions \code{resid_symmetry()} and \code{y_symmetry()} plot symmtry plots for residuals and single variable, respectively.
}
\usage{
resid_symmetry(model, title)

y_symmetry(y, title)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{A model which allows the function \code{resid()}}
   \item{y}{a single variable}
  \item{title}{A title for the plot if needed}
}
\details{
The function orders the data (or residuals) and takes the median minus the lower half and plot it against  the upper half minus the median. 
}
\value{
The symmetry plot is produced.
}
\references{
Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC. An older version can be found in \url{https://www.gamlss.com/}.

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{https://www.jstatsoft.org/v23/i07/}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  Chapman and Hall/CRC.  

(see also \url{https://www.gamlss.com/}).
}
\author{Mikis Stasinopoulos \email{d.stasinopoulos@londonmet.ac.uk}, Bob Rigby and Fernanda De Bastiani}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{resid_index}}
}
\examples{
y <- rBCT(1000, mu=3, sigma=.1, nu=-1, tau=5)
y_hist(y)
gg <- y_symmetry(y)
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
