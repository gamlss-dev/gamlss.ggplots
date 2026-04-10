\name{bivar_fun}
\alias{bivar_fun}
\alias{bivar_pdf}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to Plot Two Dimensinal Functions
}
\description{
The function \code{bivar_fun()} is a generic function which can plot the contour plot of any two demensional function \code{f(x,y)}.

The function \code{bivar_pdf()} is a similar  function  to  \code{bivar_fun()}, in that it can plot can plot any two demensional pdf,  \code{f(x,y)},  with the additinal feature that it plots also  the HPR (hight probability region) as one of the contour plots.

}
\usage{
bivar_fun(fun, xlim = c(-3, 3), ylim = c(-3, 3), 
         n = 100, xlab = "x", ylab = "y", ...)

bivar_pdf(pdf, xlim = c(-3, 3), ylim = c(-3, 3), 
        n = 200, xlab = "x", ylab = "y", HDV = TRUE)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fun}{any two dimensional function f(x,y)}
 \item{pdf}{any two dimensinal joint pdf}
  \item{xlim}{the limits of the x}
  \item{ylim}{the limits of the x}
  \item{n}{the number of grid}
  \item{xlab}{the x lable}
  \item{ylab}{the y label}
 \item{HDV}{whether to show the hight density values (50\%)}
  \item{\dots}{additinal argument passes to the function}
}


\value{
The results is a \code{ggplot2} function.
}
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

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
xy_scatter
}


\examples{
f_mix <- function(x, y) 
{
  0.5 * exp(-(x^2 + y^2)) +
  0.5 * exp(-((x-2)^2 + (y-2)^2))
}

bivar_pdf(f_mix, xlim = c(-3, 5), ylim = c(-3, 5))
bivar_fun(f_mix, xlim = c(-3, 5), ylim = c(-3, 5))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
\keyword{regression}
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
