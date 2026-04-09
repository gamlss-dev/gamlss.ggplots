\name{xy_scatter}
\alias{xy_scatter}
\alias{xy_pit_scatter}
\alias{xy_hist}
\alias{xy_pit_hist}
\alias{xy_density}
\alias{xy_density_plain}
\alias{xy_hist_kde2d}
\alias{xy_pit_density}
\alias{xy_ECDF_contour}
\alias{xy_ECDF_hist}
\alias{ECDF_2D}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Data Plot for Bivariate Vectors 
}
\description{
This set of plots are for helping visuallise relationship between two continuous variables which both could be considered as responses.

The function \code{xy_scatter()} plot the scatter plott of the two continuous variables but also add the point of the median, the heigh denstity region (50\%) and and a 2- dimnesional kernal density estimator (KDE).

The function \code{xy_pit_scatter()} is similar to the the function  \code{xy_scatter()}  but instaead of the original variables it plots their PIT (probability intergral trsnsormation).

The function \code{xy_hist()} (similar to the\code{xy_scatter()}), it plots a 2D-histogram of the original data with  which uses \eqn{bins\times bins}, by default \eqn{ bins=50}.

The function \code{xy_hist_kde2d()}is using the \code{MASS} package function  \code{kde2d()} to produce a two dimensional histogram.

The function \code{xy_pin_hist()} (similar to the\code{xy_pin_scatter()}), it plots a 2D-histogram of the transformed data ia a grid \eqn{bins\times bins}, by default \eqn{ bins=50}.

The function \code{xy_density()} produce a kernel density estimator KDE od the two continuous variables.

The function \code{xy_density_plain()}  produces identical results to  \code{xy_density()} but it shoes just the contour plots of the  2d kernel density estimator.

The functions \code{xy_ECDF_contour())}  and \code{xy_ECDF_hist()}  produce a contour plot or a 2D-histogram, rerpectively,  of the ECDF of \eqn{x} and \eqn{y} calculated by the function \code{ECDF_2D()}.

}
\usage{
xy_scatter(x, y = NULL, method = c("cw", "pc", "sp"), 
          density = TRUE, HDR = TRUE, 
          density.colour = "lightblue", 
          median.colour = "red", median.size = 3, 
          point.size = 1, point.colour = "grey", 
          HDR.colour = "darkred")

xy_pit_scatter(x, y = NULL, method = c("cw", "pc", "sp"), 
         density = TRUE, HDR = TRUE, 
         density.colour = "lightblue", 
         median.colour = "red", median.size = 3, 
         point.size = 1, point.colour = "grey", 
         HDR.colour = "darkred")  
         
xy_hist(x, y, colour = hcl.colors(20), 
         points = TRUE, 
         point.colour = "gray", 
         point.size = 0.1, 
         bins = 50)         

xy_hist_kde2d(x, y, colour = hcl.colors(20), 
      points = TRUE, point.colour = "gray", 
      point.size = 0.1, bins = 50)

xy_pit_hist(x, y, colour = hcl.colors(20), 
       points = TRUE, 
       point.colour = "gray", 
       point.size = 0.1, 
       bins = 50)

xy_density(x, y, colour = hcl.colors(20), 
      points = FALSE, point.colour = "black", 
      point.size = 0.1)

xy_density_plain(x, y, colour = hcl.colors(20), 
      points = FALSE, point.colour = "black", 
      point.size = 0.1)

xy_pit_density(x, y = NULL, 
       points = FALSE, 
       point.size = 1, point.colour = "grey")
       
xy_ECDF_contour(x, y)     

xy_ECDF_hist(x, y) 

ECDF_2D(value.x, value.y, data.x, data.y)
}

\arguments{
  \item{x}{The first continuous variable}
  \item{y}{The second continuous variable}
  \item{method}{The median method: i) \code{cw}; case wise, \code{pc}; ii) principal componet, iii) \code{sp} spacial}
  \item{density}{whether to plot the density}
  \item{HDR}{whether to plot the hight density region HDR}
  \item{density.colour}{The colour of the kernal density estimator}
  \item{median.colour}{the colour for the point for the  median}
  \item{median.size}{The size of the point for the mediam}
  \item{point.size}{The size of the points points for the scatter plot}
  \item{point.colour}{the colour of the points in he scatter plot}
  \item{HDR.colour}{the colour used for the HDR}
  \item{colour}{the colours in the 2D-histogram}
  \item{points}{whether to show the points}
  \item{bins}{the number of the bins in the the 2-D histogram defaul 50 }
  \item{value.x}{the value of x to evaluate the ECDF}
  \item{value.y}{the value of y to evaluate the ECDF}
  \item{data.x}{the x data vector for the creation of the ECDF}
  \item{data.y}{the y data vector for the creation of the ECDF}
} 

\details{
This set of plots, is, to help to discuver relationship between two continuous response variables.
}

\value{
Thew result is a relevant plot
}
\references{

Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC. An older version can be found in \url{https://www.gamlss.com/}.

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{https://www.jstatsoft.org/v23/i07/}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  Chapman and Hall/CRC.  

Stasinopoulos, M.D., Kneib, T., Klein, N., Mayr, A. and Heller, G.Z., (2024). \emph{Generalized Additive Models for Location, Scale and Shape: A Distributional Regression Approach, with Applications} (Vol. \bold{56}). Cambridge University Press.

(see also \url{https://www.gamlss.com/}).~
}

\author{
Mikis Stasinopoulos
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{xy_median}}}

\examples{
library(gamlss)
x <- rent99$rent
y <- rent99$rentsqm
xy_scatter(x,y)
xy_pit_scatter(x,y)
xy_hist(x,y)
xy_pit_hist(x,y)
xy_density(x,y)
xy_density_plain(x,y)
xy_pit_density(x,y)
xy_ECDF_hist(x,y)
xy_ECDF_contour(x,y)
# for pit ECDF use 
n <- length(x)  
u <- rank(x) / (n + 1)
v <- rank(y) / (n + 1)
xy_ECDF_hist(u,v)
xy_ECDF_contour(u,v)
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
