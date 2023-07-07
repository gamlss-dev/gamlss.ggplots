\name{resid_qqplot}
\alias{resid_qqplot}
\alias{add_resid_qqplot}
\alias{model_qqplot}

\title{QQ-plot of the residuals of a GAMLSS model}
\description{
The function \code{resid_qqplot()} produces a single QQ-plot of the residuals from a fitted GAMLSS model or any other model with suitable standardised residuals.

The function \code{add_resid_qqplot()} takes a QQ-plot created by \code{resid_qqplot()}  and adds another QQ-plot  from a different fitted model.

The function \code{add_resid_qqplots()} takes different fitted models and creates QQ-plots  for all of them. 

}
\usage{
resid_qqplot(obj, resid, value = 3, points.col = "steelblue4", 
              line.col = "darkgray", check_overlap = TRUE, title)
              
add_resid_qqplot(gg, obj, value = 3, points.col = "sienna",
             line.col = "darkgray", check_overlap = TRUE, title)  
             
model_qqplot(obj, ..., line.col = "steelblue4", title)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{A GAMLLS fitted model or}
  \item{resid}{any other residual suitable standardised.}
  \item{gg}{a ggplot}
  \item{value}{A cut off value to identify large or small residuals}
  \item{points.col}{the colout of the points in the plot}
  \item{line.col}{the colout of the line in the plot}
  \item{check_overlap}{if observations are identify this reduvce the cluterring}
  \item{title}{a title if needed it}
   \item{\dots}{extra GAMLSS models}
}
\details{This is a stanard QQ-plot but with the advadance of able to identify large or samll residuals}
\value{
A QQ-plotbis created
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
\code{\link[gamlss]{plot.gamlss}}
}
\examples{
data(abdom)
a<-gamlss(y~pb(x),family=LO,data=abdom)
b<-gamlss(y~pb(x),family=NO,data=abdom)
gg <- resid_qqplot(a)
add_resid_qqplot(gg, b)
model_qqplot(a,b)
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
