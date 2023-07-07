\name{data_outliers}
\alias{data_outliers}
\alias{y_outliers}

\title{Outlier identification}
\description{
Those two function idententify outliers in variables oor data
}
\usage{
data_outliers(data, value = 4, min.distinct = 50, family = SHASHo)

y_outliers(var, value = 4, family = SHASH)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a data frame}
  \item{var}{a continues variable}
  \item{value}{max value from which the absolute value of the z-scores should be  greater to identify outliers}
  \item{min.distinct}{if a variable has less distinct values than \code{min.distinct} is excluded}
  \item{family}{the distribution family used for standardization}
}
\details{the continuous variables are power transforemed and then standartised}
\value{
return a list
}
\references{
Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC. An older version can be found in \url{https://www.gamlss.com/}.

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{https://www.jstatsoft.org/v23/i07/}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  Chapman and Hall/CRC.  

(see also \url{https://www.gamlss.com/}).

}
\author{
Mikis Stasinopoulos
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{data_names}}
}
\examples{
da <- rent99[,-2]
data_outliers(da)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
