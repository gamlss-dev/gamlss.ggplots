\name{data_str}
\alias{data_str}
\alias{y_distinct}
\alias{data_distinct}
\alias{data_cha2fac}
\alias{data_few2fac}
\alias{data_int2num}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function applied to data}
\description{
his is a set of function are designed to help the user to deal with the structure of new data sets.
}
\usage{
data_str(data, min.values = 100, min.levels = 10)
y_distinct(var)
data_distinct(data)
data_cha2fac(data, show.str = FALSE)
data_few2fac(data, min.levels = 10, show.str = FALSE)
data_int2num(data, min.values = 50, show.str = FALSE)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a data frame}
  \item{min.values}{the minimal value distinct values befor warning }
  \item{min.levels}{the minimal value distinct levels befor warning}
  \item{var}{a vector}
  \item{show.str}{whether to show the structure}
}
\details{
The function \code{data_str()} gives the structure of the data set.

The function \code{data_distinct()} gives the distinct values of the vectors in the data set 

The function \code{y_distinct()} gives the distinct values of single  vector 

The function \code{data_cha2fac()} tranforms all character vectors to factors  

The function \code{data_few2fac()} transform all vectors with fewer values than \code{min.levels} into factors

The function \code{data_int2num()} transform all integer vectors with more values than \code{min.values} into numeric
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
\author{Mikis Stasinopoulos, Bob Rigby and Fernanda De Bastiani}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{data_dim}}}
\examples{
data_str(rent)
data_distinct(rent)
data_cha2fac(rent)
data_few2fac(rent)
data_int2num(rent)
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
