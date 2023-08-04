\name{data_dim}
\alias{data_dim}
\alias{data_which_na}
\alias{data_names}
\alias{data_sorter_names}

\title{Function to deal with data sets}
\description{
This is a set of function are designed to help the user to deal with new data sets.
}
\usage{
data_dim(data)
data_which_na(data)
data_names(data)
data_sorter_names(data, max = 5, newnames)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a data frame}
  \item{max}{the maximum number of characters allowed}
  \item{newnames}{New names if not abbreviated required}
}
\details{
The function \code{data_dim()} gives the dimensions and whether missing values exist.

The function \code{data_which_na()} gives the number of missing observation for each variable in the data 

The function \code{data_names()} gives the names of the variables  

The function \code{data_sorter_names()} takes the current names and abbreviates to \code{max} characters. 

}
\value{
The function \code{data_dim()} print the dimensions and whether missing values exist.

The function \code{data_which_na()} prints the number of missing observation for each variable in the data 

The function \code{data_names()} prints the names of the variables  

The function \code{data_sorter_names()} takes the current names and abbreviates to \code{max} characters and return a new data frame 
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
Mikis Stasinopoulos, Bob Rigby and Fernanda De Bastiani
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{data_cor}}}
\examples{
data_dim(rent)
data_which_na(rent)
data_names(rent)
data_sorter_names(rent)
}

\keyword{regression}

