\name{moment_gray_half}
\alias{moment_gray_half}
\alias{moment_gray_both}
\alias{moment_colour_half}
\alias{moment_colour_both}
\alias{centile_gray_both}
\alias{centile_colour_half}
\alias{centile_colour_both}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Functions to create the background for the bucket plots}
\description{The functions plot the moment transformed skewness and moment transformed kurtosis  of five important 4-parameter distributions in GAMLSS.
}
\usage{

moment_gray_half(legend = FALSE)

moment_gray_both(line_width = 1)

moment_colour_half(legend = TRUE)

moment_colour_both(legend = TRUE, line_width = 1)

centile_colour_half(type = c("tail", "central"), legend = TRUE, 
                   line_width = 1)

centile_colour_both(type = c("tail", "central"), legend = TRUE, 
                   line_width = 1)

centile_gray_both(type = c("tail", "central"), legend = TRUE, 
                  line_width = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{legend}{whether legend is required}
  \item{line_width}{line width}
  \item{type}{whether to plot `tail' or `central' skewness and kurtosis. }
}
\details{
The functions are described in Rigby \emph{et al} (2019) {}
}
\value{
A plot is created.
}
\references{
Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC. \doi{10.1201/9780429298547}
	An older version can be found in \url{https://www.gamlss.com/}.

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \doi{10.18637/jss.v023.i07}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  Chapman and Hall/CRC. \doi{10.1201/b21973}


(see also  \url{https://www.gamlss.com/}).

}
\author{Mikis Stasinopoulos, Bob Rigby and Fernanda de Bastiani}

\note{
The functions are use by the bucket plot function \code{model_mom_bucket()} to create the background of the bucket plots.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[gamlss.dist]{momentSK}}, \code{\link[gamlss.dist]{centileSK}}
}
\examples{
moment_gray_half()
moment_gray_both()
moment_colour_half()
moment_colour_both()

centile_colour_both()
centile_gray_both()
centile_colour_half()
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
