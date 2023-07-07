\name{data_cor}
\alias{data_cor}
%\alias{data_mcor}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plotting pairwise correlations  
}
\description{
The function \code{data_cor} takes a data frame and plots the pairwise Pearson's correlation coefficients of all continuous variables. 


}
\usage{
data_cor(data,  digits = 3, plot = TRUE, diag.off = TRUE, 
        lower.tri.off = FALSE, method = c("square", "circle"), 
        outline.color = "gray", colors = c("blue", "white", "red"),  
        legend.title = "Corr", title, ggtheme = theme_minimal(), 
        tl.cex = 12, tl.col = "black", tl.srt = 45, lab = TRUE, 
        lab_col = "black", lab_size = 3, circle.size = 20)
        
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a data frame}
  \item{digits}{the digits for printing the correlation coefficients}
  \item{plot}{whether to plot or not}
  \item{diag.off}{whether to show the diagonal ellements}
  \item{lower.tri.off}{whether to show the lower part of the matrix}
  \item{method}{plotting in \code{"square"} or \code{"cicle"}}
  \item{outline.color}{the outline colour}
  \item{colors}{the range of colours}
  \item{legend.title}{title for the legend}
  \item{title}{the main tittle}
  \item{ggtheme}{the theme for the plot, see package \pkg{ggthemes} for more themes}
  \item{tl.cex}{the text size for the marginal labels}
  \item{tl.col}{the colour of the he marginal labels}
  \item{tl.srt}{the angle of the text in the bottom labels of the table}
  \item{lab}{whether to show the correlation coefficients in the table}
  \item{lab_col}{the colour of the lettering of the correlation coefficients}
  \item{lab_size}{the size of the lettering of the correlation coefficients, increase (or decrease) if the defaul 3 is not appropriate}
  \item{circle.size}{the size of the circles, increase (or decrease) if the defaul 20 is not appropriate}
}

\value{
creates a plot
}
\references{
Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019) 
\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC. \doi{10.1201/9780429298547}.
	
Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  Chapman and Hall/CRC. \doi{10.1201/b21973}
}
\author{Mikis Stasinopoulos}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
mcor
}
\examples{
data_cor(rent)
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
