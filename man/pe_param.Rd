\name{pe_param}
\alias{pe_param}
\alias{pe_1_parameter}
\alias{pe_2_parameter}
\alias{pe_eta}
\alias{pe_param_grid}
\alias{pe_eta_grid}

\title{
Partial Effect of a Term IN a Parameters OR Predictor}
\description{

There are several functions here for plottong partial effect of terms in  a distributional parameter GAMLSS model.
Those functions help the intepretation of a GAMLSS model.

In the hard of it there are the two functions; 

i) \code{pe_1_paramater()} and 

ii)  \code{pe_2_paramater()}.

The first plots the effect of one term in the data keeping the rest of the term at specified values (scenarios).
The seocond plots the effect of two term keeping the rest of the term at specified values.
The default scenarios are specified using the argument  \code{how.scenario}.

The function \code{pe_param()} calls either  \code{pe_1_paramater()} or \code{pe_2_paramater()} depending on the argument \code{term}. That is  whether the arument is a single argument, e.g. "age", or a double one e.g. c("age", "gender").  

Similarly with  the  function \code{pe_eta()} which is almost identical to \code{pe_param()} but instead of ploting terms for the parameters (i.e. \eqn{\mu} or  \eqn{\sigma}) it plots terms for the predictors (i.e. \eqn{\eta_{\mu}} or \eqn{\eta_{\sigma}}). 

The functions 

i)  \code{pa_param_grid()} and 
ii) \code{pa_eta_grid()}

produce multple plots on a grid.

}
\usage{
pe_1_parameter(obj = NULL, term = NULL, data = NULL, 
          n.points = 100, parameter = c("mu", "sigma", "nu", "tau"), 
          type = c("parameter", "eta"), scenario = list(), 
          how.scenario = c("median", "last", "fixed"), 
          centered.from = c("mean", "median", "none", "rlevel"), 
          col = "darkblue", linewidth = 1.3, name.obj = NULL, 
          data.plot = FALSE, data.col = "lightblue", 
          data.size = 0.1, data.alpha = 0.9, 
          rug.plot = TRUE, rug.col = "gray", rug.size = 0.5, 
          factor.size = 15, ylim = NULL, plot = TRUE, title)

pe_2_parameter(obj = NULL, terms = NULL, data = NULL, 
          n.points = 100, parameter = c("mu", "sigma", "nu", "tau"), 
          type = c("parameter", "eta"), 
          how.scenario = c("median", "last", "fixed"), 
          scenario = list(), 
          centered.from = c("mean", "median", "none"), 
          col = "darkblue", linewidth = 1.3, data.plot = FALSE, 
          data.col = "lightblue", data.size = 0.1, 
          data.alpha = 0.9, bins = 30, 
          filled = FALSE, name.obj = NULL, plot = TRUE, title)

pe_param(obj = NULL, term = NULL, data = NULL, n.points = 100, 
          parameter = c("mu", "sigma", "nu", "tau"), 
          scenario = list(), how.scenario = c("median", "last", "fixed"), 
          col = "darkblue", linewidth = 1.3, name.obj = NULL, 
          rug.plot = TRUE, rug.col = "gray", rug.size = 0.5, 
          data.plot = FALSE, data.col = "lightblue", data.size = 0.1, 
          factor.size = 15, data.alpha = 0.9, bins = 30, 
          filled = FALSE, ylim = NULL, title, ...)

pe_eta(obj = NULL, term = NULL, data = NULL, n.points = 100, 
          parameter = c("mu", "sigma", "nu", "tau"), 
          scenario = list(), how.scenario = c("median", "last", "fixed"), 
          col = "darkblue", linewidth = 1.3, name.obj = NULL, 
          rug.plot = TRUE, rug.col = "gray", rug.size = 0.5, 
          data.plot = FALSE, data.col = "lightblue", data.size = 0.1, 
          factor.size = 15, data.alpha = 0.9, bins = 30, 
          filled = FALSE, ylim = NULL, title, ...)

pe_param_grid(model, terms, maxcol = 2, maxrow = 3, ylim = NULL, ...)

pe_eta_grid(model, terms, maxcol = 2, maxrow = 3, ylim = NULL, ...)


}
\arguments{
  \item{obj}{a GAMLSS fitted object}
   \item{model}{a GAMLSS fitted model}
  \item{term}{the model term we want to investigate can be one i.e. "Fl" or two 
  c("Fl", "A") }
  \item{terms}{This argument is for functions \code{pa_param_grid()} and \code{pa_eta_grid()}. It should be a list of  model terms, for example,  \code{list(c("Fl","A"), "H", "loc" ))}}
  \item{data}{the data frame used otherwise it takes it from the fitted model}
  \item{n.points}{the number of points for the evaluation of the term}
  \item{parameter}{the distribution parameter in which  the term is fitted}
  \item{type}{this is for functions  \code{pe_1_parameter()} and  \code{pe_2_parameter()}, it allowes to specify  the \code{"parameter"} or its  prediction, \code{"eta"}}
  \item{how.scenario}{how to set all the rest of the continuous terms in the model if are not specified by \code{scenario}. The altenatives are for "median", "last" (for time series) or "fixed"}
  \item{scenario}{this can be a list of values for the rest of the terms in the model for the specific distribution parameter}
  \item{plot}{whether to plot the result or ust ger the created \code{data.frame}}
  \item{col}{the colour of the partial effect of the term}
  \item{linewidth}{the size of the line of partial effect of the term}
  \item{bins}{the number of binds for the contour plot}
  \item{filled}{whether to display the values  in the contour}
  \item{title}{the title if different from the default}
  \item{name.obj}{this is a way to pass the name of the object}
  \item{maxcol}{the maximum columns in the grid plot}
  \item{maxrow}{the maximum rowss in the grid plot}
  \item{data.plot}{whether to plot the data}
  \item{rug.plot}{whether to print the rug bellow the figure}
  \item{rug.size}{the size of the rug}
  \item{rug.col}{the colour of the rug}
  \item{data.col}{the color of the data points}
  \item{data.size}{the size of the data points}
  \item{data.alpha}{the trnsparance constant of the data points}
  \item{factor.size}{the size of the symbol if a factor is plotted}
  \item{ylim}{if a common y limit is required}
  \item{centered.from}{whether to centered the result}
  \item{\dots}{for passing argument from the function \code{pe_param_grid} to the function \code{pe_param}}
}
\details{
The functions \code{pe_param()} and \code{pe_param_grid()} can be used to help the use the  interpretation of a GAMLSS model. 
The functions \code{pe_param()} provides the partial effect of one or two   terms  of a specified  parameter of the distribution while the rest of the terms in the model are set on specific values or scenarios. The function \code{pe_param()} calls \code{pe_1param()} if the argument terms is one i.e. "Fl" or the function 
\code{pe_2param()} if the terms are two i.e. c("Fl"","A"). 
The \code{pe_param_grid()} plots multiple plots specified by the list used in the \code{term} argument. 

Similar functions are 
\code{pe_quantile()} and \code{pe_moment()}. 
}
\value{
It is plotting the partial effect or is producing the resulting function
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

Stasinopoulos, M.D., Kneib, T., Klein, N., Mayr, A. and Heller, G.Z., (2024). \emph{Generalized Additive Models for Location, Scale and Shape: A Distributional Regression Approach, with Applications} (Vol. \bold{56}). Cambridge University Press.

(see also \url{https://www.gamlss.com/}).
}
\author{
Mikis Stasinopulos
}

\note{ The \code{data} if is  not declared is taken for from the fitted model.  So the \code{data.frame} used for fitting the model should exist in your working space.  Variables in the \code{data}  set not used in the fit could slow down the functions because they go thought a loop containg all variables (columns) in the  \code{data}.    

A \code{term} do not have to be in the model, (in which case his contribution will be flat), but it has to be in the \code{data}. 

}
\seealso{
\code{\link[gamlss]{getPEF}}
}
\examples{
library(gamlss)
m1 <- gamlss(R~pb(Fl)+pb(A)+loc+H, data=rent, gamily=GA)
pe_param(m1, "A")
pe_param(m1, c("Fl","A"), filled=TRUE)
pe_param_grid(m1, list(c("Fl","A"), c("H","loc")), filled=TRUE)
# the terms are additive no interaction
}

\keyword{ regression }
