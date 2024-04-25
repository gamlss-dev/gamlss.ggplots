################################################################################
################################################################################
################################################################################
################################################################################
resid_param <- function (obj,
                        param = c("mu", "sigma", "nu", "tau"), 
                        title, 
                     line.col = "darkred", 
                    point.col = "steelblue4",
                  point.shape = 20
                         ) 
{#furTob-vixzi3-hudhyv
################################################################################
# local functions 
gamlss_prep_data <- function (obj, param) 
  {
             FV <- get_fitted_param(obj, param)
        weights <- get_weights(obj) 
           yVal <- get_residuals(obj)
            obs <- seq_len(length(FV))
             FV <- FV[weights!=0]
            obs <- obs[weights!=0]
           yVal <- yVal[weights!=0]
            out <- data.frame(obs = obs, resid = yVal, fv=FV)
  return(out)
  } 
################################################################################
################################################################################
# the main function starts here  
if (missing(obj))  stop("A GAMLSS fitted object should be used")
if (!missing(obj)&&!(is.gamlss(obj)|is(obj, "gamlss2"))) 
  stop("the model is not a gamlss model")
if (missing(param)) 
       param <- "mu" 
       param <- if (is(obj, "gamlss"))  match.arg(param)
        else
        {
          param <-  match.arg(param, obj$family$names)
        }
           d <- gamlss_prep_data(obj, param) 
           y <- NULL
        corr <- with(d,cor(resid,fv)) 
   txt.title <- if (missing(title))  paste(paste0("r = ",sprintf("%.3f",corr )))
                else title    
          pp <- try(obj$call$formula[[2]], silent=TRUE)
  txt_ylabel <-  "residuals" 
  txt_xlabel <- param
          gg <- ggplot2::ggplot(d, ggplot2::aes(x = fv, y = resid)) + 
            ggplot2::geom_point(shape = point.shape, colour = point.col) + 
            ggplot2::geom_smooth(method="gam",col=line.col)+
            ggplot2::xlab(txt_xlabel) + # working  with facet_wrap 
            ggplot2::ylab(txt_ylabel) + # working  with facet_wrap 
            ggplot2::ggtitle(txt.title)   # working  with facet_wrap 
    suppressWarnings(return(gg))
}
#resp_fitted(m6)+facet_wrap(~ cut_number(rent$A, 6))
################################################################################
################################################################################
################################################################################
################################################################################
resid_quantile <- function (obj, 
                         quantile = 0.5,  
                         title, 
                         newdata,
                         line.col = "darkred", 
                        point.col = "steelblue4",
                      point.shape = 20
) 
{
################################################################################
# local functions 
gamlss_prep_data <- function (obj, median) 
  {
     FV <- median
    obs <- seq_len(length(FV))
weights <- get_weights(obj) 
     FV <- FV[weights!=0]
    obs <- obs[weights!=0]
   yVal <- get_residuals(obj)[weights!=0]
    out <- data.frame(obs = obs, y = yVal, fv=FV)
    return(out)
} 
################################################################################
################################################################################
# the main function starts here  
 if (missing(obj))  stop("A GAMLSS fitted object should be used")
if (!missing(obj)&&!(is.gamlss(obj)|is(obj, "gamlss2"))) stop("the model is not a gamlss model")
################################################################################
################################################################################
    median <-  get_quantile(obj, quantile, newdata)
         d <- gamlss_prep_data(obj, median) 
#color <- obs <-  hat <- 
         y <- NULL
      corr <- with(d,cor(y,fv)) 
 txt.title <- if (missing(title))  paste(paste0("r = ",sprintf("%.3f",corr )))
              else title    
        pp <- try(obj$call$formula[[2]], silent=TRUE)
txt_ylabel <-  if (any(class(pp)%in%"try-error")) txt_ylab = "response" 
               else  paste(obj$call$formula[[2]])
txt_xlabel <-  paste(quantile, "quantile", sep=" ")
        gg <- ggplot2::ggplot(d, ggplot2::aes(x = fv, y = y)) + 
          ggplot2::geom_point(shape = point.shape, colour = point.col) + 
          ggplot2::geom_smooth(method="gam",col=line.col)+
          ggplot2::xlab(txt_xlabel) + # working  with facet_wrap 
          ggplot2::ylab(txt_ylabel) + # working  with facet_wrap 
          ggplot2::ggtitle(txt.title)   # working  with facet_wrap 
suppressWarnings(return(gg))
}
# resp_quantile(m6)+facet_wrap(~ cut_number(rent$A, 6))
################################################################################
################################################################################
################################################################################
################################################################################