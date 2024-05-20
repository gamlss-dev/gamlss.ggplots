################################################################################
################################################################################
################################################################################
################################################################################
resp_mu <- function (obj, title, 
                             line.col = "darkred", 
                            point.col = "steelblue4",
                          point.shape = 20
                         ) 
{
################################################################################
# local functions 
  gamlss_prep_data <- function (obj) 
  {
         FV <- get_fitted_param(obj, parameter="mu")
    weights <- get_weights(obj)
   response <- get_response(obj)
        obs <- seq_len(length(FV))
         FV <- FV[weights!=0]
        obs <- obs[weights!=0]
       yVal <- response[weights!=0]
        out <- data.frame(obs = obs, y = yVal, fv=FV)
    return(out)
  } 
################################################################################
################################################################################
# the main function starts her
  if (missing(obj))  stop("A GAMLSS fitted object should be used")
  if (!inherits(obj, c("gamlss", "gamlss2"))) stop("the model is not a gamlss model")
          # N <- obj$N
           d <- gamlss_prep_data(obj) 
      #color <- obs <-  hat <- 
      y <- NULL
#           f <- d[d$color == "outlier", c("obs", "hat")]
# colnames(f) <- c("observation", "quan_resid")
# try colors() for different colors
#facet_wrap(~ cut_number(rent$A, 6))
     corr <- with(d,cor(y,fv)) 
txt.title <- if (missing(title))  paste(paste0("r = ",sprintf("%.3f",corr )))
               else title    
pp <- try(obj$call$formula[[2]], silent=TRUE)
txt_ylabel <-  if (any(class(pp)%in%"try-error")) txt_ylab = "response" 
              else  paste(obj$call$formula[[2]])
       gg <- ggplot2::ggplot(d, ggplot2::aes(x = fv, y = y)) + 
             ggplot2::geom_point(shape = point.shape, colour = point.col) + 
             ggplot2::geom_line(ggplot2::aes(x = fv, y = fv), col=line.col)+
             ggplot2::xlab("mu fitted values") + # working  with facet_wrap 
             ggplot2::ylab(txt_ylabel) + # working  with facet_wrap 
             ggplot2::ggtitle(txt.title)   # working  with facet_wrap 
    suppressWarnings(return(gg))
}
#resid_plot(r1, no.lines=T)+facet_wrap(~ cut_number(rent$A, 6))
################################################################################
################################################################################
################################################################################
################################################################################
