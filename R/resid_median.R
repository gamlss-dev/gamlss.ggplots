################################################################################
################################################################################
################################################################################
################################################################################
resid_median <- function (obj, resid, plot = TRUE, value=3, title, 
                          annotate=TRUE) 
{
# Note that I am taking the 
# obj$resid rather resid(obj) so I can preserve the no of the observations
################################################################################
# local functions 
gamlss_prep_data <- function (obj, value=2) 
{
    sdres <- get_residuals(obj)
       fv <- get_quantile(obj, quantile=0.5)
  weights <- get_weights(obj)
       obs <- seq_len(length(sdres))
      obs <- obs[weights!=0]
    sdres <- sdres[weights!=0]
       fv <- fv[weights!=0]
      out <- data.frame(obs = obs, sdres = sdres, fv)
out$color <- ifelse(((out$sdres >= value) | (out$sdres <= -value)), 
                        c("outlier"), c("normal"))
out$fct_color <- ordered(factor(out$color), levels = c("normal", "outlier"))
  out$txt <- ifelse(out$color == "outlier", out$obs, NA)
  return(out)
  } 
################################################################################
  # the main function starts here  
if (missing(obj))  stop("A GAMLSS fitted object should be used")
  if (!missing(obj)&&!(is.gamlss(obj)|is(obj, "gamlss2"))) stop("the model is not a gamlss model")
        d <- gamlss_prep_data(obj,   value=value) 
txt.title <- if (missing(title))  paste("Residuals & fitted vals of model",deparse(substitute(obj)))
else title
#      obs <- NULL
    sdres <- NULL
      txt <- NULL
        f <- d[d$color == "outlier", c("obs", "sdres")]
colnames(f) <- c("observation", "quan_resid")
# try colors() for different colors
  p <- ggplot2::ggplot(d, ggplot2::aes(x = fv, y = sdres, label = txt, 
                                       ymin = 0, ymax = sdres)) + 
    ggplot2::geom_linerange(colour =  "steelblue4" ) + 
    ggplot2::geom_point(shape = 1, colour = "steelblue4"  ) + 
    ggplot2::geom_hline(yintercept = 0, colour = "gray") + 
    ggplot2::geom_hline(yintercept = c(value, -value), colour = "red") + 
    ggplot2::xlab("mu fitted values") + 
    ggplot2::ylab("Quantile Residuals") + 
    ggplot2::ggtitle(txt.title) + 
    ggplot2::geom_text(hjust = -0.2, nudge_x = 0.15, size = 3, family = "serif", 
              fontface = "italic", colour = "darkred", na.rm = TRUE) + 
      if(annotate) ggplot2::annotate("text", x = Inf, y = Inf, hjust = 1.5, 
                                     vjust = value, 
             family = "serif", fontface = "italic", colour = "darkred", 
             label = paste0("Threshold: abs(", value, ")"))
  if (plot) {
    suppressWarnings(return(p))
  }
  else {
    return(list(plot = p, outliers = f, threshold = value))
  }
}
################################################################################
################################################################################
################################################################################
################################################################################