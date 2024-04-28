################################################################################
################################################################################
################################################################################
################################################################################
resid_density <- function(obj, resid,
                          hist.col = "black", 
                          hist.fill = "white",
                          dens.fill = "#FF6666",
                          title)
{
################################################################################
  # local functions 
  gamlss_prep_data <- function (obj) 
  {
    rqres <- get_residuals(obj)
  weights <- get_weights(obj) 
      obs <- seq_len(length(rqres))
      obs <- obs[weights!=0]
    rqres <- rqres[weights!=0]
      out <- data.frame(obs = obs, rqres = rqres)
    return(out)
  }  
################################################################################
  other_prep_data <- function (resid) 
  {
    rqres <- resid
      obs <- seq_len(length(rqres))
      obs <- obs[!is.na(resid)]
    rqres <- rqres[!is.na(resid)]
      out <- data.frame(obs = obs, rqres = rqres)
    return(out)
  }  
################################################################################ 
   rqres <- NULL 
if (missing(obj)&&missing(resid))  stop("A GAMLSS fitted object or the argument resid should be used")
if (!missing(obj)&&!(is.gamlss(obj)|is(obj, "gamlss2"))) 
  stop("the model is not a gamlss model")
        d <- if (missing(obj)) other_prep_data(resid) 
             else             gamlss_prep_data(obj) 
txt.title <- if (missing(title))   
  paste("Quantile residuals of model",deparse(substitute(obj)))
            else title
        f <- d[d$color == "outlier", c("obs", "rqres")]
colnames(f) <- c("observation", "quan_resid")
      gg <- ggplot(d, aes(x=rqres))+
  geom_histogram(aes(y=after_stat(density)),binwidth = 0.2, colour=hist.col, 
                   fill=hist.fill)+
    geom_density(alpha=0.2, fill=dens.fill)+
    xlab("Quantile Residuals") + 
    ylab("density") + 
    ggtitle(txt.title) 
  return(gg)
}
################################################################################
################################################################################
################################################################################
################################################################################
# function to be use with gamlss.ggplots 
# This function is getting the weights of a fitted gamlss model
# whether is gamlss or gamlss2
# is needs the weigjhts to weighted out obsrvation with zero weights
get_weights <- function(obj)
{
  rqres <- residuals(obj)
  if (!missing(obj)&&!(is.gamlss(obj)|is(obj, "gamlss2"))) 
    stop("the model is not a gamlss model")  
  weights <- if (is(obj,"gamlss")) obj$weights else 
  {
    if (is.null(model.weights(model.frame(obj)))) rep(1,length(rqres)) 
    else model.weights(model.frame(obj)) 
  }   
  weights  
}
################################################################################
################################################################################
################################################################################
################################################################################
# the function residuals(obj) produce different sizes if there are weights 
# equal to zero  get_residual() meke sure that both gamlss and gamlss2 residual 
# are thesame length   
get_residuals <- function(obj)
{
  if (!missing(obj)&&!(is.gamlss(obj)|is(obj, "gamlss2"))) 
    stop("the model is not a gamlss model")  
  residuals <- if (is(obj,"gamlss")) obj$residuals else residuals(obj)
  residuals
}
################################################################################
################################################################################
################################################################################
################################################################################
get_fitted_mu <- function(obj)
{
  if (!missing(obj)&&!(is.gamlss(obj)|is(obj, "gamlss2"))) 
    stop("the model is not a gamlss model")  
  fv <- if (is(obj,"gamlss")) obj$mu.fv 
       else fitted(obj, type="parameter", what="mu")
fv
}
################################################################################
################################################################################
################################################################################
################################################################################
get_fitted_param <- function(obj, parameter="mu")
{
  if (!missing(obj)&&!(is.gamlss(obj)|is(obj, "gamlss2"))) 
    stop("the model is not a gamlss model")  
  fv <- if (is(obj,"gamlss")) fitted(obj, parameter=parameter) 
  else fitted(obj, type="parameter", what=parameter)
  fv
}
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
get_quantile <- function (obj,
                           quantile = 0.5,
                             newdata) 
{
################################################################################
################################################################################
  if (is(obj, "gamlss"))
{
      pdf <- obj$family[1]
    binom <- pdf%in%gamlss::.gamlss.bi.list # whether binomial
     qfun <- paste("q", obj$family[[1]],sep="")
     lpar <- eval(parse(text=pdf))()$nopar
if (binom) {bd <- obj$bd ; Y <- obj$y}
      pp <-  if (missing(newdata)) predictAll(obj,output="matrix")
             else  predictAll(obj, newdata=newdata, output="matrix")
    median <- switch(lpar, 
                     eval(call(qfun, p= quantile, mu=pp[,"mu"])),       # 1
                     eval(call(qfun, p= quantile, mu=pp[,"mu"], sigma=pp[,"sigma"])),   
                     eval(call(qfun, p= quantile, mu=pp[,"mu"], sigma=pp[,"sigma"],  
                               nu=pp[,"nu"])),  # 3                   
                     eval(call(qfun, p= quantile, mu=pp[,"mu"], sigma=pp[,"sigma"],  
                               nu=pp[,"nu"], tau=pp[,"tau"])))
  } else # if gamlss2
  {
      pdf <- obj$family[1]
    binom <- pdf%in%gamlss::.gamlss.bi.list # whether binomial
     qfun <- paste("q", obj$family[[1]],sep="")
     lpar <- eval(parse(text=pdf))()$nopar
if (binom) {bd <- obj$bd ; Y <- obj$y}
       pp <-  predict(obj, type="parameter") 
   median <- obj$family$q(p=quantile, par=pp)
  }  
  median  
}
################################################################################
################################################################################
################################################################################
################################################################################









################################################################################