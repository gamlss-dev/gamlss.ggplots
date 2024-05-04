################################################################################
################################################################################
################################################################################
# utilities function for gamlss2
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
       pp <- if (missing(newdata)) predictAll(obj,output="matrix")
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
get_family <- function (model)
{
  if (!(is.gamlss(model)||is(model,"gamlss2"))) stop("the model should be an gamlss object")  
  if (is.gamlss(model))
  {
    family <-  if(is.null(model$call$family)) as.gamlss.family(NO) 
               else as.gamlss.family(model$call$family)
     fname <- model$family[1]  
      type <- family$type
     param <- family$param
     nopar <- family$nopar
      dfun <- paste("d",fname,sep="")
      pfun <- paste("p",fname,sep="")
     p_d_f <- eval(parse(text=dfun)) 
     c_d_f <- eval(parse(text=pfun)) 
  } else 
  {
   family <- model$family
    fname <- family$family
     type <- family$type
    param <- family$names
    nopar <- length(param)
    dfun <- paste("d",fname,sep="")
    pfun <- paste("p",fname,sep="")
    p_d_f <- eval(family$d) 
    c_d_f <- eval(family$p) 
  } 
list(fname=fname, type=type, nopar=nopar, param=param, dfun=dfun, pfun=pfun,
     p_d_f=p_d_f, c_d_f=c_d_f)  
}
################################################################################
################################################################################
################################################################################
################################################################################





