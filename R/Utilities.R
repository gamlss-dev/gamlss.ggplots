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
  if (!missing(obj)&&(!inherits(obj, c("gamlss", "gamlss2")))) 
         stop("the model is not a gamlss model")  
  weights <- if (is(obj,"gamlss")) obj$weights else 
  {
    rqres <- residuals(obj)
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
  if  (!missing(obj)&&(!inherits(obj, c("gamlss", "gamlss2")))) 
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
  if  (!missing(obj)&&(!inherits(obj, c("gamlss", "gamlss2")))) 
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
  if  (!missing(obj)&&(!inherits(obj, c("gamlss", "gamlss2")))) 
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
  if  (!missing(obj)&&(!inherits(obj, c("gamlss", "gamlss2")))) 
    stop("the model is not a gamlss model") 
################################################################################
################################################################################
  if (inherits(obj, "gamlss"))
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
  if  (!missing(model)&&(!inherits(model, c("gamlss", "gamlss2")))) 
       stop("the model should be an gamlss object")  
  if (inherits(model, "gamlss"))
  {
    family <-  if(is.null(model$call$family)) as.gamlss.family(NO) 
               else as.gamlss.family(model$call$family)
     fname <- model$family[1]  
      type <- family$type
     param <- family$param
     nopar <- family$nopar
      dfun <- paste("d",fname,sep="")
      pfun <- paste("p",fname,sep="")
      qfun <- paste("q",fname,sep="")
     p_d_f <- eval(parse(text=dfun)) 
     c_d_f <- eval(parse(text=pfun))
     q_fun <- eval(parse(text=qfun))
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
    q_fun <- eval(family$q) 
  } 
list(fname=fname, type=type, nopar=nopar, param=param, dfun=dfun, pfun=pfun,
     p_d_f=p_d_f, c_d_f=c_d_f, q_fun=q_fun)  
}
################################################################################
################################################################################
################################################################################
################################################################################
# get_devianceIncr <- function(obj, newdata=NULL)
# {
# if  (!missing(obj)&&(!inherits(obj, c("gamlss", "gamlss2")))) 
#     stop("the model is not a gamlss model") 
# if (inherits(obj, c("gamlss")))
# {
# devianceIncr <- devianceIncr(obj, newdata=newdata)  
# } else 
# {
#   fam <- get_family(obj)
#   browser()
# }  
#   
# devianceIncr  
# }
################################################################################
################################################################################
################################################################################
################################################################################
find_power <- function(y, x, data = NULL,  profile=FALSE, k=2,  
                       from = 0, to=1.5, step=0.1)  
{
  cat("*** Checking for transformation for x ***", "\n") 
  ptrans<- function(x, p) if (abs(p)<=0.0001) log(x) else I(x^p)
  
  fn1 <- function(p, data=data)
  {
    m1 <- gamlss2(y~s(ptrans(x,p)), data=data, trace=FALSE)
    gamlss2::GAIC(m1, k=k)
  }

  if (profile) # profile dev
  {
    pp <- seq(from,to, step) 
    pdev <- rep(0, length(pp)) 
    for (i in 1:length(pp)) 
    {
      pdev[i] <- fn1(pp[i], data=data)  
    }
    plot(pdev~pp, type="l")
    points(pdev~pp,col="blue")
    par <- pp[which.min(pdev)]
    cat('*** power parameters ', par,"***"," \n") 
  } else
  {
    fn <- function(p, data=data)
      { m1 <- gamlss2(y~s(ptrans(x,p)), data=data, trace=FALSE)
        gamlss2::GAIC(m1, k=k)
      }
    par <- with(data, optimise(fn, lower=from, upper=to))
    cat('*** power parameters ', par[[1]],"***"," \n") 
  }  
  par
}
################################################################################
################################################################################
################################################################################
################################################################################




