# MIKIS STASINOPOULOS
# 20-10-2022
##################################################################### 
##################################################################### 
# to do
# i) IT NEEDS CLEARING UP done
# ii) also to exlcude variables from the data done
# iii) how to scale is important we should have options
#       scale = c("none", "z-scores", "0to1", "norm")  
####################################################################
####################################################################
#################################################################### 
# this function creates a data frame with main or first order effects
# so the continuous x's will be scale and the interaction will be on 
# the scaled variables
# the factors are left as dummies
# 
#####################################################################
#####################################################################
##################################################################### 
data_stand<- function(data, response, 
                 exclude = NULL,
                   scale = c("z-scores", "0to1"),  
                  family = SHASHo,
                    type = c("main.effect", "first.order"),
                 weights = NULL)
{
# all the variable in data are used to create the data.frame
# unless the 'exclude' is used
# the argument `data' is compulsory 
# the argument `response' is compulsory  
# scale:  the type of scalling 
#  i) "z-scores" using SHASH
#  ii) 0 to 1 
#  iii) norm NON
################################################################################
scale0to1 <- function(x)
  {
       x <- as.matrix(x)
      nc <- ncol(x)
    minX <- apply(x, 2L, min, na.rm=TRUE )
    maxX <- apply(x, 2L, max, na.rm=TRUE )
       Y <- sweep(x, 2L, minX, check.margin = FALSE)
       Y <- sweep(Y, 2L, maxX-minX, FUN="/",  check.margin = FALSE)
    attr(Y, "min") <- minX
    attr(Y, "max") <- maxX
    Y
  } 
  #-----------------------------------------------
  unscale0to1 <- function(x) 
  {
    minX <- attributes(x)$min
    maxX <- attributes(x)$max
    Y <- sweep(x, 2L, (maxX-minX), FUN="*", check.margin = FALSE)
    Y <- sweep(Y, 2L, minX, FUN="+", check.margin = FALSE)
    attr(Y, "min") <- NULL
    attr(Y, "max") <- NULL
    Y
  }
  #----------------------------------------------- 
################################################################################
if (missing(data)) stop("the data frame is missing")  
if (missing(response)) stop("response should be given")
if (!is.null(exclude))  data <- data[, setdiff(names(data),exclude)]    
           type <- match.arg(type)  
          scale <- match.arg(scale)
     response_t <- deparse(substitute(response)) 
        pos_res <- match(response_t, names(data))
        x_Names <- names(data)[-pos_res]  
            daT <- data[,x_Names]
  whetherFactor <- sapply(daT, is.factor)|
                   sapply(daT, is.character)|
                   sapply(daT, is.logical)|
                   data_distinct(data[,-pos_res]) <10|
                   sapply(data[,-pos_res], function(x) is(x, class2="Date"))
     theFactors <- x_Names[whetherFactor]
             pp <- unlist(sapply(theFactors, grep, names(data)))
## scale only the continuous variables 
if (scale=="z-scores") 
         {
     DF1 <- data_zscores(data[,-c(pp, pos_res)], plot=FALSE, family=family)
data[,names(DF1)] <- DF1 
         }
if (scale=="0to1")             
         {
           DF1 <- scale0to1(data[,-c(pp, pos_res)])
           data[,colnames(DF1)] <- DF1
         }
## get the formula according to type 
if (type=="main.effect")
  {
        formula <- as.formula(paste(paste0(response_t,"~"), paste(x_Names, collapse='+'))) 
   } 
if (type=="first.order")
  {
    formula <- as.formula(paste(paste0(response_t,"~"), 
                    paste0(paste0("(",paste(x_Names, collapse='+')), ")^2"))) 
  }
# take the response out so you can scale the rest     
     XX <- model.matrix(formula, weights=weights, data=data)[,-1]
if (type=="first.order")
{
     d2 <- dim(XX)[2]
  Names <- character(d2)
  for (i in 1:d2) 
  {
    Names[i] <-  gsub(":", ".", colnames(XX)[i])
  }
 colnames(XX) <- Names
}
## create a data frame     
     dXX <-  as.data.frame(XX)
## in order to have a complete data frame we need also the response 
     dXX[, response_t] <- data[, response_t]
dXX
}
#####################################################################
#####################################################################
#####################################################################
gnet_path <- function(model, parameter="mu")
{
  if (!(class(getSmo(model, parameter)[[1]])[1]%in%c("elnet", "lars")))
       stop("this is not a relevant model")
  if (class(getSmo(model, parameter)[[1]])[1]=="elnet")
  {
    plot(getSmo(model, parameter)[[1]], xvar="lambda")
   if (!is.null(getSmo(model,parameter)[[2]]$optlambda)) abline(v=log(getSmo(model,parameter)[[2]]$optlambda),  col="gray")
  } else 
  {
    plot(getSmo(model, parameter)[[1]], xvar="df")
    abline(v=getSmo(model,parameter)[[2]]$df,  col="red")
  }  
}
#####################################################################
#####################################################################
#####################################################################
gnet_terms <- function(model, parameter="mu")
{
  lout = length(getSmo(model, parameter))
  if (!(class(getSmo(model, parameter)[[1]])[1]%in%c("elnet", "lars")))
    stop("this is not a relevant model")
  getSmo(model, parameter)[[lout]]$beta[getSmo(model, parameter)[[lout]]$beta!=0]
}
#####################################################################
#####################################################################
#####################################################################
gnet_coef <- function(model, parameter="mu")
{
  lout = length(getSmo(model, parameter))
  if (!(class(getSmo(model, parameter)[[1]])[1]%in%c("elnet", "lars")))
    stop("this is not a relevant model")
  getSmo(model, parameter)[[lout]]$beta
}
######################################################################
######################################################################
######################################################################
gnet_df <- function(model, parameter="mu")
{
  lout = length(getSmo(model, parameter))
  if (!(class(getSmo(model, parameter)[[1]])[1]%in%c("elnet", "lars")))
    stop("this is not a relevant model")
  getSmo(model, parameter)[[lout]]$df
}
######################################################################
######################################################################
######################################################################
# old code
# X <- as.matrix(model.frame(flow~time+month+nino+soi+eof1+eof2+logf1+logf2+ logf3+logf4+logf5 , data=da1)[,-1])
#X1 <- model.matrix(flow ~time+month+nino+soi+eof1+eof2+logf1+logf2+ logf3+logf4+logf5 , data=da1)[,-1]
#identical(X,X1)
# the problem is that we do not want to scale the factors  
# pp contains the position of the factor in the 
#    pp <- unlist(sapply(theFactors, grep, colnames(XX)))
# scale only the continuous variables 
#     if (scale) XX[,-pp] <- scale(XX[,-pp])
# if (type=="second.order")
#      {
#       formula <- as.formula(paste(paste0(response_t,"~"), 
#                     paste0(paste0("(",paste(x_Names, collapse='+')), ")^3"))) 
#      }   
################################################################################
################################################################################
################################################################################
get_kfolds <- function(data, K=6, setseed=123 )
{
  set.seed(setseed)
  nfolds <- K
       n <- dim(data)[1]
# folds for cross-validation 
CVfolds <-  lapply(
       as.data.frame(
                    t(
                  sapply(
          sample(rep_len(1:nfolds,length.out=n),replace=FALSE)
          ,"!=", 1:nfolds)
                     )
                    )
          , which )   
CVfolds
}
################################################################################
################################################################################
################################################################################ 

