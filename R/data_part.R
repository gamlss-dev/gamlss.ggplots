################################################################################
################################################################################
################################################################################
################################################################################
# PARTITION 
################################################################################
################################################################################
# function 1 saving data with a partition factor
################################################################################
data_part <- function(data, partition=2L, probs, setseed=123, ...)
{
  set.seed(setseed)
if (partition<=1L) stop("data partition should be greater that one","\n")
if (partition>=20L) stop("data partition should be less that 20","\n")
if (partition==2L)
{
  cat("data partition into two sets", "\n")
   probs <-   if (missing(probs)) c(0.6,0.4) 
             else probs
if (sum(probs)!=1) stop("probs should add up to 1")   
if (length(probs)>2||length(probs)<=0) stop("the length of probs should be 2")
    rand <- sample(2, dim(data)[1], replace=TRUE, prob=probs)
    rand <- factor(rand, labels=c("train", "test"))
     out <- data.frame(data, partition=rand)
invisible(return(out))
}
if (partition==3L)
{
  cat("data partition into three sets", "\n")
    probs <- if  (missing(probs)) c(0.6,0.2,0.2)
             else probs
if (sum(probs)!=1) stop("probs should add up to 1")    
if (length(probs)>4||length(probs)<=0) stop("the length of probs should be  3")
    rand <- sample(3, dim(data)[1], replace=TRUE, prob=probs)
    rand <- factor(rand, labels=c("train", "valid", "test"))
     out <- data.frame(data, partition=rand)
    invisible(return(out))              
}  
if (partition>=4L)
  cat( paste0(partition,"-fold data partition"), "\n")
    probs <- rep(1/partition, partition) 
    if (sum(probs)!=1) stop("probs should add up to 1")    
    rand <- sample(partition, dim(data)[1], replace=TRUE, prob=probs)
    rand <- factor(rand)
    out <- data.frame(data, partition=rand)
    invisible(return(out))    
}
#mosaicplot(table(da$partition))
################################################################################
################################################################################
################################################################################
################################################################################
data_indexing <- function(data, kfolds=2, bootstrap=FALSE)
{
  dD <- dim(data)
  if (bootstrap)  
  {
    mm <-    lapply(as.data.frame(t(sapply(sample(rep_len(1:dD[1], length.out= dD[1]), replace=TRUE),"!=",1:kfolds))), which)    
  } else 
  {
    mm <-    lapply(as.data.frame(t(sapply(sample(rep_len(1:kfolds, length.out= dD[1]), replace=FALSE),"!=",1:kfolds))), which) 
  } 
  mm
}
################################################################################
################################################################################
################################################################################
################################################################################
data_Kfold <- function(data, K=6, setseed=123 )
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
################################################################################
data_boot <- function(data, K=10, setseed=123 )
{
  set.seed(setseed)
  nfolds <- K
  n <- dim(data)[1]
  BOOTfolds<- lapply( 
    as.data.frame(
      matrix(
        sample(1:n, nfolds*n, replace=TRUE)
        , n)
    ),
    sort) 
  BOOTfolds
}
################################################################################
################################################################################
################################################################################
################################################################################







################################################################################
################################################################################
################################################################################
################################################################################            
# type none

# nofunction <- function(x)
# {
#   
# if (type=="first.order")
# {
# if (nonlinear)
#   {
#      daC <- data[,-c(pp, pos_res)]
#      ndc <- names(daC)
#     lndc <- length(ndc)
#     args <- deparse(substitute(arg))
#     newn <- paste0(basis, "(", ndc, ",", args, ")")
#     mindex <- match(ndc,x_Names)
#     qq <- character(length=lndc)
#     for (i in 1:lndc) qq[i] <- gsub(ndc[i], newn[i], x_Names[mindex[i]])
#     x_Names[mindex] <- qq
#     formula <- as.formula(paste(paste0(response_t,"~"), 
#                                 paste0(paste0("(",paste(x_Names, collapse='+')), ")^2")))  
#     XX <- model.matrix(formula, weights=weights, data=data)[,-1]
#     d2 <- dim(XX)[2]
#     Names <- character(d2)
#     for (i in 1:d2) 
#     {
#       Names[i] <-  gsub(":", ".", colnames(XX)[i])
#     }
#     #for (i in 1:length(Names)) colnames(XX)[grep(ndc[i], colnames(XX))] <- paste0(ndc[i], 1:pp)      
#     colnames(XX) <- Names
#     ## create a data frame  
#     ## 
#     #for (i in 1:lndc) colnames(XX)[grep(ndc[i], colnames(XX))] <- paste0(ndc[i], 1:pp)   
#     dXX <-  as.data.frame(XX)
#     dXX[, response_t] <- data[, response_t]
#     return(dXX)
#   } else 
#   {
#           
#      formula <- as.formula(paste(paste0(response_t,"~"), 
#                                 paste0(paste0("(",paste(x_Names, collapse='+')), ")^2"))) 
#                XX <- model.matrix(formula, weights=weights, data=data)[,-1]
#                d2 <- dim(XX)[2]
#             Names <- character(d2)
#     for (i in 1:d2) 
#     {
#          Names[i] <-  gsub(":", ".", colnames(XX)[i])
#     }
#      colnames(XX) <- Names
#               dXX <-  as.data.frame(XX)
# ## in order to have a complete data frame we need also the response 
# dXX[, response_t] <- data[, response_t]
#     return(dXX)
#   }  
# } # type= first order 
#   
# }
# }
################################################################################
################################################################################
################################################################################
################################################################################
# old function from 2020
data.partition <- function(data, partition=2, probs, setseed=123, ...)
{
  set.seed(setseed)
  if (partition!=2&&partition!=3) stop("partition should be 2 or 3")
  if(missing(probs))
  {
    probs <- if (partition==2)  c(0.6,0.4) 
    else c(0.6,0.2,0.2)
  } else probs <- probs
  if (length(probs)>4||length(probs)<=0) stop("the length of probs should be  2 o 3")
  if (sum(probs)!=1) stop("probs should add up to 1")
  if (partition==2)
  {
    rand <- sample(2, dim(data)[1], replace=TRUE, prob=probs)
    train <- subset(data, rand==1)
    test <- subset(data, rand==2)
    out <- list(train=train , test=test)
    return(out)
  }
  if (partition==3)
  {
    rand <- sample(3, dim(data)[1], replace=TRUE, prob=probs)
    train <- subset(data, rand==1)
    valid <- subset(data, rand==2)
    test <- subset(data, rand==3)
    out <- list(train=train, valid=valid, test=test)
    return(out)              
  }  
}








########

#data_rm(rent99, c("rentsqm", "district"))
#data_rm(rent99, c(2, 9))


