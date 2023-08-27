################################################################################
################################################################################
################################################################################
################################################################################
# PARTITION 
################################################################################
################################################################################
# function 1
data_partition <- function(data, partition=2, probs, setseed=123, ...)
{
set.seed(setseed)
if (partition!=2&&partition!=3) stop("partition should be 2 or 3")
if(missing(probs))
{
    probs <- if (partition==2)  c(0.6,0.4) 
                        else    c(0.6,0.2,0.2)
} else probs <- probs
  if (length(probs)>4||length(probs)<=0) stop("the length of probs should be  2 o 3")
if (sum(probs)!=1) stop("probs should add up to 1")
if (partition==2)
{
     rand <- sample(2, dim(data)[1], replace=TRUE, prob=probs)
    train <- subset(data, rand==1)
     test <- subset(data, rand==2)
      out <- list(train=train , test=test, partition=rand)
    return(out)
}
if (partition==3)
{
     rand <- sample(3, dim(data)[1], replace=TRUE, prob=probs)
    train <- subset(data, rand==1)
    valid <- subset(data, rand==2)
     test <- subset(data, rand==3)
      out <- list(train=train, valid=valid, test=test, partition=rand)
    return(out)              
  }  
}
################################################################################
################################################################################
################################################################################
################################################################################
# a=data_partition(rent)
# mosaicplot(table(a$partition))
################################################################################
################################################################################
################################################################################
################################################################################
# function 2
# save a data frame with partition as factor
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
if (length(probs)>2||length(probs)<=0) stop("the length of probs should be 2")
    rand <- sample(2, dim(data)[1], replace=TRUE, prob=probs)
    rand <- factor(rand, labels=c("train", "test"))
     out <- data.frame(data, partition=rand)
invisible(out)
}
if (partition==3L)
{
  cat("data partition into three sets", "\n")
    probs <- if  (missing(probs)) c(0.6,0.2,0.2)
             else probs
if (length(probs)>4||length(probs)<=0) stop("the length of probs should be  3")
    rand <- sample(3, dim(data)[1], replace=TRUE, prob=probs)
    rand <- factor(rand, labels=c("train", "valid", "test"))
     out <- data.frame(data, partition=rand)
    invisible(out)              
}  
if (partition>=4L)
  cat( paste0(partition,"-fold data partition"), "\n")
    probs <- rep(1/partition, partition) 
    rand <- sample(partition, dim(data)[1], replace=TRUE, prob=probs)
    rand <- factor(rand)
    out <- data.frame(data, partition=rand)
    invisible(out)    
}
################################################################################
################################################################################
################################################################################
################################################################################
#mosaicplot(table(da$partition))






