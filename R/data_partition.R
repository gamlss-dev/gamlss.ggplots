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
    else c(0.6,0.2,0.2)
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
