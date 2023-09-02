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
# function 2 removing variables with only one value
################################################################################ 
data_rm1val <- function(data) 
{
  # what is the data
  if (is(data, "list"))  stop("the data is list  the function needs a data.frame") 
  if (is(data, "table")) stop("the data is a table the function needs a data.frame")
  if (is(data, "matrix"))    data <- as.data.frame(data)
  if (is(data[1],"mts"))     data <- as.data.frame(data)
  if (is(data, "array")) stop("the data is an array the function needs a data.frame")  
  Names <- names(data)
  PP <- list()
  for (i in 1:length(Names))
  {
    PP[[i]] <- length(table(data[,Names[i]]))
  }
  pp <- unlist(PP)
  names(pp) <- Names
  if (any(pp==1))
  {
    w1val <-  which(pp==1)
    removed <- names(data)[w1val]
    data[,w1val] <- NULL 
  }
  cat("the var", removed, "has been removed \n")
  invisible(data)
}
################################################################################
################################################################################
################################################################################
################################################################################
data_rm <- function(data, vars)
{
  if (is(data, "list"))  
    stop("the data is list  the function needs a data.frame") 
  if (is(data, "table")) 
    stop("the data is a table the function needs a data.frame")
  if (is(data, "matrix"))    data <- as.data.frame(data)
  if (is(data[1],"mts"))     data <- as.data.frame(data)
  if (is(data, "array"))   
    stop("the data is an array the function needs a data.frame")
  if (is.character(vars))
  {
    da <- subset(data, select=setdiff(names(data), vars))  
  } else
  {
    da <- subset(data, select=setdiff(1:dim(data)[2], vars)) 
  }  
  da
}  
################################################################################
################################################################################
################################################################################
################################################################################





