################################################################################
################################################################################
################################################################################
################################################################################
# OUTLIERS function 
################################################################################
################################################################################ 
# function 1
y_outliers <- function(var, value=4, family=SHASH)
{
  if (!is(var,"numeric")) stop("the variable should be numeric")
  # the problem is integer is numeric so do not stop them
  if (any(var<0))
  {
    tvar <- var 
  } else
  {
    par  <- y_power_trans(var)
    tvar <- if (abs(par) < 0.001) log(var) else var^par
  }  
  z.scores <- y_zscores(tvar, family=family, plot=FALSE)
      ival <- var[which(abs(z.scores)>value)]
  iind <- which(abs(z.scores)>value)
  names(ival) <- iind
  ival
}
################################################################################
################################################################################
################################################################################
################################################################################
# function 3
data_outliers <- function(data, 
                          value = 4,
                          min.distinct = 50, 
                          family = SHASHo)
{
  # what is the data
  if (is(data, "list"))  stop("the data is list  the function needs a data.frame")
  if (is(data, "table")) stop("the data is a table the function needs a data.frame")
  if (is(data, "matrix")) data <- as.data.frame(data)
  if (is(data[1],"mts"))  data <- as.data.frame(data)
  if (is(data, "array")) stop("the data is an array the function needs a
                            data.frame")
  dimD <- dim(data)
  # checking data  
  if (is.null(dimD)) stop("only one variable in the data") 
  if (dimD[1]<20)   stop(cat("the size of the data set is too small", "\n",
                             "to detect non-linear correlations", "\n"))  
  sat.cont <- sapply(data,is.factor)|sapply(data,is.character)|
    data_distinct(data, get.distinct=TRUE) < min.distinct|
    sapply(data, function(x) is(x, class2="Date"))
  daTa <- subset(data,  select=!sat.cont)  
  #daTa <- subset(data,  select=ifelse(sapply(data,is.factor)|
  #            sapply(data,is.character)==TRUE, FALSE, TRUE))
  Dim <- dim(daTa)
  if (Dim[2]==0) stop("no variable is left after taking out the factors")         
  if (Dim[2]==1) stop("only one variable is left after taking out the factors")   
  Names <- names(daTa)
  class_Vars <- sapply(daTa,class)
  PP <- list()
  for (i in 1:length(class_Vars))
  { 
    PP[[i]] <-  y_outliers(daTa[,i], value=value, family=family)
  }
  names(PP) <- Names       
  PP 
}
################################################################################
################################################################################
################################################################################
################################################################################