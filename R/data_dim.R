################################################################################
################################################################################
################################################################################
################################################################################
# DIM, NAMES 
# functions 
# i)   data_dim()
# ii)  data_which_na()
# iii) data_names()
# iv)  data_sorter_names()
################################################################################
################################################################################
# function 1
data_dim <- function(data)
{
  # what is the data
  if (is(data, "list"))  stop("the data is list  the function needs a data.frame") 
  if (is(data, "table")) 
    stop("the data is a table the function needs a data.frame")
  if (is(data, "matrix"))    data <- as.data.frame(data)
  if (is(data[1],"mts"))     data <- as.data.frame(data)
  if (is(data, "array")) 
    stop("the data is an array the function needs a data.frame") 
  # checking data 
  DIM <- dim(data)
  cat("**************************************************************",  "\n")  
  cat("**************************************************************",  "\n")  
  # dimensions  and missing   
  cat("the dimensions of the data set are:",DIM[1],"by",DIM[2], "\n") 
  Data <- na.omit(data)
  DimOm <- dim(Data)
  dif1 <- DIM[1]-DimOm[1]
  dif2 <- DIM[2]-DimOm[2]
  cat("number of observations with missing values;", dif1, "\n" )   
  cat("% data omited;", (dif1/DIM[1])*100,"%", "\n")
  cat("**************************************************************",  "\n")
  cat("**************************************************************",  "\n") 
  invisible(Data)  
} 
################################################################################
################################################################################
################################################################################
################################################################################
# function 2
data_which_na <- function(data) 
{
  # what is the data
  if (is(data, "list"))  stop("the data is list  the function needs a data.frame") 
  if (is(data, "table")) stop("the data is a table the function needs a data.frame")
  if (is(data, "matrix"))    data <- as.data.frame(data)
  if (is(data[1],"mts"))     data <- as.data.frame(data)
  if (is(data[1],"tible"))  data <- as.data.frame(data)
  if (is(data, "array")) stop("the data is an array the function needs a data.frame")  
  Names <- names(data)
  PP <- list()
  for (i in 1:length(Names))
  {
    PP[[i]] <- sum(is.na(data[,Names[i]]))
  }
  pp=unlist(PP)
  names(pp) <- Names
  print(pp)
  invisible(data) 
}
################################################################################
################################################################################
################################################################################
################################################################################
# function 3
data_names <- function(data)
{
  # what is the data
  if (is(data, "list"))  
    stop("the data is list  the function needs a data.frame") 
  if (is(data, "table")) 
    stop("the data is a table the function needs a data.frame")
  if (is(data, "matrix"))    data <- as.data.frame(data)
  if (is(data[1],"mts"))     data <- as.data.frame(data)
  if (is(data, "array")) 
    stop("the data is an array the function needs a data.frame")  
  # checking data 
  DIM <- dim(data)
  cat("**************************************************************",  "\n")
  cat("**************************************************************",  "\n")
  cat("the names of variables", "\n")
  Names <- names(data)
  print(Names)
  nCh <- nchar(Names)
  whichNames <- nCh>10
  if (any(whichNames))
  {
    cat("*********************************",  "\n")  
    cat("WARNING: the variables", "\n")
    cat(Names[whichNames], "\n")
    cat("have very long names","\n")
    cat("it is advisable to rename them using data_sorter_names()", "\n")  
  }
  cat("**************************************************************",  "\n")
  cat("**************************************************************",  "\n")
  invisible(data) 
}  
################################################################################
################################################################################
################################################################################
################################################################################
# function 4
data_sorter_names <- function(data, max=5, newnames)
{
  # what is the data
  if (is(data, "list"))  
    stop("the data is list  the function needs a data.frame") 
  if (is(data, "table")) 
    stop("the data is a table the function needs a data.frame")
  if (is(data, "matrix"))    data <- as.data.frame(data)
  if (is(data[1],"mts"))     data <- as.data.frame(data)
  if (is(data, "array")) 
    stop("the data is an array the function needs a data.frame")  
  #cat("**************************************************************",  "\n")
  #cat("**************************************************************",  "\n")
  Names <- names(data)
  if (!missing(newnames)) 
  {
    if (length(newnames)!=length(Names)) 
      stop("the newdata should have the same length than the old one")
    names(data) <-  newnames
    
  }
  newnames <- Names
  for (i in 1:length(Names))  
  {
    if (nchar(Names[i])<max)
    {
      newnames[i] <- Names[i]  
    } else
    {
      pp  <- strsplit(Names[i], NULL)[[1]][1:max]
      newnames[i] <- paste0(pp, collapse ="")
    }     
  }
  names(data) <-  newnames
  #  cat("**************************************************************",  "\n")
  data_names(data)
  #  cat("**************************************************************",  "\n")
  invisible(data)
}
################################################################################
################################################################################
################################################################################
################################################################################
# END of DIM, NAMES functions 
################################################################################
################################################################################
################################################################################
################################################################################ 