# MIKIS STASINOPOULOS
# 20-10-2022
################################################################################
################################################################################
################################################################################
################################################################################
# to do
# i) IT NEEDS CLEARING UP done
# ii) also to exlcude variables from the data DONE
# iii) how to scale is important we should have options
#       scale = c(z-scores", "0to1") DONE
# iv)    type = c("main.effect", "first.order"), I need also 
#                "none"      
################################################################################
################################################################################
################################################################################
################################################################################
# this function creates a data frame with main or first order effects
# so the continuous x's will be scale and the interaction will be on 
# the scaled variables
# the factors are left as dummies
# 
################################################################################
################################################################################
################################################################################
################################################################################
data_stand<- function(data, response, 
                 exclude = NULL,
                   scale = c("z-scores", "0to1"),  
                  family = SHASHo,
                    type = c("main.effect", "first.order", "none"),
                 weights = NULL)
{
# all the variable in data are used to create the data.frame
# unless the 'exclude' is used
# the argument `data' is compulsory 
# the argument `response' is compulsory  
# scale:  the type of scalling 
#  i) "z-scores" using SHASH
#  ii) 0 to 1 
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
################################################################################
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
################################################################################
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
################################################################################
## get the formula according to type 
if (type=="none")
  {
  return(data)
  }
if (type=="main.effect")
  {
  formula <- as.formula(paste(paste0(response_t,"~"), paste(x_Names, 
                                  collapse='+'))) 
       XX <- model.matrix(formula, weights=weights, data=data)[,-1]
      dXX <-  as.data.frame(XX)
       ## in order to have a complete data frame we need also the response 
dXX[, response_t] <- data[, response_t]
       return(dXX)
   } 
if (type=="first.order")
  {
    formula <- as.formula(paste(paste0(response_t,"~"), 
                    paste0(paste0("(",paste(x_Names, collapse='+')), ")^2"))) 
# take the response out so you can scale the rest     
     XX <- model.matrix(formula, weights=weights, data=data)[,-1]
     d2 <- dim(XX)[2]
  Names <- character(d2)
for (i in 1:d2) 
   {
    Names[i] <-  gsub(":", ".", colnames(XX)[i])
   }
 colnames(XX) <- Names
## create a data frame     
         dXX <-  as.data.frame(XX)
## in order to have a complete data frame we need also the response 
dXX[, response_t] <- data[, response_t]
   return(dXX)
  }
}
################################################################################
################################################################################
################################################################################
################################################################################
