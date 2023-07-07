################################################################################
################################################################################
################################################################################
################################################################################
# create 30-12-2022
# Mikis 
# the binomial case is not working
# check about weights 
################################################################################
################################################################################
################################################################################
################################################################################
# comments
# Any importance measure  needs a criterion to  evaluate how important 
#    a term is. Here we use the -logL of the training data. We could do to the 
#    test data or use a different measure like CRPS   
#  NOTE that there are two option for the option method 
#      c("getTGD", "drop1All")
#   the first is base on differences in the test global deviance [getTGD()] 
#   the second is based on Likelihood Ration test of the function drop1All()
#         
################################################################################
################################################################################
################################################################################
################################################################################
# to DO
# i)   binomial is not working at the moment 
# ii)  ordering of the plot is DONE
# iii) there is a problem if names line "same-more"  in the data they need to
#      be "some_more" why? 
# iv)  can we introduce the option parameter to get only the contribution for 
#      specific parameter?      
################################################################################
################################################################################
################################################################################
################################################################################
# This in combination with  formulae2data()
# takes a gamlss object, extract its formulae and then 
# creates a data frame 
modelformula2data <- function(obj, data)
{
  if (!is(obj,"gamlss")) stop("it needs a gamlss object")
  param <- obj$parameters
  lparam <- length(param)
  formul <- list()
  for (i in 1:lparam) 
  {
    formul[[param[i]]] <- as.formula(obj[[paste0(param[i],".formula")]])
  }
  DaTa <-  formulae2data(formul, data=data) #
  DaTa  <- DaTa[, names(DaTa) %in% names(data)]   
  DaTa
}
################################################################################
################################################################################
################################################################################
formulae2data <- function(formula = list(), data=NULL, weights=NULL, subset=NULL, 
                          na.action, print = TRUE  )
{
if (is(formula,"list"))
  {
    lenList <- length(formula)
    if (lenList==0) stop("no formula detected")
    if (lenList==1) 
    {
      ff <- deparse(formula[[1]])
    } else
    {
      # the first formula  
      form <- formula(formula[[1]])
      # create y~x+   
      f1 <- ff <- paste(paste(form[[2]],form[[1]]), 
                        deparse(form[[3]], width.cutoff = 500L), "+")
      # now add the of he formulae    
      for (i in 2:lenList)
      {
        ff <- if (i==lenList) paste(ff, deparse(formula[[i]][[2]], width.cutoff = 500L))
        else paste(ff, deparse(formula[[i]][[2]], width.cutoff = 500L),"+")
      } 
    }
  } else if (is(formula,"formula")) {ff  <- deparse(substitute(formula))}
  else stop("The formula argument should be a formula or a list") 
  if (!is.null(weights)) 
  {
    # formula(paste(ff[[3]], collapse = " "))
    ff <- paste(ff, deparse(substitute(weights)), sep="+")
    # ff[[3]] <- paste(ff[[3]],deparse(substitute(weights)), sep="+")
  }
  environment(ff) <- globalenv()    # do I need this
  all.vars <- get_all_vars(ff, data=data)
  if (!is.null(data)&&!inherits(data,"data.frame")) warning("data is not a data frame class attributes will be lost")
  M <- dim(all.vars)[1]
  ## subsetting             
  if (!is.null(subset)) {
    r <- if (!is.null(data))  eval(substitute(subset), data,  parent.frame())
    else eval(substitute(subset),  parent.frame())
    if (!is.logical(r)) stop("'subset' must be logical")
    all.vars <- all.vars[r,]
    M <- dim(all.vars)[1]
    if (print) cat( M, "observations left after subsetting \n" )           
  }
  # it need a futher warning here      N <- dim(all.vars)[1]  
  # na.omit   
  all.vars <- na.omit(all.vars)                             # clear NA's
  N <- dim(all.vars)[1]     
  if (print) {if (M-N > 0) cat(M-N, "rows with NAs are deleted", "\n" )}
  if (print) cat( N, "observations with", dim(all.vars)[2], "variables \n")    
  attr(all.vars, "formula") <- ff
  return(all.vars)
}
################################################################################
################################################################################
################################################################################
################################################################################
# this should work with most GAMLSS models but not with binomial
term_importance <- function (   
                         obj = NULL,
                        data = NULL,
                      method = c("getTGD", "drop1All"),  
                  point_size = 4, # the size of the line
                 point_alpha = 0.6, 
                 colour.segm = "darkblue",
                colour.point = "darkblue",
                      title, ...) 
{
gamlss.bi.list <- .binom    
# do we need this?
if (is.null(obj) || !class(obj)[1] == "gamlss") 
    stop("Supply a standard GAMLSS model in obj")
 method <-  match.arg(method)
      x <-  y <- width <- NULL
if (method=="getTGD")
{
# get the response
  resp <- paste(obj$mu.formula[[2]])
  if (!is.null(obj$call[["weights"]])) weightCha = paste(obj$call[["weights"]])
# get the data  
if (any(grepl("data", names(obj$call)))) 
  {
      DaTa <- if (startsWith(as.character(obj$call["data"]), "na.omit")) 
                   eval(parse(text = as.character(obj$call["data"])))
              else get(as.character(obj$call["data"]))
  }
else if (is.null(data)) 
  stop("The data argument is needed in obj")
# what about prior weights   
# from a gamlss model get the formulae and create a data frame
      DaTa <- modelformula2data(obj, DaTa) 
   v.names <- names(DaTa)
  pos.resp <- match(resp, names(DaTa))
   v.names <- v.names[-pos.resp]
   #match(weightCha, names(DaTa))
      LDev <- list()
for (i in v.names)
 {
     NData <- DaTa
       IND <- sample.int(length(DaTa[[i]]))
 NData[,i] <- DaTa[,i][IND] 
 LDev[[i]] <- getTGD(obj, newdata=NData)$TGD-deviance(obj)
 }
     ldev <- unlist(LDev)
   ldname <-  names(ldev)
    sldev <- sum( ldev)
txt.title <- if (missing(title))
    {
      paste("Relative importance of terms from model", deparse(substitute(obj)),"using getTGD")
    } else title    
      DAT <- data.frame(ri = (ldev/sldev)*100, names=ldname)
DAT$names <- with(DAT, reorder(names, ri, median))
       gg <- ggplot2::ggplot(DAT, ggplot2::aes(names, ri))+
         ggplot2::geom_segment( aes(x=names, xend=names, y=0, yend=ri), 
                                color=colour.segm)+
         ggplot2::geom_point( color=colour.point, size=point_size, 
                              alpha=point_size) + 
         ggplot2::coord_flip()+  ggtitle(txt.title) 
  return(gg)
} else 
        TT <- drop1All(obj,...)
        RI <-  TT[-1,"LRT"]/sum(TT[-1,"LRT"])*100
       DAT <- data.frame(ri = RI, names=rownames(TT)[-1])
 DAT$names <- with(DAT, reorder(names, RI, median)) 
 txt.title <- if (missing(title))
   {
   paste("Relative importance of terms from model", deparse(substitute(obj)), 
         "using drop1All()")
   } else title  
        gg <- ggplot2::ggplot(DAT, ggplot2::aes(names, ri))+
          ggplot2::geom_segment( aes(x=names, xend=names, y=0, yend=ri), 
                                 color=colour.segm)+
          ggplot2::geom_point(color=colour.point, size=point_size, alpha=point_size) +
          ggplot2::coord_flip() +  
          ggplot2::ggtitle(txt.title) 
  print(gg)
}
################################################################################
################################################################################
################################################################################
################################################################################
