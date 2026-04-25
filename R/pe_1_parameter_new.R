################################################################################
################################################################################
################################################################################
################################################################################
# there are several functions here 
# the basic functions are 
# pe_1_parameter : for one term partial effects 
#                : with difference between variates and factors 
# pe_2_parameter : for one term partial effects 
#                : with different 
#                : variate <-> variate 
#                : variate <-> factor
#                : factor <-> factor
#the following function call the basic functions
# pe_param       : it calls either   pe_1_parameter or  pe_2_parameter
#                : with option type="parameter"
# pe_eta         : it calls either pe_1_parameter or  pe_2_parameter
#                : with option type="eta"      
# pe_param_grid  :
# pe_eta_grid    :                        
################################################################################
################################################################################
################################################################################
################################################################################
# this function plots the partial effect of one term 
# The baisc structure of the function is
#  i) get the data  
#  ii) get the position of the terms  in the data  
#  iii) calculate the values for the scenarios  
#  iv) predict the secnarios
#  v) create data 
#  vi) if plot=TRUE plot using ggplot2  otherwise save data  
################################################################################
################################################################################
################################################################################
################################################################################   
pe_1_parameter<-  function(obj = NULL, # a gamlss object
         term = NULL, # which term to get the derivative
         data = NULL, # which data is needed here
     n.points = 100,  # number of points needed for evaluating the function
    parameter = c("mu", "sigma", "nu", "tau"), # which parameter
         type = c("parameter", "eta"),
     scenario = list(), # see below (1)
 how.scenario = c("median", "last", "fixed"),
centered.from = c( "mean", "median", "none", "rlevel"),
          col = "darkblue",
    linewidth = 1.3,
     name.obj = NULL,
    data.plot = FALSE,
     data.col = "lightblue",
    data.size = 0.1,
   data.alpha = 0.9,
     rug.plot = TRUE,
      rug.col = "gray",
     rug.size = 0.5,
  factor.size = 15,
         ylim = NULL,
         plot = TRUE,
        title) # whether to plot
{
#  scenario:a named list of the values to use for the other predictor terms. 
#  Variables omitted from this list will have values set to the median 
#  for continuous variables and the most commonly occuring level for factors 
#  or the last observation (I guesss for TS)
# if (is.null(obj)||!class(obj)[1]=="gamlss") stop("Supply a standard GAMLSS model in obj")
################################################################################
# local function 
relevant.factor<- function(fac)
  {
   if (!is.factor(fac)) stop("only factors for the rfactor option") 
      value1stlevel <- fitted.star[1] # need the reference level here 
      value1stlevel 
}
################################################################################  
# function starts here 
if (!missing(obj)&&!(inherits(obj,c("gamlss", "gamlss2")))) 
    stop("the model is not a gamlss model")  
if (is.null(term))  stop("The model term is not set")
            x <-  y <- NULL
 how.scenario <- match.arg(how.scenario)
         type <- match.arg(type)
    parameter <- match.arg(parameter)
centered.from <- match.arg(centered.from)
if (inherits(obj, "gamlss")) # for gamlss() #####################################
  {
    if (any(grepl("data", names(obj$call)))) 
    {
      DaTa <- if (startsWith(as.character(obj$call["data"]), "na.omit"))
        eval(parse(text=as.character(obj$call["data"]))) 
      else get(as.character(obj$call["data"]))	
      v.names <- names(DaTa)
      type <- if (type=="parameter") "response" else "link"
    } else if (missing(data)) stop("The data argument is needed in obj")   
  } else ###################### gamlss2() ######################################  
  { 
       DaTa <-model.frame(obj)
    v.names <-  colnames(DaTa) # names(DaTa)
       type <- if (type=="eta")  "link" else type
  } ########################  end gamlss2() ####################################  
  pos <- which(v.names==term)
if (pos<1) stop("supply a  term")
if (is.factor(DaTa[,pos])||is.character(DaTa[,pos])) # if factor ###############
  {
    if (is.factor(DaTa[,pos]))
    {
      xvar <- levels(DaTa[,pos])
      n.points <- nlevels(DaTa[,pos])
      x.is.factor <- TRUE
    } else 
    {
      xvar <- attr(table(DaTa[,pos]),"names")
      n.points <- length(xvar)
      x.is.factor <- TRUE
    }  
  } else ####################################### not a factor ################## 
  {
    xvar <-  seq(from = min(DaTa[,pos]), to=max(DaTa[,pos]), length.out=n.points)
    x.is.factor <- FALSE
  } ######## finish  ###########################################################
            mat <- matrix(0, nrow = dim(DaTa)[1]+n.points, ncol =dim(DaTa)[2])
       dat.temp <- as.data.frame(mat)
names(dat.temp) <- v.names             
## creating the new data frame          
  for (i in 1:dim(dat.temp)[2])
  {
    if(pos==i)                      # if the variable of interest
    {                               # new data for x is
      dat.temp[,i]  <- if (is.factor(DaTa[,i])) 
        as.factor(c(as.character(DaTa[,i]),as.character(xvar)))
      else c(DaTa[,i],xvar)
    }
    else                            # for all other variables
    {                               # if scenario is set it gets priority
      ma <- scenario[[v.names[i]]]
      if (is.null(ma))                # if scenario in not set
      {
        if (how.scenario=="median")       # get the median for continuous 
          # or the level with high values for factor
        {
          if (is.character(DaTa[,i])) DaTa[,i] <- as.factor(DaTa[,i])
          ma <- if(is.factor(DaTa[,i])) levels(DaTa[,i])[which.max(table(DaTa[,i]))]
          else median(DaTa[,i]) # get the median continuous 
        }
        if (how.scenario=="last")         # otherwise get the last values
        {
          if (is.character(DaTa[,i])) DaTa[,i] <- as.factor(DaTa[,i])
          ma <- if(is.factor(DaTa[,i])) levels(DaTa[,i])[which.max(table(DaTa[,i]))]
          else tail(DaTa[,i],1) # otherwise get the last values
        }
      }
      dat.temp[,i] <- if (is.factor(DaTa[,i])) as.factor(c(as.character(DaTa[,i]),as.character(rep(ma,n.points))))
      else c(DaTa[,i],rep(ma,n.points))
    }
  } # end going thought the variables
  ## Now predict  
  # pp = attr(predict(obj,  type = "term", parameter = parameter),"constant")
  if (inherits(obj, "gamlss")) ## GAMLSS
  {
    fitted.star  <- predict(obj, parameter = parameter, 
                             newdata=tail(dat.temp, n.points), type = type) 
    aver.fitted.star <- switch(centered.from, 
                                "mean" = mean(fitted.star),
                                "median" = median(fitted.star),
                                "none" = 0,
                                "rlevel" = relevant.factor(dat.temp[,pos]))
    fittted.orig <- fitted.star - aver.fitted.star
    
       rfactor <- function(fact)
       {
         
       }
  }  else # if gamlss2
  {
    fitted.star  <- predict(obj, model = parameter, newdata=tail(dat.temp, n.points), type=type)  
    if (x.is.factor) 
    {
      value1stlevel <- fitted.star[1] # need the refencce level here 
      fittted.orig  <- fitted.star-value1stlevel
    } else  # x is not a factor 
    {      
      aver.fitted.star <- switch(centered.from, 
                                  "mean" = mean(fitted.star),
                                  "median" = median(fitted.star),
                                  "none" = 0 )
      fittted.orig <- fitted.star - aver.fitted.star
    }
  }      
  name.obj <-  if (is.null(name.obj))  deparse(substitute(obj)) else name.obj
  txt.title <- if (missing(title))  
    paste("Partial effect of",term, "for", parameter, "for model", name.obj)
  else title
  yaxislabel <- if (type=="response") paste0("PE_param(", term, ")")
  else                  paste0("PE_eta](", term, ")")
  da <- data.frame(y=fittted.orig,x=xvar )
  y_name <- paste(eval(obj$call$formula)[[2]])
if (plot)
{ 
  if (x.is.factor)
  {
    pp <-  ggplot2::ggplot(data=da, ggplot2::aes(x, y))+
           ggplot2::geom_point(color=col, size=factor.size, shape="-")+
           ggplot2::ylab(yaxislabel)+ 
           ggplot2::xlab(term)+ 
          ggplot2::ggtitle(txt.title)
       if ( !is.null(ylim))
     {
      pp <- pp + ggplot2::ylim(ylim)
     }  
  if (data.plot)
    {
    if (parameter!="mu")  stop("data.plot=TRUE can be used only with parameter=\"mu\"") 
    if (type=="link") stop("it is not a good idea to plot the data with type=\"eta\"") 
     pp <- pp +
          ggplot2::geom_jitter(data = DaTa, 
          ggplot2::aes(DaTa[,term], y=DaTa[,y_name]-value1stlevel),
          size = data.size, alpha = data.alpha, colour = data.col)
    }
  } else  # not factor 
  {
    pp <- ggplot2::ggplot(data=da) +
          ggplot2::geom_line( ggplot2::aes(x=x, y=y), color=col, linewidth=linewidth) +
          ggplot2::ylab(yaxislabel)+ 
          ggplot2::xlab(term)+ 
          ggplot2::ggtitle(txt.title)
    if (data.plot)
    {
      if (parameter!="mu")  stop("data.plot=TRUE can be used only with parameter=\"mu\"") 
      if (type=="link") stop("it is not a good idea to plot the data with type=\"eta\"") 
        pp <- pp + ggplot2::geom_point(data=DaTa,ggplot2::aes(y =DaTa[,y_name]- 
                aver.fitted.star,  x = DaTa[,term]), size = data.size, 
                alpha = data.alpha, colour = data.col)#
    }
    if ( rug.plot)
    {
      pp <- pp +
        ggplot2::geom_rug(data=DaTa, ggplot2::aes(x=DaTa[,pos]), col=rug.col, 
                          size=rug.size)
    }
    if ( !is.null(ylim))
    {
      pp <- pp + ggplot2::ylim(ylim)
    }  
  }          
  return(pp)
}
  else
  {  return(da)}  
}
################################################################################
################################################################################
################################################################################
################################################################################
pe_2_parameter <- function(obj = NULL, # the gamlss object
                         terms = NULL, # which terms to get the derivative
                          data = NULL, # which data is needed here
                      n.points = 100,  # number of points needed for evaluating the function
                     parameter = c("mu", "sigma", "nu", "tau"), # which parameter
                          type = c("parameter", "eta"),
                  how.scenario = c("median", "last", "fixed"),
                      scenario = list(), # see below (1)
                 centered.from = c( "mean", "median", "none"),
                           col = "darkblue",
                     linewidth = 1.3,
                     data.plot = FALSE,
                      data.col = "lightblue",
                     data.size = 0.1,
                    data.alpha = 0.9,
                          bins = 30, # for contour plot
                        filled = FALSE, #for contour plot
                      name.obj = NULL,
                          plot = TRUE,
                        title) # whether to plot
{
#  scenario: a named list of the values to use for the other predictor terms. 
#  Variables omitted from this list will have values set to the median 
#  for continuous variables and the most commonly occurring level for factors 
#  or the last observation
if (!missing(obj)&&!(inherits(obj,c("gamlss", "gamlss2")))) 
    stop("the model is not a gamlss model")  
if (is.null(terms))  stop("The model terms are not set")
        x <- y <- NULL
  how.scenario <- match.arg(how.scenario)
          type <- match.arg(type)
          type <- if (type=="parameter") "response" else "link"
     parameter <- match.arg(parameter)
 centered.from <- match.arg(centered.from)
if (inherits(obj, "gamlss"))
  {
    if (any(grepl("data", names(obj$call)))) 
    {
      DaTa <- if (startsWith(as.character(obj$call["data"]), "na.omit"))
                  eval(parse(text=as.character(obj$call["data"]))) 
            else get(as.character(obj$call["data"]))	
      v.names <- names(DaTa)
    }
    else if (missing(data)) stop("The data argument is needed in obj")   
  } else
  {
          DaTa <- model.frame(obj)
       v.names <- colnames(DaTa) # names(DaTa)
  }      
           pos <- match(terms, v.names)
          lpos <- length(pos)                    
if (lpos<=1) stop("supply 2 terms")
if (lpos>3) stop("only up to two terms are allowed")   
  WhichFactor <- sapply(DaTa[,pos], is.factor)
if (any(WhichFactor)) # if any is factor 
  {
        pf <- which(WhichFactor)
        pv <- which(!WhichFactor)
if (length(pf)==2)
    {
      fac1 <- levels(DaTa[,pos[1]])
      #n.points.f1 <- nlevels(DaTa[,pos[1]])
      fac2 <- levels(DaTa[,pos[2]])
      #n.points.f2 <- nlevels(DaTa[,pos[2]])
        d1 <- expand.grid(f1 = fac1, f2 = fac2)
 names(d1) <- terms
       mat <- matrix(0, nrow = dim(DaTa)[1]+dim(d1)[1],ncol =dim(DaTa)[2]) 
      case <- 3 # both factors
    } else
    {
      if (pv==2)   stop("the factor should be set second in order i.e c(\"var\",\"fac\")")         
             fac <- levels(DaTa[,pos[pf]])
      # n.points.f <- nlevels(DaTa[,pos[pf]])
      var <- seq(min(DaTa[,pos[pv]]), max(DaTa[,pos[pv]]), length.out=n.points)
             d1 <- expand.grid(x = var, f = fac)
      names(d1) <- terms
            mat <- matrix(0, nrow = dim(DaTa)[1]+dim(d1)[1], ncol =dim(DaTa)[2])  
           case <- 2 # one factor one continuous
    } 
  } else
  {
              x <- seq(min(DaTa[,pos[1]]), max(DaTa[,pos[1]]), length.out=n.points)
              y <- seq(min(DaTa[,pos[2]]), max(DaTa[,pos[2]]), length.out=n.points)
             d1 <- expand.grid(x = x, y = y)
      names(d1) <- terms
            mat <- matrix(0, nrow = dim(DaTa)[1]+n.points^2, ncol =dim(DaTa)[2])
           case <- 1 # both continuous
  }    
       dat.temp <- as.data.frame(mat)
names(dat.temp) <- v.names             
## creating new data         
  for (i in 1:dim(dat.temp)[2])
  {
    if(pos[1]==i)                      # if the variable of interest
    {                               # new data for x is
      dat.temp[,i] <- c(DaTa[,i],d1[,1]) # min(x) to max(x)
    } else
      if(pos[2]==i)                      # if the variable of interest
      {                               # new data for x is
        dat.temp[,i] <- c(DaTa[,i],d1[,2]) # min(x) to max(x)
      }
    else                            # for all other variables
    {                               # if scenario is set gets priority
      ma <- scenario[[v.names[i]]]
      if (is.null(ma))                  # if scenario in not set
      {
        if (how.scenario=="median")          # get the median for continuous 
          # or the level with high values for factor
        {
          if (is.character(DaTa[,i])) DaTa[,i] <- as.factor(DaTa[,i])
          ma <- if(is.factor(DaTa[,i])) levels(DaTa[,i])[which.max(table(DaTa[,i]))]
          else median(DaTa[,i])
        }
        if (how.scenario=="last")           # otherwise get the last values
        {
          if (is.character(DaTa[,i])) DaTa[,i] <- as.factor(DaTa[,i])
          ma <- if(is.factor(DaTa[,i])) levels(DaTa[,i])[which.max(table(DaTa[,i]))]
          else tail(DaTa[,i],1)
        }
      }
      dat.temp[,i] <-   c(DaTa[,i],rep(ma,dim(d1)[1]))       
    }
  } # end going thought the variables
## now predict   
fittted.orig  <- predict(obj, newdata=tail(dat.temp, dim(d1)[1]), type = type, 
                          parameter = parameter)
  aver.fitted.star <- switch(centered.from, 
                                 "mean" = mean(fittted.orig) ,
                              "median" = median(fittted.orig),
                              "none" = 0 )
  fittted.orig  <- fittted.orig - aver.fitted.star                      
     name.obj  <-  if (is.null(name.obj))  deparse(substitute(obj)) else name.obj
     txt.title <- if (missing(title))  
  paste("Partial effect of", terms[1], "and", terms[2], "for", parameter, "for model", name.obj)
       else title
           da <- data.frame(z=fittted.orig, d1)
       y_name <- paste(eval(obj$call$formula)[[2]])
       
if (plot)
{
 if (case==1)  # both continuous
  {
    pp  <-  ggplot2::ggplot(da, ggplot2::aes(.data[[terms[1]]], .data[[terms[2]]]))
    if (data.plot)  
      pp <- pp + ggplot2::geom_point(data=DaTa, 
                                     ggplot2::aes(x=DaTa[,terms[[1]]], y=DaTa[,terms[[2]]]), 
                                     size = data.size, alpha=data.alpha, colour=data.col)
    pp <-  if (filled)  pp + ggplot2::geom_contour_filled(
      ggplot2::aes(z = da[,1]), bins=bins/3 )
    else        pp + ggplot2::geom_contour(
      ggplot2::aes(z = da[,1]), bins=bins, linewidth=linewidth,  colour=col)
    pp 
    pp <- pp + ggplot2::ggtitle(txt.title)
  } 
 if (case==2) # fist continuous second categorical
  {
    pp <- ggplot2::ggplot(da, ggplot2::aes_string(x=terms[pv], y=da[,1], 
                                                  color=terms[pf]))+
      ggplot2::geom_line(linewidth=linewidth)+
      ggplot2::ggtitle(txt.title)
    if (data.plot)
    {
      if (type=="link") warning("it is not a good idea to plot the data with type=\"eta\"") 
      pp <- pp +   ggplot2::geom_point(data = DaTa,
                                       ggplot2::aes(x=DaTa[,terms[pv]], y=DaTa[,y_name]), 
                                       size = data.size, alpha=data.alpha, colour=data.col)  
    }
  }  
if (case==3) # both factors
  {
    pp <- ggplot2::ggplot(data=da, 
                          ggplot2::aes_string(x=terms[1], y=da[,1], group=terms[2], color=terms[2]))+
      ggplot2::geom_line()+
      ggplot2::geom_point(size=linewidth+2)
    if (data.plot)  
    {
      if (type=="link") warning("it is not a good idea to plot the data with type=\"eta\"") 
      pp <-  pp +ggplot2:: geom_jitter(data = DaTa, 
                                       ggplot2::aes(x=DaTa[,terms[[1]]], y=DaTa[,y_name]),
                                       size=data.size, alpha=data.alpha, colour=data.col)
    }
    pp <- pp +  ggplot2::ggtitle(txt.title)
  }
  return(pp)  
 }  else  return(da)
}       
################################################################################
################################################################################
################################################################################
################################################################################
pe_param <- function(obj = NULL, #  gamlss or gamlss2 object
                    term = NULL, # which terms to get the derivative
                    data = NULL, # which data is needed here
                n.points = 100,  # number of points needed for evaluating the function
               parameter = c("mu", "sigma", "nu", "tau"), # which parameter
                scenario = list(), # see below (1)
            how.scenario = c("median", "last", "fixed"),
                     col = "darkblue",
               linewidth = 1.3,
                name.obj = NULL,
                rug.plot = TRUE,
                 rug.col = "gray",
                rug.size = 0.5,
               data.plot = FALSE,
                data.col = "lightblue",
               data.size = 0.1,
             factor.size = 15,
              data.alpha = 0.9,
                    bins = 30, # for contour plot
                  filled = FALSE, #for contour plot
                    ylim = NULL,
                     title, ...) # whether to plot

{
     lterm <- length(term)
  name.obj <-  if (is.null(name.obj)) deparse(substitute(obj))
if (lterm==1) {
    gg <-  pe_1_parameter(obj = obj,
                         term = term,
                         data = data,
                     n.points = n.points,
                    parameter = parameter,
                         type = "parameter",
                     scenario = scenario,
                 how.scenario = how.scenario,
                          col = col,
                    data.plot = data.plot,
                     data.col = data.col,
                    data.size = data.size,
                   data.alpha = data.alpha,
                     rug.plot = rug.plot,
                      rug.col = rug.col,
                     rug.size = rug.size,
                  factor.size = factor.size,
                    linewidth = linewidth,
                     name.obj = name.obj,
                         ylim = ylim,
                        title = title, ...)}
  else if (lterm==2) {
    gg <-  pe_2_parameter(obj = obj,
                        terms = term,
                         data = data,
                     n.points = n.points,
                    parameter = parameter,
                         type = "parameter",
                 how.scenario = how.scenario,
                    scenario = scenario,
                    linewidth = linewidth,
                         bins = bins,
                       filled = filled,
                     name.obj = name.obj,
                          col = col,
                     data.col = data.col,
                    data.size = data.size,
                   data.alpha = data.alpha,
                    data.plot = data.plot,
                        title = title, ...)}
  else stop("only up to two way interactions can be plotted")
  return(gg)
}
################################################################################
################################################################################
################################################################################
################################################################################
pe_eta <- function(obj = NULL, #  gamlss or gamlss2 object
                  term = NULL, # which terms to get the derivative
                  data = NULL, # which data is needed here
              n.points = 100,  # number of points needed for evaluating the function
             parameter = c("mu", "sigma", "nu", "tau"), # which parameter
              scenario = list(), # see below (1)
          how.scenario = c("median", "last", "fixed"),
                   col = "darkblue",
             linewidth = 1.3,
              name.obj = NULL,
              rug.plot = TRUE,
               rug.col = "gray",
              rug.size = 0.5,
             data.plot = FALSE,
              data.col = "lightblue",
             data.size = 0.1,
           factor.size = 15,
            data.alpha = 0.9,
                  bins = 30, # for contour plot
                filled = FALSE, #for contour plot
                  ylim = NULL,
                 title, ...) # whether to plot

{
     lterm <- length(term)
  name.obj <-  if (is.null(name.obj)) deparse(substitute(obj))
  if (lterm==1) {
    gg <-  pe_1_parameter(obj = obj,
                         term = term,
                         data = data,
                     n.points = n.points,
                    parameter = parameter,
                         type = "eta",
                     scenario = scenario,
                 how.scenario = how.scenario,
                          col = col,
                    data.plot = data.plot,
                     data.col = data.col,
                    data.size = data.size,
                   data.alpha = data.alpha,
                     rug.plot = rug.plot,
                      rug.col = rug.col,
                     rug.size = rug.size,
                  factor.size = factor.size,
                    linewidth = linewidth,
                     name.obj = name.obj,
                         ylim = ylim,
                        title = title, ...)}
  else if (lterm==2) {
    gg <-  pe_2_parameter(obj = obj,
                        terms = term,
                         data = data,
                     n.points = n.points,
                    parameter = parameter,
                         type = "eta",
                 how.scenario = how.scenario,
                     scenario = scenario,
                    linewidth = linewidth,
                         bins = bins,
                       filled = filled,
                    name.obj = name.obj,
                         col = col,
                    data.col = data.col,
                   data.size = data.size,
                  data.alpha = data.alpha,
                   data.plot = data.plot,
                       title = title, ...)}
  else stop("only up to two way interactions can be plotted")
  return(gg)
}
################################################################################
################################################################################
################################################################################
################################################################################
pe_param_grid <- function(model, terms, maxcol=2, maxrow=3, ylim = NULL, ...)
{  
  ################################################################################ 
  define_region <- function(row, col){
    grid::viewport(layout.pos.row=row, layout.pos.col=col) }
  ################################################################################  
  # function starts
  lterms <- length(terms)
  if (lterms  >   maxcol*maxrow) stop("increase the maxcol or maxrow")   
  norow <- ceiling(lterms/maxcol)
  nocol <- if (norow == 1)  lterms  else  maxcol    
  IJ <- expand.grid(j=1:nocol, i=1:norow)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout=grid::grid.layout(nrow=norow,ncol=nocol)))   
  GG <- DA <- list()
  # for (i in xs ) gg[[i]] <-pe_pdf(linear_3, term=i, title=i)
  for (p  in 1:lterms) 
  {
    DA[[p]] <- pe_param(model, term=terms[[p]], 
                        plot=FALSE)
  }
  for (p  in 1:lterms) 
  {
    title.term <- if (length(terms[[p]])==1) terms[[p]]
    else paste(terms[[p]], collapse=":")
    GG[[title.term]] <- pe_param(model, term=terms[[p]], 
                         title= title.term, ylim=ylim,
                          ...)
    print(GG[[title.term]], vp=define_region(IJ$i[p], IJ$j[p]))
  }
}
################################################################################
################################################################################
################################################################################
################################################################################
pe_eta_grid <- function(model, terms, maxcol=2, maxrow=3, ylim = NULL, ...)
{  
################################################################################ 
  define_region <- function(row, col){
    grid::viewport(layout.pos.row=row, layout.pos.col=col) }
################################################################################  
# function starts
     lterms <- length(terms)
if (lterms  >   maxcol*maxrow) stop("increase the maxcol or maxrow")   
     norow <- ceiling(lterms/maxcol)
     nocol <- if (norow == 1)  lterms  else  maxcol    
        IJ <- expand.grid(j=1:nocol, i=1:norow)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout=grid::grid.layout(nrow=norow,ncol=nocol)))   
      GG <- DA <- list()
# for (i in xs ) gg[[i]] <-pe_pdf(linear_3, term=i, title=i)
for (p  in 1:lterms) 
  {
    DA[[p]] <- pe_param(model, term=terms[[p]], 
                                   plot=FALSE)
  }
  for (p  in 1:lterms) 
  {
    title.term <- if (length(terms[[p]])==1) terms[[p]]
                   else paste(terms[[p]], collapse=":")
  GG[[title.term]] <- pe_eta(model, term=terms[[p]], 
                                 title= title.term, ylim=ylim, ...)

    print(GG[[title.term]], vp=define_region(IJ$i[p], IJ$j[p]))
  }
}
################################################################################
################################################################################
################################################################################
################################################################################


