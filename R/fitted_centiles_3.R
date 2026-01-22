# functions for centile plots in ggplots 
# last change on may 2021
# there are two functions 
# fitted_centile_1()
# which use sort data 
# centiles x y 
################################################################################
################################################################################
################################################################################
################################################################################
fitted_centiles <-function (obj, 
                     xvar, 
                     cent = c(99.4, 98, 90, 75, 50, 25, 10, 2, 0.4),
                   points = TRUE,
                point.col = "gray",
               point.size = 1,
                line.size = .8, 
                 line.col = hcl.colors(lc, palette="Dark 2"),
                line.type = rep(1, length(cent)),
                     xlab = NULL,
                     ylab = NULL,
                     ylim,
                    title, ...)        
{
################################################################################
if (missing(obj)) stop("the model is  missing")
if (!inherits(obj, c("gamlss", "gamlss2"))) stop("the model is not a gamlss model")  
if (inherits(obj, "gamlss"))
{
  yvarCh <- all.vars(obj$call$formula)[[1]]
  if (missing(xvar))
  {
    xvarCh <-  all.vars(obj$call$formula)[[2]]   # get it from fotmula
    if (length(xvarCh)>1) stop("fitted_centiles is design for one x only")  
    if (any(grepl("data", names(obj$call))))# if data exist 
    {
      DaTa <- eval(obj$call[["data"]]) 
      xvar <- get(xvarCh, envir=as.environment(DaTa))
    } else 
    {
      stop("data are required in the original fit or the xvar")
    }
  } else # xvar is not missing 
  { 
    xvarCh <-  deparse(substitute(xvar))
    if (any(grepl("data", names(obj$call))))# if data exist 
    {
      DaTa <- eval(obj$call[["data"]]) 
      xvar <- get(xvarCh, envir=as.environment(DaTa))
    } else { xvar <- get(xvarCh, envir=globalenv())}
  }
     fam <-  get_family(obj)
   fname <- fam$fname
    lpar <- fam$nopar
    qfun <- paste0("q", fname)
   oxvar <- xvar[order(xvar)]
   oyvar <- obj$y[order(xvar)]  
if (is.matrix(obj$y)) # Monday, March 26, 2007 at 14:12
   {
     oyvar <-  obj$y[,1][order(xvar)] 
     ylim  <-  range(obj$y[,1])
     yleg <- max(obj$y[,1])
     lpar <- fam$nopar
   yvarCh <- paste(obj$call$formula[[2]])
   }
} # end of gamlss
################################################################################
if (inherits(obj, "gamlss2"))
{
   xvarCh <- all.vars(obj$call$formula)[[2]] 
   yvarCh <- response_name(obj)
if (length(xvarCh)>1) stop("fitted_centiles is design for one x only")
   if (missing(xvar))
   {
     xvarCh <-  all.vars(obj$call$formula)[[2]]   # get it from fotmula
     if (length(xvarCh)>1) stop("fitted_centiles is design for one x only")  
     if (any(grepl("data", names(obj$call))))# if data exist 
     {
       DaTa <- eval(obj$call[["data"]]) 
       xvar <- get(xvarCh, envir=as.environment(DaTa))
     } else 
     {
       stop("data are required in the original fit or the xvar")
     }
   } else # xvar is not missing 
   { 
     xvarCh <-  deparse(substitute(xvar))
     if (any(grepl("data", names(obj$call))))# if data exist 
     {
       DaTa <- eval(obj$call[["data"]]) 
       xvar <- get(xvarCh, envir=as.environment(DaTa))
     } else { xvar <- get(xvarCh, envir=globalenv())}
   }
     DaTa <- DaTa[order(xvar),]
      par <- predict(obj, newdata=DaTa, type="parameter")
     lpar <- length(obj$family$names)
      fam <- get_family(obj)
    fname <- fam$fname
    oxvar <- xvar[order(xvar)]
    oyvar <- obj$y[order(xvar)]  
if (is.matrix(obj$y)) # Monday, March 26, 2007 at 14:12
   {
   oyvar <-  obj$y[,1][order(xvar)] 
   ylim  <-  range(obj$y[,1])
    yleg <- max(obj$y[,1])
   }
}
       x <- y <-   NULL
   txt.title <- if (missing(title)) 
                paste("Centile curves using", fname, sep = " ")
                else title
   #  col <- 3 # set this to 1 if you do not want colour 
    #length(obj$parameters)
      ii <- 0
     per <- rep(0,length(cent))
   centM <- matrix(0, ncol=length(cent), nrow= dim(DaTa)[1])
colnames(centM) <- cent 
      lc <- length(cent)
############## get the centiles ################################################ 
for(var in cent) 
{ # centile loop 
  
if (inherits(obj, "gamlss"))
{  
    if(lpar==1) 
    {
      newcall <-call(qfun,var/100, mu=fitted(obj,"mu")[order(xvar)]) 
    }
    else if(lpar==2)
    {
      newcall <-call(qfun,var/100, mu=fitted(obj,"mu")[order(xvar)],
                     sigma=fitted(obj,"sigma")[order(xvar)]) 
    }
    else if(lpar==3)
    {
      newcall <-call(qfun,var/100, mu=fitted(obj,"mu")[order(xvar)],
                     sigma=fitted(obj,"sigma")[order(xvar)],
                     nu=fitted(obj,"nu")[order(xvar)])
    }
    else 
    {
      newcall <-call(qfun,var/100, mu=fitted(obj,"mu")[order(xvar)],
                     sigma=fitted(obj,"sigma")[order(xvar)],
                     nu=fitted(obj,"nu")[order(xvar)],
                     tau=fitted(obj,"tau")[order(xvar)]) 
    } 
  ii <- ii+1
    centM[,ii] <- eval(newcall)
} else # gamlss2
{
  ii <- ii+1
   centM[,ii] <- fam$q_fun(p=var/100, par=par)
}  
}###############################################################################
   DataC <-  data.frame(c = centM, 
                        x = oxvar, 
                        y = oyvar)
  Cnames <- colnames(DataC)
     ggc <- ggplot(DataC)
if (points) 
    {
    ggc <- ggc + geom_point(aes(x=x, y=y), colour=point.col, size=point.size)
     }
for (i in 1:lc)
    {
  #fcol <-rep(line.col[i], N)
      ggc <- ggc + geom_line(aes(x=.data[["x"]], y=.data[[Cnames[i]]]), linetype=line.type[i],
                             color=line.col[i], linewidth=line.size)
} 
  xvarCh <-   if (is.null(xlab)) xvarCh else xlab  
  yvarCh <-   if (is.null(ylab)) yvarCh else ylab  
     ggc <- ggc+ ggtitle( txt.title)+ylab(yvarCh)+xlab(xvarCh)
  if (!missing(ylim)) ggc <-  ggc+ggplot2::ylim(ylim)
  ggc
}
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
fitted_centiles_legend <-function (obj, 
                              xvar, 
                              cent = c(99.4, 98, 90, 75, 50, 25, 10, 2, 0.4),
                            points = TRUE,
                         point.col = "gray",
                        point.size = 1,
                         line.size = .8, 
                          line.col = hcl.colors(ncent, palette="Dark 2"),
                         line.type = rep(1, length(cent)),
                       show.legend = TRUE,
                         save.data = FALSE,
                             title,
                              xlab = NULL,
                              ylab = NULL,
                              ylim,
                       ...)        
{
      x <- y <-   NULL
  ncent <- length(cent)
if (missing(obj)) stop("the model is  missing")
if (!inherits(obj, c("gamlss", "gamlss2"))) stop("the model is not a gamlss model")  
if (inherits(obj, "gamlss"))
{
  if (missing(xvar))
  {
    xvarCh <-  all.vars(obj$call$formula)[[2]]   # get it from fotmula
    if (length(xvarCh)>1) stop("fitted_centiles is design for one x only")  
    if (any(grepl("data", names(obj$call))))# if data exist 
    {
      DaTa <- eval(obj$call[["data"]]) 
      xvar <- get(xvarCh, envir=as.environment(DaTa))
    } else 
    {
      stop("data are required in the original fit or the xvar")
    }
  } else # xvar is not missing 
  { 
    xvarCh <-  deparse(substitute(xvar))
    if (any(grepl("data", names(obj$call))))# if data exist 
    {
      DaTa <- eval(obj$call[["data"]]) 
      xvar <- get(xvarCh, envir=as.environment(DaTa))
    } else { xvar <- get(xvarCh, envir=globalenv())}
  }
    fam <-  get_family(obj)
  fname <- fam$fname
   qfun <- paste0("q", fname)
   lpar <- length(obj$parameters)
  oxvar <- xvar[order(xvar)]
  oyvar <- obj$y[order(xvar)]  
 yvarCh <- paste(obj$call$formula[[2]])
     lc <- length(cent)  
if (is.matrix(obj$y)) # Monday, March 26, 2007 at 14:12
  {
    oyvar <-  obj$y[,1][order(xvar)] 
    ylim  <-  range(obj$y[,1])
    yleg <- max(obj$y[,1])
    lpar <- fam$nopar
  yvarCh <- paste(obj$call$formula[[2]])
  }
} # end of gamlss
################################################################################
if (inherits(obj, "gamlss2"))
{
  xvarCh <- all.vars(obj$call$formula)[[2]] 
  yvarCh <- response_name(obj)
if (length(xvarCh)>1) stop("fitted_centiles is design for one x only")
if (any(grepl("data", names(obj$call))))# if data exist 
  {
    DaTa <- eval(obj$call[["data"]]) 
    xvar <- get(xvarCh, envir=as.environment(DaTa))
  }   
# oxvar <- xvar[order(xvar)]
    DaTa <- DaTa[order(xvar),]
     par <- predict(obj, newdata=DaTa, type="parameter")
    lpar <- length(obj$family$names)
     fam <- get_family(obj)
   fname <- fam$fname
   oxvar <- xvar[order(xvar)]
   oyvar <- obj$y[order(xvar)]  
if (is.matrix(obj$y)) # Monday, March 26, 2007 at 14:12
  {
    oyvar <-  obj$y[,1][order(xvar)] 
    ylim  <-  range(obj$y[,1])
    yleg <- max(obj$y[,1])
  }
}
txt.title <- if (missing(title)) 
  paste("Centile curves using", fname, sep = " ")
else title
col <- 3 # set this to 1 if you do not want colour 
#length(obj$parameters)
ii <- 0
per <- rep(0,length(cent))
centM <- matrix(0, ncol=length(cent), nrow= dim(DaTa)[1])
colnames(centM) <- cent 
lc <- length(cent)
############## get the centiles ################################################ 
for(var in cent) 
{ # centile loop 
   if (inherits(obj, "gamlss"))
    {  
      if(lpar==1) 
      {
        newcall <-call(qfun,var/100, mu=fitted(obj,"mu")[order(xvar)]) 
      }
      else if(lpar==2)
      {
        newcall <-call(qfun,var/100, mu=fitted(obj,"mu")[order(xvar)],
                       sigma=fitted(obj,"sigma")[order(xvar)]) 
      }
      else if(lpar==3)
      {
        newcall <-call(qfun,var/100, mu=fitted(obj,"mu")[order(xvar)],
                       sigma=fitted(obj,"sigma")[order(xvar)],
                       nu=fitted(obj,"nu")[order(xvar)])
      }
      else 
      {
        newcall <-call(qfun,var/100, mu=fitted(obj,"mu")[order(xvar)],
                       sigma=fitted(obj,"sigma")[order(xvar)],
                       nu=fitted(obj,"nu")[order(xvar)],
                       tau=fitted(obj,"tau")[order(xvar)]) 
      } 
      ii <- ii+1
      centM[,ii] <- eval(newcall)
    } else # gamlss2
    {
      ii <- ii+1
      centM[,ii] <- fam$q_fun(p=var/100, par=par)
    }  
}###############################################################################
#yvarCh <- paste(obj$call$formula[[2]])
     N <- length(xvar) 
    lc <- length(cent)
 DataM <- data.frame(c = as.vector(centM),
                      x = rep(oxvar, lc),
                      y = rep(oyvar, lc),
               centiles = gl(length(cent), N, labels = as.character(cent)),
                  color = gl(length(cent), N, labels = line.col)
              # typeline = factor(rep(line.type, each = length(oxvar))) 
                        )
  if (save.data) return(DataM)  
  gg <- ggplot2::ggplot(DataM, ggplot2::aes(x=x, y=c, col=centiles, group=centiles)) 
  #linetype = typeline
  if (points) 
  {
    gg <-  gg +
      ggplot2::geom_point(ggplot2::aes(x=x, y=y), colour=point.col, size=point.size)+
      ggplot2::geom_line(linewidth=line.size,  
                    show.legend = show.legend)+#, color=DataM$color
          ggtitle( txt.title)
   } else
  {
    gg <- gg +
      ggplot2::geom_line(linewidth=line.size, linetype=line.type,
                    show.legend = show.legend)+
      ggplot2::ggtitle( txt.title)
  }
  xvarCh <-   if (is.null(xlab)) xvarCh else xlab  
  yvarCh <-   if (is.null(ylab)) yvarCh else ylab  
  gg <- gg+ ggplot2::ylab(yvarCh)+ggplot2::xlab(xvarCh)
  if (!missing(ylim)) gg <-  gg+ggplot2::ylim(ylim)
  gg
}
################################################################################
################################################################################
################################################################################
################################################################################
fitted_centiles_gap <-function (obj, 
                                   xvar, 
                                   cent = c(98, 90, 75, 50, 25, 10, 2),
                                 points = TRUE,
                              point.col = "gray",
                             point.size = 1,
                              line.size = .8, 
                               line.col = hcl.colors(ncent, palette="Dark 2"),
                              line.type = rep(1, length(cent)),
                            show.legend = TRUE,
                              save.data = FALSE,
                                  title,
                                   xlab = NULL,
                                   ylab = NULL,
                                   ylim,
                                   ...)        
{
      x <- y <- xx<-  NULL
  ncent <- length(cent)
if (missing(obj)) stop("the model is  missing")
if (!inherits(obj, c("gamlss", "gamlss2"))) stop("the model is not a gamlss model")  
if (inherits(obj, "gamlss"))
  {
  if (missing(xvar))
    {
      xvarCh <-  all.vars(obj$call$formula)[[2]]   # get it from fotmula
      if (length(xvarCh)>1) stop("fitted_centiles is design for one x only")  
      if (any(grepl("data", names(obj$call))))# if data exist 
      {
        DaTa <- eval(obj$call[["data"]]) 
        xvar <- get(xvarCh, envir=as.environment(DaTa))
      } else 
      {
        stop("data are required in the original fit or the xvar")
      }
    } else # xvar is not missing 
    { 
      xvarCh <-  deparse(substitute(xvar))
      if (any(grepl("data", names(obj$call))))# if data exist 
      {
        DaTa <- eval(obj$call[["data"]]) 
        xvar <- get(xvarCh, envir=as.environment(DaTa))
      } else { xvar <- get(xvarCh, envir=globalenv())}
    }
      fam <-  get_family(obj)
    fname <- fam$fname
     qfun <- paste0("q", fname)
     lpar <- length(obj$parameters)
    oxvar <- xvar[order(xvar)]
    oyvar <- obj$y[order(xvar)]  
    ooxvar <-seq(from=oxvar[1], to=oxvar[length(oxvar)], length.out=length(oxvar))  
   # ooyvar <-seq(from=oyvar[1], to=oyvar[length(oyvar)], length.out=length(oyvar))  
   yvarCh <- paste(obj$call$formula[[2]])
   
    lc <- length(cent)  
    if (is.matrix(obj$y)) # Monday, March 26, 2007 at 14:12
    {
      oyvar <- obj$y[,1][order(xvar)] 
       ylim <- range(obj$y[,1])
       yleg <- max(obj$y[,1])
       lpar <- fam$nopar
     yvarCh <- paste(obj$call$formula[[2]])
    }
  } # end of gamlss
################################################################################
if (inherits(obj, "gamlss2"))
  {
    xvarCh <- all.vars(obj$call$formula)[[2]] 
    yvarCh <- response_name(obj)
    if (length(xvarCh)>1) stop("fitted_centiles is design for one x only")
    if (any(grepl("data", names(obj$call))))# if data exist 
    {
      DaTa <- eval(obj$call[["data"]]) 
      xvar <- get(xvarCh, envir=as.environment(DaTa))
    }   
    # oxvar <- xvar[order(xvar)]
    DaTa <- DaTa[order(xvar),]
     par <- predict(obj, newdata=DaTa, type="parameter")
    lpar <- length(obj$family$names)
     fam <- get_family(obj)
   fname <- fam$fname
   oxvar <- xvar[order(xvar)]
   oyvar <- obj$y[order(xvar)]  
  ooxvar <-seq(from=oxvar[1], to=oxvar[length(oxvar)], length.out=length(oxvar))  
  if (is.matrix(obj$y)) # Monday, March 26, 2007 at 14:12
    {
      oyvar <-  obj$y[,1][order(xvar)] 
      ylim  <-  range(obj$y[,1])
       yleg <- max(obj$y[,1])
    }
  } # end of gamlss2
################################################################################  
txt.title <- if (missing(title)) 
    paste("Centile curves using", fname, sep = " ")
  else title
################################################################################
  col <- 3 # set this to 1 if you do not want colour 
  #length(obj$parameters)
     ii <- 0
            per <- rep(0,length(cent))
          centM <- matrix(0, ncol=length(cent), nrow= dim(DaTa)[1])
colnames(centM) <- cent 
             lc <- length(cent)
############## get the centiles ################################################ 
for(var in cent) 
  { # centile loop 
    if (inherits(obj, "gamlss"))
    {  
      if(lpar==1) 
      {
        newpred <-  predictAll(obj, newdata=data.frame(ga=ooxvar))
        newcall <-call(qfun,var/100, mu = newpred$mu) 
      }
      else if(lpar==2)
      {
        newpred <-  predictAll(obj, newdata=data.frame(ga=ooxvar))
        newcall <-call(qfun,var/100, mu = newpred$mu,
                       sigma = newpred$sigma) 
      }
      else if(lpar==3)
      {
        newpred <-  predictAll(obj, newdata=data.frame(ga=ooxvar))
        newcall <-call(qfun,var/100, mu = newpred$mu,
                       sigma = newpred$sigma,
                       nu = newpred$nu) 
      }
      else 
      {
        newpred <-predictAll(obj, newdata=data.frame(ga=ooxvar))
        newcall <-call(qfun,var/100, mu = newpred$mu,
                                  sigma = newpred$sigma,
                                     nu = newpred$nu,
                                    tau = newpred$tau) 
      } 
      ii <- ii+1
      centM[,ii] <- eval(newcall)
    } else # gamlss2
    {
      ii <- ii+1
      centM[,ii] <- fam$q_fun(p=var/100, par=par)
    }  
}###############################################################################
  #yvarCh <- paste(obj$call$formula[[2]])
         N <- length(xvar) 
        lc <- length(cent)
     DataM <- data.frame(c = as.vector(centM),
                         x = rep(oxvar, lc),
                        xx = rep(ooxvar, lc),
                         y = rep(oyvar, lc),
                  centiles = gl(length(cent), N, labels = as.character(cent)),
                     color = gl(length(cent), N, labels = line.col)
                        ) 
  if (save.data) return(DataM)  
  gg <- ggplot2::ggplot(DataM, ggplot2::aes(x=xx, y=c, col=centiles, group=centiles)) 
  #linetype = typeline
  if (points) 
  {
    gg <-  gg +
      ggplot2::geom_point(ggplot2::aes(x=x, y=y), colour=point.col, size=point.size)+
      ggplot2::geom_line(linewidth=line.size,  
                         show.legend = show.legend)+#, color=DataM$color
      ggtitle( txt.title)
  } else
  {
    gg <- gg +
      ggplot2::geom_line(linewidth=line.size, linetype=line.type,
                         show.legend = show.legend)+
      ggplot2::ggtitle( txt.title)
  }
  xvarCh <-   if (is.null(xlab)) xvarCh else xlab  
  yvarCh <-   if (is.null(ylab)) yvarCh else ylab  
  gg <- gg+ ggplot2::ylab(yvarCh)
if (!missing(ylim)) gg <-  gg+ggplot2::ylim(ylim)
  gg
}
################################################################################
################################################################################
################################################################################
################################################################################