################################################################################
################################################################################
# functions to check the data 
################################################################################
################################################################################ 
# rm(list=ls())
# # checking for missing values
# function available 
################################################################################ 
#  NOT THIS ONE 1) data_check       : will be fixed in the end 
################################################################################
#  3) data_response    :  
#  5) xy_power_trans         : 
# 14) y_power_trans     : takes x and y and find power transformation for x
# 16) datetime2datehour: from datetime to date and hour
# 18) y_zscores        : transform one variable to SHASH or other dist. z-scores 
# 19) y_outliers       : identify outliers of one x by taking z-scores
# 20) data_outliers    : identify outliers using z-scores SHASH
# 20) data_zscores_plot: for continuous variables plots the z scores SHASH
# 21) y_trans 
# 
################################################################################
################################################################################
################################################################################
################################################################################ 

################################################################################
################################################################################
################################################################################
################################################################################
# STRUCTRURE functions
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################


################################################################################
################################################################################
################################################################################
################################################################################
# function 3
#require(gridExtra)
data_response <- function(data, response, plot=TRUE)
{
#  what is the data
if (is(data, "list"))  stop("the data is list  the function needs a data.frame")
if (is(data, "table")) stop("the data is a table the function needs a data.frame")
if (is(data, "matrix"))    data <- as.data.frame(data)
if (is(data[1],"mts"))     data <- as.data.frame(data)
if (is(data, "array")) stop("the data is an array the function needs a data.frame")
      Y <-  deparse(substitute(response))
if (any(!(Y %in%names(data)))) stop("the response should be in data") 
   actY <- data[,Y]
     cY <- class(actY)
cat("the class of the response is", cY, "is this correct?", "\n")   
if (cY=="character")
{
  actY <- factor(actY)
if (nlevels(actY)>20)  
  stop("something is wrong here the number of levels are too big")
if (nlevels==2) cat("binary response", "/n") else
               cat("multinonial response", "/n")
}
if (cY=="integer")
   {
cat("If the response is a true integer a count data distribution could be used", "\n")
cat("otherwhise make it numeric", "\n") 
   }
if (cY=="numeric")
   {
   if (any(actY < 0)) 
     cat("a continuous distribution in the realline could be used", "\n")
   else if (all(actY > 0 & actY< 1))  
stop("a continuous distribution on (0,1) should be used")
  else cat("a continuous distribution on (0,inf) could be used", "\n")
}
if (plot)
{ 
  #layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
  GG1 <- y_hist(actY, title="(a)")+xlab(Y)
  GG2 <- y_dots(actY, title="(b)")+xlab(Y)
    z <- y_zscores(actY, plot=FALSE)
  GG3 <- y_hist(z, title="(c)")+xlab(paste(Y, "z-scores"))
  GG4 <- y_dots(z, title="(d)")+xlab(paste(Y, "z-scores"))
  gridExtra::grid.arrange(GG1, GG2, GG3, GG4 )
}  
invisible(data)   
}
################################################################################
################################################################################
################################################################################
################################################################################
# TRANSFORMATION functions
################################################################################
################################################################################ 
# function 5
xy_power_trans <- function(x,y, data = NULL,  lim.trans = c(0, 1.5), prof=FALSE, 
                     k=2,  c.crit = 0.01, step=0.1)  
{
  #  cat("*** Checking for transformation for x ***", "\n") 
  cY <- class(y)
  cX <- class(x)
  if (any(c(cY, cX)!="numeric")) return(NA)
  ptrans<- function(x, p) if (abs(p)<=0.0001) log(x) else I(x^p)
  fn <- function(p) AIC(lm(y~ptrans(x,p)), k=k)
  if (prof) # profile dev
  {
    pp <- seq(lim.trans[1],lim.trans[2], step) 
    pdev <- rep(0, length(pp)) 
    for (i in 1:length(pp)) 
    {
      pdev[i] <- fn(pp[i])  
      #   cat(pp[i], pdev[i], "\n")
    }
    plot(pdev~pp, type="l")
    points(pdev~pp,col="blue")
    par <- pp[which.min(pdev)]
    cat('*** power parameters ', par,"***"," \n") 
  } else
  {
    fn <- function(p) AIC(lm(y~ptrans(x,p)), k=k)
    par <- optimise(fn, lower=lim.trans[1], upper=lim.trans[2])$minimum
    # cat('*** power parameters ', par,"***"," \n") 
  }  
  par
}
################################################################################
################################################################################
################################################################################
################################################################################
# functions 5
# it takes on x and find the power witch minimised its 
y_power_trans <- function(x, lim.trans = c(0, 1.5), prof=FALSE, 
                          step=0.01,    bucket=FALSE)
{
if (length(lim.trans)!=2) stop(" the limits of  p are not set properly")
  #  cat("*** Checking for transformation for x ***", "\n") 
  cX <- class(x)
  if ((cX!="numeric")&(cX!="integer")) return(NA)
  ptrans <- function(x, p) if (abs(p)<=0.001) log(x) else I(x^p)
  fn <- function(p) momentSK(ptrans(x,p))$jarque.bera.test
  if (prof) # profile dev
  {
    pp <- seq(lim.trans[1],lim.trans[2], step) 
    pdev <- rep(0, length(pp)) 
    for (i in 1:length(pp)) 
    {
      pdev[i] <- fn(pp[i])  
      #   cat(pp[i], pdev[i], "\n")
    }
    GG <- ggplot(data.frame(pdev,pp), aes(x=pp, y=pdev))+
      geom_point()+
      geom_line()
    #  points(pdev~pp,col="blue")
    par <- pp[which.min(pdev)]
    print(GG)
    cat('*** power parameters ', par,"***"," \n") 
  } else
  {
    par <- optimise(fn, lower=lim.trans[1], upper=lim.trans[2])$minimum
    # cat('*** power parameters ', par,"***"," \n") 
  }  
  PP <-  moment_bucket(ptrans(x,par),x, text_to_show=c("+","*"))
  if (bucket) print(PP)
  # 1-pchisq(momentSK(ptrans(x,par))$jarque.bera.test, 2)
  # 1-pchisq(momentSK(ptrans(x,1))$jarque.bera.test, 2)
  # hist(ptrans(x,par))
  #  hist(ptrans(x,1))
  #  momentSK(ptrans(x,1))$jarque.bera.test
  #cat('*** power parameters ', par,"***"," \n")    
  par
}
################################################################################
################################################################################
################################################################################
# function 3
data_trans_plot <- function(data, response,
                             hist.col = "black", 
                            hist.fill = "white", 
                            dens.fill = "#FF6666", 
                                alpha = 0.2,
                                 nrow = NULL,
                                 ncol = NULL,
                       plots.per.page = 9,
                           one.by.one = FALSE,
                            title,...)
{
if (is(data, "list"))  
  stop("the data is list  the function needs a data.frame") 
if (is(data, "table")) 
  stop("the data is a table the function needs a data.frame")
  if (is(data, "matrix"))    data <- as.data.frame(data)
  if (is(data[1],"mts"))     data <- as.data.frame(data)
  if (is(data, "array")) stop("the data is an array the function needs a data.frame")
      Y <-  deparse(substitute(response))
  if (any(!(Y %in%names(data)))) stop("the response should be in data") 
  Names <- names(data)
  pos <- match(Y, Names)
  daTa <- data[,-pos] # data without response 
  class_Vars <- sapply(daTa,class)
  daTa <- daTa[,(class_Vars=="numeric")|(class_Vars=="integer")]# only numeric
  Namesnum <- names(daTa)
  daTa <- data[, c(Namesnum, Y) ]# new data with numeric + response  
  PP <- list() 
  # cY <- class(actY) 
  I <- 1
  for (i in Namesnum)
  {
    GG1 <-  daTa |> ggplot(aes(x = .data[[i]], .data[[Y]]))+geom_point()+
      ggtitle("no trans")
    PP[[I]] <- GG1
    I <- I + 1
    #  cat(I,"\n")
    GG2 <-  daTa |> ggplot(aes(x = sqrt(.data[[i]]), .data[[Y]]))+
      geom_point()+ggtitle("sqrt")
    PP[[I]] <- GG2  
    I <- I + 1
    #         cat(I,"\n")
    GG3 <-  daTa |> ggplot(aes(x = log(.data[[i]]), .data[[Y]]))+
      geom_point()+ggtitle("log")
    PP[[I]] <- GG3  
    I <- I + 1
    #           cat(I,"\n")
  } 
  n.plots <- length(PP)    
  if (one.by.one)
  {
    oask <- devAskNewPage(one.by.one)
    on.exit(devAskNewPage(oask))
    for (i in 1:n.plots) print(PP[[i]])
  } 
  else
  { # multiple plots
    ################################################################# 
    define_region <- function(row, col){
      viewport(layout.pos.row=row, layout.pos.col=col) }
    #################################################################  
    if (n.plots>plots.per.page)
    {
      pages <- ceiling(n.plots/plots.per.page)  
      page <- n.plots%/%plots.per.page
      ppp <- rep(plots.per.page,page) 
      if (n.plots%%plots.per.page != 0) ppp <- c(ppp, n.plots%%plots.per.page)
      if (plots.per.page==9)
      {
        nc <- 3
        nr <- 3
        IJ <- expand.grid(j=1:nc, i=1:nr) 
      } else
      {
        if (is.null(nrow)||is.null(nrow)) stop("the nrow and ncol need to be defined") 
        if (plots.per.page> ncol*nrow) stop("the nrow or ncol has to increase") 
        nc <- ncol
        nr <- nrow
        IJ <- expand.grid(j=1:nc, i=1:nr) 
        IJ <- IJ[1:plots.per.page,]
      }  
      start <- 1
      finish <- ppp[1]
      for (pa in 1:pages)
      {
        grid.newpage()
        pushViewport(viewport(layout=grid.layout(nrow=nr,ncol=nc)))   
        for (p  in start:finish) 
        {
          print(PP[[p]], vp=define_region(IJ$i[p],IJ$j[p]))
        }
        start <- finish +1
        finish <- finish+ppp[pa+1] 
        IJ <- rbind(IJ, IJ)
        oask <- devAskNewPage(ask=TRUE)
        on.exit(devAskNewPage(oask))
      } 
    } else  
    {
      pages <- 1
      ppp <- n.plots%/%pages
      nc  <- nr <- trunc(sqrt(ppp))
      if (nc < 1)        nr <- nc <- 1
      if (nc * nr < ppp) nc <- nc + 1
      if (nc * nr < ppp) nr <- nr + 1 
      IJ <- expand.grid(j=1:nc, i=1:nr)
      grid.newpage()
      pushViewport(viewport(layout=grid.layout(nrow=nr,ncol=nc)))
      for (p  in 1:n.plots) 
      {
        print(PP[[p]], vp=define_region(IJ$i[p],IJ$j[p]))
      }
    }      
  } 
  on.exit( pushViewport(viewport(layout=grid.layout(nrow=1,ncol=1))))        
  invisible(PP)         
}


################################################################################
################################################################################
################################################################################
################################################################################
# function 
# get only continuous variables??
data_continuous <- function(data, response)
{
if (is(data, "list"))  stop("the data is list  the function needs a data.frame") 
if (is(data, "table")) stop("the data is a table the function needs a data.frame")
if (is(data, "matrix"))    data <- as.data.frame(data)
if (is(data[1],"mts"))     data <- as.data.frame(data)
if (is(data, "array")) stop("the data is an array the function needs a data.frame")
          Y <-  deparse(substitute(response))
if (any(!(Y %in%names(data)))) stop("the response should be in data") 
     Names <- names(data)
       pos <- match(Y, Names)
      daTa <- data[,-pos] # data without response 
class_Vars <- sapply(daTa,class)
      daTa <- daTa[,(class_Vars=="numeric")|(class_Vars=="integer")]# only numeric
daTa    
}
################################################################################
################################################################################
################################################################################
################################################################################
# TIME functions 
################################################################################
################################################################################
# function 16 for time series
datetime2datehour <- function(datetime, format=NULL)
{
            X <- t(as.data.frame(strsplit(datetime,' ')))
  rownames(X) <- NULL
  colnames(X) <- c("date", "time")
         hour <- as.numeric(sub(":",".",X[,2]))
         date <- as.Date(X[,1],format=format)
  data.frame(date, hour)
}
################################################################################
################################################################################
################################################################################
################################################################################
# function 16 for time series
time2num <- function(time, pattern=":")
{
t <-  gsub(pattern, ".", time)
as.numeric(t)
}
################################################################################
################################################################################
################################################################################
################################################################################
# END of TIME functions
################################################################################
################################################################################
# function 3
data_formula <- function(data, response)
{
if (is(data, "list"))  
    stop("the data is list  the function needs a data.frame") 
if (is(data, "table")) 
    stop("the data is a table the function needs a data.frame")
if (is(data, "matrix"))    data <- as.data.frame(data)
if (is(data[1],"mts"))     data <- as.data.frame(data)
if (is(data, "array")) 
    stop("the data is an array the function needs a data.frame")
      Y <- deparse(substitute(response))
if (any(!(Y %in%names(data)))) stop("the response should be in data") 
  #.   Names <- names(data)
    pos <- match(Y, names(data))
  nameS <- names(data)[-pos]
     PP <- list() 
   actY <- data[,Y]
     cY <- class(actY) 
      I <- 0
     
     f1 <- formula(paste(paste0(Y,"~"),paste0(nameS, collapse='+')), 
                   data=data,      envir=globalenv())#.GlobalEnv
     f2 <- formula( paste0(paste0(Y,"~"), 
                    paste0("(",paste0(nameS, collapse='+'),")^2")), 
                   data=data,      envir=globalenv())#.GlobalEnv
     f3 <- formula(paste("~",paste0(nameS, collapse='+')), 
                   data=data,      envir=globalenv())#.GlobalEnv
     f4 <- formula( paste0(paste0("~"), 
                           paste0("(",paste0(nameS, collapse='+'),")^2")), 
                    data=data,      envir=globalenv())#.GlobalEnv
  invisible(list(rme=f1,rint=f2, me=f3, int=f4))         
}
################################################################################
################################################################################
################################################################################
################################################################################
data_exclude <- function(data, class="factor")
{
if (is(data, "list"))  
    stop("the data is list  the function needs a data.frame") 
if (is(data, "table")) 
    stop("the data is a table the function needs a data.frame")
if (is(data, "matrix"))    data <- as.data.frame(data)
if (is(data[1],"mts"))     data <- as.data.frame(data)
if (is(data, "array"))   
  stop("the data is an array the function needs a data.frame")
  CData <- sapply(data,class)
     da <- subset(data, select=CData!=class)
da
}  

