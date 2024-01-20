# function 
data_plot <- function(data, 
                      value = 3, 
                   hist.col = "black", 
                  hist.fill = "white", 
                  dens.fill = "#FF6666", 
                       nrow = NULL,
                       ncol = NULL,
                 percentage = TRUE, # for big data take % of data
                 seed = 123,
                  plot.hist = TRUE,
             plots.per.page = 9,
                 one.by.one = FALSE,
                      title,...)
{
# what is the data
if (is(data, "list"))  stop("the data is list  the function needs a data.frame") 
if (is(data, "table")) 
  stop("the data is a table the function needs a data.frame")
if (is(data, "matrix"))    data <- as.data.frame(data)
if (is(data[1],"mts"))     data <- as.data.frame(data)
if (is(data, "array")) stop("the data is an array the function needs a data.frame")
if (percentage)
{
  nobs <- dim(data)[1] 
  # check the size of the data 
  per <- ifelse(nobs<50000,1,         # all data 
                ifelse(nobs<100000,.5,# 50% of data 
                       ifelse(nobs<1000000,.2, # 20% of data 
                              ifelse(nobs>1000000,1))))  # 10% of data
  set.seed(seed)
  ind <- sample(nobs, per*nobs)
  data <- data[ind,]
}

# checking data  
  class_Vars <- sapply(data,class)
if (any(class_Vars=="character"))
  {
    chr.pos <- match("character",class_Vars)
    data <- data[,-chr.pos] 
  }
       Names <- names(data)
  class_Vars <- sapply(data,class)
          PP <- list()
for (i in 1:length(class_Vars))
  {
    if (class_Vars[i]%in%c("integer", "factor", "character"))
    {
      PP[[i]] <- ggplot2::ggplot(data, 
                  ggplot2::aes( .data[[Names[i]]]))+ xlab(Names[i]) + 
        ggplot2::geom_bar()
    } else 
    {
      if (plot.hist)
      {
        PP[[i]] <-   y_hist(data[,i], title="",...) + xlab(Names[i])  
      } else 
      {
        PP[[i]] <-   y_dots(data[,i], title="", value=value,...) + xlab(Names[i])  
      }  
    }
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
data_response <- function(data, 
                          response, 
                          plot=TRUE, 
                          percentage = TRUE) # for big data take % of data)
{
  #  what is the data
  if (is(data, "list"))  stop("the data is list  the function needs a data.frame")
  if (is(data, "table")) stop("the data is a table the function needs a data.frame")
  if (is(data, "matrix"))    data <- as.data.frame(data)
  if (is(data[1],"mts"))     data <- as.data.frame(data)
  if (is(data, "array")) stop("the data is an array the function needs a data.frame")
  if (percentage)
  {
    nobs <- dim(data)[1] 
    # check the size of the data 
    per <- ifelse(nobs<50000,1,         # all data 
                  ifelse(nobs<100000,.5,# 50% of data 
                         ifelse(nobs<1000000,.2, # 20% of data 
                                ifelse(nobs>1000000,1))))  # 10% of data    
    ind <- sample(nobs, per*nobs)
    data <- data[ind,]
  }  
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
