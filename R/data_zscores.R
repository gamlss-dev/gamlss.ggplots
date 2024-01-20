################################################################################
################################################################################
################################################################################
################################################################################
# function 1
y_zscores <- function(x, 
                    family = SHASHo, 
                      plot = TRUE, 
                      hist = FALSE,...)
{
   name <- deparse(substitute(x))
     m1 <- gamlssML(x, family=family)
zscores <- resid(m1)
  param <- names(as.gamlss.family(family)$par)
 lparam <- length(param)
if ("mu"%in%param)        mu <- fitted(m1,"mu")[1]
if ("sigma"%in%param)  sigma <- fitted(m1,"sigma")[1]
if ("nu"%in%param)        nu <- fitted(m1,"nu")[1]
if ("tau"%in%param)      tau <- fitted(m1,"tau")[1]
attr(zscores, "parameter") <-   switch(lparam, c("mu"),
                                     c("mu", "sigma"),
                                     c(mu, sigma, nu),
                                     c(mu, sigma, nu, tau))  
if (plot) 
{
   title <- paste0("z-scores of ",name)
  if (hist) print(gamlss.ggplots::y_hist(zscores)+
                  ggplot2::xlab(name))
  else      print(gamlss.ggplots::y_dots(zscores)+
                  ggplot2::xlab(name))
}
invisible(zscores)
}
################################################################################
################################################################################
################################################################################
################################################################################
# function 
data_zscores <- function(data, 
                          plot = TRUE,
                          hist = FALSE,
                         value = 3, 
                        family = SHASHo,
                    max.levels = 10,
                      hist.col = "black", 
                     hist.fill = "white", 
                     dens.fill = "#FF6666", 
                          nrow = NULL,
                          ncol = NULL,
                plots.per.page = 9,
                    one.by.one = FALSE,
                         title,
                    percentage = TRUE, # for big data take % of data
                          seed = 123,
                        ...)
{
# data   
if (is(data, "list"))  
  stop("the data is list  the function needs a data.frame") 
if (is(data, "table")) 
  stop("the data is a table the function needs a data.frame")
if (is(data, "matrix"))    data <- as.data.frame(data)
if (is(data[1],"mts"))     data <- as.data.frame(data)
if (is(data, "array")) 
  stop("the data is an array the function needs a data.frame")
       dimD <- dim(data)
# checking data  
if (any(is.na(data)))
  {
       l1 <- dim(data)[1]
     data <- na.omit(data)
       l2 <- dim(data)[1]
warning(cat(l1-l2, "observations were omitted from the data", "\n"))
  }
#  if is a list or table
if (is.null(dimD)) stop("only one variable in the data") 
if (percentage)
       {
         # check the size of the data 
         nobs <- dim(data)[1] 
         per <- ifelse(nobs<50000,1,         # all data 
                       ifelse(nobs<100000,.5,# 50% of data 
                              ifelse(nobs<1000000,.2, # 20% of data 
                                     ifelse(nobs>1000000,1))))  # 10% of data  
         set.seed(seed)
         ind <- sample(nobs, per*nobs)
         data <- data[ind,]
       }       
  sat.cont <- sapply(data,is.factor)|sapply(data,is.character)|
    data_distinct(data, get.distinct=TRUE) < max.levels|
              sapply(data, function(x) is(x, class2="Date"))
      daTa <- subset(data,  select=!sat.cont)  
       Dim <- dim(daTa)
if (Dim[2]==0) stop("no variable is left after taking out the factors")         
if (Dim[2]==1) stop("only one variable is left after taking out the factors")   
     Names <- names(daTa)
class_Vars <- sapply(daTa,class)
        PP <- list()
     zlist <- list()
for (i in 1:length(class_Vars))
{
      izsc <- y_zscores(daTa[,i], family=family, plot=FALSE) 
  if  (any(!is.finite(izsc)))
  {
        II <- which(!(is.finite(izsc)))
      izsc <-  izsc[which(is.finite(izsc))]
      warning("observations ", II, " taken out in ", Names[i],"\n" )
  }
      zlist[[i]] <- izsc
if (plot)
 {
  PP[[i]] <- if (hist)  y_hist(izsc,  title="") + ggplot2::xlab(Names[i]) 
             else y_dots(izsc,  title="") + ggplot2::xlab(Names[i])
 }
} #end of loop
if (plot)
{
  n.plots <- length(PP)       
 if (one.by.one)
  {
    oask <- devAskNewPage(one.by.one)
    on.exit(devAskNewPage(oask))
    for (i in 1:n.plots) print(PP[[i]])
  } else
  { # multiple plots
################################################################################ 
define_region <- function(row, col){
      grid::viewport(layout.pos.row=row, layout.pos.col=col) }
################################################################################  
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
        grid::grid.newpage()
        grid::pushViewport(grid::viewport(layout=grid.layout(nrow=nr,ncol=nc)))   
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
    if (nc < 1)    nr <- nc <- 1
    if (nc * nr < ppp) nc <- nc + 1
    if (nc * nr < ppp) nr <- nr + 1 
         IJ <- expand.grid(j=1:nc, i=1:nr)
      grid::grid.newpage()
      grid::pushViewport(grid::viewport(layout=
                          grid::grid.layout(nrow=nr,ncol=nc)))
  for (p  in 1:n.plots) 
      {
        print(PP[[p]], vp=define_region(IJ$i[p],IJ$j[p]))
      }
    }      
  } 
  on.exit( grid::pushViewport(grid::viewport(layout=
                                   grid::grid.layout(nrow=1,ncol=1))))        
  return(invisible(PP))   
}  
               mm <- matrix(unlist(zlist), ncol=length(zlist))   
               MM <- as.data.frame(mm)
        names(MM) <- Names
return(MM)
}
################################################################################
################################################################################
################################################################################
################################################################################