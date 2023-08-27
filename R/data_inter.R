################################################################################
################################################################################
################################################################################
################################################################################
# INTERACTIONS
################################################################################
################################################################################
# fitting  interactions for a data.frame
# it check whether they are factor and only show the
# interactions for continuous variables
# to do : it needs also factors fitting
################################################################################ 
# This version uses foreach
data_inter <- function(data, 
                       response, 
                       weights,
                     digits = 3,
                       plot = TRUE,
                   diag.off = TRUE,
              lower.tri.off = TRUE,  
                     method = c("circle", "square"),
                 fit.method = c("linear", "nonlinear"),
              outline.color = "gray",
                     colors = c("blue", "white", "red"),
               legend.title = "Inter",
                      title,
                    ggtheme = theme_minimal(),
                     tl.cex = 12,
                     tl.col = "black", 
                     tl.srt = 45,
                        lab = TRUE, 
                    lab_col = "black", 
                   lab_size = 3,
                circle.size = 20,
                       ...) # c(1,15) maybe will do
{
################################################################################
################################################################################
# require(foreach)
################################################################################
################################################################################
# local function 
meltit <- function(mat)
  {
     rna <- rownames(mat)
    lrna <- length(rna)
   value <- as.vector(mat)
    Var1 <- gl(length(rna), 1, length = lrna*lrna, labels=rna)
    Var2 <- gl(length(rna), lrna, length = lrna*lrna, labels=rna)
     daf <-  na.omit(data.frame(Var1, Var2, value=value)) 
    daf
  }
################################################################################
################################################################################
pw_inter<-function(y, x1, x2, weights)
  {
      N <- length(y)
if (missing(weights)) weights <- rep(1,N)
   DaTa <- data.frame(y=y, x1 = x1, x2 = x2)
     m1 <- lm(y~x1*x2, data=DaTa, weights=weights)
     m0 <- lm(y~x1+x2, data=DaTa, weights=weights)
    CHI <-  1-pchisq(AIC(m0, k=0)-AIC(m1, k=0), df=1)
    CHI
  }
################################################################################
################################################################################
pw_nl_inter<-function(y, x1, x2, weights)
{
     N <- length(y)
if (missing(weights)) weights <- rep(1,N)
  DaTa <- data.frame(y=y, x1 = x1, x2 = x2)
    m1 <- gam(y~s(x1,x2), data=DaTa)
    m0 <- gam(y~s(x1)+s(x2), data=DaTa)
   CHI <- 1-pchisq(AIC(m0, k=0)-AIC(m1, k=0),df=
                      (m0$df.residual-m1$df.residual))
   CHI
}
################################################################################
################################################################################
################################################################################
# main function stats here
  i <- j <- NULL
  fit.method <- match.arg(fit.method)
if (missing(data) || NROW(data) <= 1) 
    stop("nothing to do for this data frame")
# data obs na's
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
  actY <- data[,Y]
# standardise data  
  stdY <- y_zscores(actY, plot=FALSE)
   pos <- match(Y,names(data))
daTA  <- data[,-pos] # take out response
   TT <- lapply(daTA, unique)
  # browser()
  #         ifelse(sapply(daTA,is.factor)
  #        |
  #          sapply(daTA,is.character)|
  #          apply(daTA,2,FUN=is.integer)&sapply(TT, length)<10, FALSE, TRUE)
    daTa <-  subset(daTA, select= 
              ifelse(sapply(daTA,is.character)|
                     apply(daTA,2,FUN=is.integer)&
                     sapply(TT, length)<10, FALSE, TRUE))
     namesD <- names(daTa)
       daTa <- cbind(daTa, actY)
names(daTa) <- c(namesD, Y) 
       dimD <- dim(data)
if (any(is.na(data)))
  {
      l1 <- dim(data)[1]
    data <- na.omit(data)
      l2 <- dim(data)[1]
    warning(cat(l1-l2, "observations were omitted from the data", "\n"))
  }
if (is.null(dimD)) stop("only one variable in the data") 
if (dimD[1]<20)    stop(cat("the size of the data set is too small", "\n",
                             "to detect interaction", "\n"))   
      Dim <- dim(daTa)
if (Dim[2]==0) stop("no variable is left after taking out unwanded variables")         
if (Dim[2]==1) stop("only one variable is left")    
  diffDim <- dimD[2]-Dim[2]
if (diffDim > 0)
{
    warning(cat(diffDim, 'variables have been omited from the data', "\n"))
} 
   cnames <-  namesD
  lcnames <- length(cnames)
       CC <- matrix(0, ncol=lcnames, nrow=lcnames)
       CC <- foreach(i=1:lcnames, .combine='rbind') %dopar% 
{
       xi <- if(is.null(dim(daTa[,i]))) daTa[,i] else daTa[,i][,1]
foreach(j=1:lcnames, .combine='c') %do% 
        {
       xj <- if(is.null(dim(daTa[,j]))) daTa[,j] else daTa[,j][,1] 
             if (fit.method=="linear")
          {
            if (i<j) CC[i,j]  <- pw_inter(y=daTa[,Y], x1=xi, x2=xj)
            else CC[i,j] <- 0  
          } else
          {
            if (i<j) CC[i,j]  <- pw_nl_inter(y=daTa[,Y], x1=xi, x2=xj)
            else CC[i,j] <- 0   
          } 
        }
  }
             CC <- CC+t(CC)  # to get the full matrix (rather than diagonal)
   rownames(CC) <- cnames
   colnames(CC) <- cnames
       diag(CC) <- NA
             CC <- base::round(x = CC, digits = digits)   
if (diag.off) diag(CC) <- NA
if  (lower.tri.off) CC[lower.tri(CC)] <- NA
if (plot==FALSE) return(CC)
         method <- match.arg(method)
          inter <- meltit(CC)
       lowerLim <- 25-floor((range(inter$value)[2]-range(inter$value)[1])*20)
colnames(inter) <- c("var_1", "var_2", "value")
  if (fit.method=="linear")
  {
      txt.title <- if (missing(title))
      paste("Linear pair-wise interactions for:", deparse(substitute(data)))
    else title
  } else 
  {
      txt.title <- if (missing(title))
      paste("Nonlinear pair-wise interactions for:", deparse(substitute(data)))
    else title 
  }  
  #inter$abs_inter <- (1-abs(inter$value)) * 10
  inter$abs_inter <- ifelse((1-abs(inter$value) > 0.95), 
                            (1-abs(inter$value)) * 10, 
                            1-abs(inter$value))
  p <- ggplot(data = inter,
              mapping = aes_string(x = "var_1", y = "var_2",
                                   fill = "value"))
  
  # p <- ggplot(data = inter,
  #             mapping = aes(x = .data[["var_1"]], y = .data[["var_2"]], 
  #                           fill = "value"))
  if (method == "square")################# square
  {
    p <- p + geom_tile(color = outline.color)
  }
  else if (method == "circle")  # cirle
  {
    p <- p + geom_point(color = outline.color, shape = 21,
                        # aes_string(size = .data[["abs_inter"]])+
                        aes_string(size = "abs_inter")) +
      # scale_size(range = circle_scale_size_range) +
      scale_size_area(max_size = circle.size) +
      guides(size = "none")
  }
  label <- round(x = CC, digits = digits)
  p <- p + scale_fill_gradient2(low = colors[3], high = colors[1],
                                mid = colors[2],  midpoint = 0.05, limit = c(0, .1),
                                space = "Lab",
                                name = legend.title)+ggtitle(txt.title)
  if (class(ggtheme)[[1]] == "function") {
    p <- p + ggtheme()
  }
  else if (class(ggtheme)[[1]] == "theme") {
    p <- p + ggtheme
  }
  p <- p + theme(axis.text.x = element_text(angle = tl.srt,
                                            vjust = 1, size = tl.cex, colour=tl.col, hjust = 1),
                 axis.text.y = element_text(size = tl.cex, colour=tl.col)) +
    coord_fixed()
  label <- round(x = inter[, "value"], digits = digits)
  if (lab) {
    p <- p + ggplot2::geom_text(
      mapping = aes_string(x = "var_1", y = "var_2"),
      label = label, color = lab_col, size = lab_size)
  }
  p# you also need the data as output 
}          
################################################################################
################################################################################
################################################################################
################################################################################


