################################################################################
################################################################################
################################################################################
# correlations coefficients from  a data.frame
# it check whether they are factor and only show the
# correlation for continuous variables
# i) data_cor()
# ii) which_data_cor() is equivalent to which.Data.Corr() from 
#     gamlss.foreach
#     
################################################################################
################################################################################
################################################################################
#require(igraph)
#require(networkD3)
################################################################################
################################################################################
################################################################################
data_cor <- function(data, 
                     digits = 3,
                       plot = TRUE,
                   diag.off = TRUE,
              lower.tri.off = FALSE,  
                     method = c("square", "circle"),
              outline.color = "gray",
                     colors = c("blue", "white", "red"),
               legend.title = "Corr",
                      title,
                    ggtheme = theme_minimal(),
                     tl.cex = 12,
                     tl.col = "black", 
                     tl.srt = 45,
                        lab = TRUE, 
                    lab_col = "black", 
                   lab_size = 3,
                circle.size = 20  ) 
{
####################################################################
####################################################################
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
#################################################################### 
####################################################################  
# data.frame missing
if (missing(data) || NROW(data) <= 1) 
    stop("nothing to do for this data frame")
# data obs na's
if (is(data, "list"))  stop("the data is list  the function needs a data.frame") 
if (is(data, "table")) stop("the data is a table the function needs a data.frame")
if (is(data, "matrix"))    data <- as.data.frame(data)
if (is(data[1],"mts"))     data <- as.data.frame(data)
if (is(data, "array")) stop("the data is an array the function needs a data.frame")
            dimD <- dim(data)
if (any(is.na(data)))
  {
    l1 <- dim(data)[1]
  data <- na.omit(data)
    l2 <- dim(data)[1]
  warning(cat(l1-l2, "observations were omitted from the data", "\n"))
  }
#  if is a list or table
if (is.null(dimD)) stop("only one variable in the data") 
if (dimD[1]<20)   stop(cat("the size of the data set is too small", "\n",
                                "to detect non-linear correlations", "\n"))              
        daTa <- subset(data,  select=ifelse(sapply(data,is.factor)|
              sapply(data,is.character)==TRUE, FALSE, TRUE))
        Dim <- dim(daTa)
if (Dim[2]==0) stop("no variable is left after taking out the factors")             
if (Dim[2]==1) stop("only one variable is left after taking out the factors")              
        diffDim  <- dimD[2]-Dim[2]        
if (any(is.na(data)))
        {
           l1 <- dim(data)[1]
          data <- na.omit(data)
            l2 <- dim(data)[1]
          warning(cat(l1-l2, "observations were omitted from the data", "\n"))
        }
if (is.null(dimD)) stop("only one variable in the data")         
if (diffDim > 0)
               {
               warning(cat(diffDim, 'factors have been omited from the data', "\n"))
              }
              CC <- cor(daTa)
              CC <- base::round(x = CC, digits = digits)
if ( diag.off) diag(CC) <- NA
if (lower.tri.off)  CC[lower.tri(CC)]<-NA            
       txt.title <- if (missing(title))  
          paste("Correlations from data",deparse(substitute(data)))
              else title  
if (plot==FALSE) return(CC)
          method <- match.arg(method)
            corr <- meltit(CC)
            var_1 <-  var_2 <-  value <- NULL
  colnames(corr) <- c("var_1", "var_2", "value")
       txt.title <- if (missing(title))  
                paste("Correlations from data",deparse(substitute(data)))
  else title  
   corr$abs_corr <- abs(corr$value) * 10
               p <- ggplot2::ggplot(data = corr, 
                ggplot2::aes(x = .data[["var_1"]], y = .data[["var_2"]], 
                                        fill = .data[["value"]]))
if (method == "square") {
               p <- p + ggplot2::geom_tile(color = outline.color)
  }
else if (method == "circle") {

    p <- p + ggplot2::geom_point(color = outline.color, shape = 21, 
             ggplot2::aes(size = .data[["abs_corr"]])) +
            # scale_size(range = c(5, 20)) +
      ggplot2::scale_size_area(max_size = circle.size) +
      ggplot2::guides(size = "none")
}
      label <- round(x = CC, digits = digits)               
  p <- p + ggplot2::scale_fill_gradient2(low = colors[1], high = colors[3], 
            mid = colors[2], midpoint = 0, limit = c(-1, 1), space = "Lab",
            name = legend.title)+
            ggplot2::ggtitle(txt.title)
  if (class(ggtheme)[[1]] == "function") {
    p <- p + ggtheme
  }
  else if (class(ggtheme)[[1]] == "theme") {
    p <- p + ggtheme
  }
  p <- p + ggplot2::theme(axis.text.x = element_text(angle = tl.srt, 
                vjust = 1, size = tl.cex, , colour=tl.col, hjust = 1), 
                axis.text.y = element_text(size = tl.cex, colour=tl.col)) + 
                ggplot2::coord_fixed()
  label <- round(x = corr[, "value"], digits = digits)  
  if (lab) {
    p <- p + ggplot2::geom_text(mapping = ggplot2::aes(x = .data[["var_1"]], 
                                                     y = .data[["var_2"]]), 
                  label = label, color = lab_col, size = lab_size)
  }
  p
}
################################################################################
################################################################################
################################################################################
################################################################################
#-------------------------------------------------------------
which_data_cor <- function(data, r=.90, digits=3, plot=FALSE, igraph=TRUE)
{
if (abs(r)>=1||abs(r)<=0) stop("r should be greater than  0 and lass than 1")
  daTa <- subset(data,  select=ifelse(sapply(data,is.factor)|sapply(data,is.character)==TRUE, FALSE, TRUE))
   Dim <- dim(daTa)
    CC <- cor(daTa)
    CC <- base::round(x = CC, digits = digits)
   CCC <- CC-diag(rep(1,Dim[2]))
if (is.null(colnames(daTa))) colnames(daTa) <- paste0("X", seq(1:dim(data)[2]))
if (!any(which(abs(CCC)>r))) return(cat("no correlation above", r, "\n"))
    mm <- which(abs(CCC)>r, arr.ind=T)
    nn <- mm[mm[,1]< mm[,2],]
if (is.vector(nn))
  {
    name1 <- colnames(data)[nn[1]]
    name2 <- colnames(data)[nn[2]]
    corrs <- CCC[nn[1],nn[2]]
  } else
  { 
   name1 <- colnames(data)[nn[,1]]
   name2 <- colnames(data)[nn[,2]]
   corrs <- CCC[nn]
  }
M <-  cbind(name1, name2, corrs)
if (plot)
{
 # dd <- which.Data.Corr(InfMort, r=0.5)[,c(1,2)]
#  network <- graph_from_data_frame(d=dd, directed=F) 
  # plot it
#  plot(network)
#  InfMort |> which.Data.Corr( r=0.5) |>  as.data.frame() |> 
#    simpleNetwork(CC, height="100px", width="100px")
  dd <- as.data.frame(M)
 if (igraph) plot(igraph::graph_from_data_frame(d=dd, directed=F)) 
  else{
    p <- networkD3::simpleNetwork(dd, height="100px", width="100px")  
    print(p)  
  }
} else 
  return(M) 
}
###############################################################################