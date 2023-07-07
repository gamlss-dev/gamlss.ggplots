################################################################################
################################################################################
################################################################################
################################################################################
# function 3
data_leverage <- function(data, response, weights, 
                  point.size = 0.5, 
                        nrow = NULL,
                        ncol = NULL,
                   quan.val = 0.99, 
                   annotate = TRUE,
                   line.col = "steelblue4",
                  point.col = "steelblue4", 
                  annot.col = "darkred", 
                  max.levels = 10,#
                        plot = TRUE,  
                        title,...)#value = NULL#quan.val = 0.99, 
{
########################################################################
# local functions 
gamlss_prep_data <- function (hat, weights, quan.val) 
  {
    value <-  quantile(hat, quan.val)
      obs <- seq_len(length(hat))
      hat <- hat[weights!=0]
      out <- data.frame(obs = obs, hat = hat)
out$color <- ifelse((out$hat >= value), 
                        c("outlier"), c("normal"))
out$fct_color <- ordered(factor(out$color), levels = 
                               c("normal", "outlier"))
  out$txt <- ifelse(out$color == "outlier", out$obs, NA)
  out$value <- value
    return(out)
  } 
#####################################################################  
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
if (missing(weights)) weights <- rep(1, dim(data)[1])       
          dv <- y_distinct(data[,Y])
if (dv < max.levels) 
  stop("the response do not seems to have many distinct values")
  #.   Names <- names(data)
  class_Vars <- sapply(data,class)
if (any(class_Vars=="character"))## take out character variables
  {
    chr.pos <- match("character",class_Vars)
       data <- data[,-chr.pos] 
  }
        pos <- match(Y, names(data))
      nameS <- names(data)[-pos] ## get out the response 
         f1 <- formula(paste(paste0(Y,"~"),paste0(nameS, collapse='+')), 
                    data=data,      envir=globalenv())#.GlobalEnv
         m1 <- lm(f1, weights=weights, data=data)
        lev <- hatvalues(m1)
          r <- m1$rank
          N <- dim(data)[1]
          d <- gamlss_prep_data(lev,weights=weights, quan.val=quan.val )
     lev
txt.title <- if (missing(title))  paste("Linear leverage of data",
                                        deparse(substitute(data)))
             else title
        obs <-  value <- txt <-  NULL
          f <- d[d$color == "outlier", c("obs", "hat")]
colnames(f) <- c("observation", "quan_resid")
         gg <- ggplot2::ggplot(d, ggplot2::aes(x = obs, y = hat, label = txt, 
                                               ymin = 0, ymax = hat)) + 
           ggplot2::geom_linerange(colour = line.col ) + 
           ggplot2::geom_point(shape = 1, colour = point.col  ) + 
           ggplot2::xlab("Observation number") + # working  with facet_wrap 
           ggplot2::ylab("linear leverage") + # working  with facet_wrap 
           ggplot2::ggtitle(txt.title) +  # working  with facet_wrap 
           ggplot2::geom_text(hjust = -0.2, nudge_x = 0.15, size = 3, 
                         family = "serif",
                 fontface = "italic", colour = annot.col, na.rm = TRUE)  
    
     p <- gg + 
       ggplot2::geom_hline(yintercept=2*(r/N), col=annot.col)+
       if (annotate) ggplot2::annotate("text", x = Inf, y = Inf, hjust = 1.5, 
                                       vjust = d$value,
                  family = "serif", fontface = "italic", colour = annot.col,
                              label = paste0("Threshold: abs(", d$value, ")"))
     if (plot) {
       suppressWarnings(return(p))
     }
     else {
       return(list(leverage = d$hat, index=d$obs, threshold = d$value))
     }
}
################################################################################
################################################################################
################################################################################
################################################################################