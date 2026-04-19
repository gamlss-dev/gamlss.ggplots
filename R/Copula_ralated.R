################################################################################
################################################################################
################################################################################
################################################################################
# function 1
# piecewise median in 2-dim 
median_cw <- function(x,y)
{
if (is.null(y)) # data handiling
  {
    if (is.matrix(x) || is.data.frame(x)) 
    {
      y <- x[, 2]
      x <- x[, 1]
    } else 
    {
      stop("Provide either (x, y) or a 2-column matrix/data.frame")
    }
  }  
  out <- c(median(x), median(y))
  return(out)
}
# median_cw(da1$x, da1$y)
################################################################################
################################################################################
################################################################################
################################################################################
# function 2
# principle component median
median_pc <- function(x,y)
{
if (is.null(y)) # data handiling
  {
    if (is.matrix(x) || is.data.frame(x)) 
    {
      y <- x[, 2]
      x <- x[, 1]
    } else 
    {
      stop("Provide either (x, y) or a 2-column matrix/data.frame")
    }
  }    
        df  <- data.frame(cbind(x,y))
         pp <- gamlss.foreach::getSVD(df)
          T <- pp$u*pp$d
  beta_coef <- sum(T[,1]*df[,2])/pp$d[1]^2
         fv <- mean(df[,2])+beta_coef*mean(df[,1])+beta_coef*T[,1]
    x_point <- median(fv) 
    y_point <- mean(df[,2])++beta_coef*mean(df[,1])+beta_coef*x_point
        out <- c(x_point, y_point)
        out <- c(median(x), median(y))
  return(out)
}
# median_pc(da1$x, da1$y)
################################################################################
################################################################################
################################################################################
################################################################################
# function 3
median_sp <- function(x, y) 
{
  if (is.null(y)) # data handiling
  {
    if (is.matrix(x) || is.data.frame(x)) 
    {
      y <- x[, 2]
      x <- x[, 1]
    } else 
    {
      stop("Provide either (x, y) or a 2-column matrix/data.frame")
    }
  }
 ## local function   
  fn <- function(par) 
  {
    sum(sqrt((x - par[1])^2 + (y - par[2])^2))
  }
out <-   optim(c(median(x), median(y)), fn)$par
return(out)
}
################################################################################
################################################################################
################################################################################
################################################################################
# fitted_pc_lines <- function(x,y, plot=TRUE) 
# {
#   require(gamlss.foreach)   
#   if (is.null(y)) # data handiling
#   {
#     if (is.matrix(x) || is.data.frame(x)) 
#     {
#       y <- x[, 2]
#       x <- x[, 1]
#     } else 
#     {
#       stop("Provide either (x, y) or a 2-column matrix/data.frame")
#     }
#   }
#   if (length(x)!=length(y)) stop("x and y lengths differ")
#      df <- data.frame(x=x, y=y)  
#      pc <- getSVD(df)
#       T <- pc$u*pc$d
#   beta1 <- sum(T[,1]*y)/pc$d[1]^2 
#   beta2 <- sum(T[,2]*y)/pc$d[2]^2
#     fv1 <- mean(y)+beta1*x
#     fv2 <- mean(y)+beta2*x
# if (plot)
# {
#   browser()
#   plot(df)
#   lines(fv1~x)
#   abline(a=(mean(y)+beta1*mean(x)), b=beta1, col="red") 
#   abline(a=mean(y)), b=beta2, col="blue")
# }  
#   
#   out<- data.frame(c1=fv1, c2=fv2)
#   return(out)
# }
################################################################################
################################################################################
################################################################################
################################################################################
# cw: component wise, 
# pc: principal component, 
# sp: spatial 
# function 4
xy_median <- function(x, y, method = c("cw", "pc", "sp")) 
{
method <- match.arg(method)
if (method == "cw") 
 {
  out <- median_pc(x,y)
 } 
if (method == "sp") 
 {
  out <- median_sp(x, y)
 }
if (method == "pc") 
 {
  out <- median_pc(x, y)
 }  
return(out)
}
################################################################################
################################################################################
################################################################################
################################################################################
# function 5
# cw: component wise, 
# pc: principal component, 
# sp: spatial 
xy_scatter <-function(x, y = NULL,
                  method = c("cw", "pc", "sp"), 
                 density = TRUE,
                     HDR = TRUE,
          density.colour = "lightblue",
           median.colour = "red", 
             median.size = 3, 
              point.size = 1,
            point.colour = "grey",
              HDR.colour = "darkred")
{
  z <- NULL
if (is.null(y)) 
  {
    if (is.matrix(x) || is.data.frame(x)) 
    {
      y <- x[, 2]
      x <- x[, 1]
    } else 
    {
      stop("Provide either (x, y) or a 2-column matrix/data.frame")
    }
  }
## input 
  method <- match.arg(method)
      df <- data.frame(x = x, y = y)
       p <- ggplot2::ggplot(df, ggplot2::aes(x=x, y=y)) +
            ggplot2::geom_point(alpha = 0.5, color=point.colour, size=point.size) 
            ggplot2::theme_minimal()  
## Optional density contours
if (density) 
   {
       p <- p +  ggplot2::stat_density_2d(color = density.colour )
}
 ## ---- Median methods ----      
      mm <- xy_median(x,y, method=method)
       p <- p + ggplot2::geom_point(x = mm[1], y = mm[2], color = median.colour,
                   size = median.size)
# ---- 50% HDR (median region) ----
if (HDR) {
         kde <- MASS::kde2d(x, y, n = 100)
## Convert to data frame
          dx <- expand.grid(x = kde$x, y = kde$y)
        dx$z <- as.vector(kde$z)
## Find density threshold for top 50% mass
         ord <- order(dx$z, decreasing = TRUE)
     cumprob <- cumsum(dx$z[ord]) / sum(dx$z)
 level_index <- which(cumprob >= 0.5)[1]
         z50 <- dx$z[ord][level_index]
## Add contour corresponding to 50% HDR
         p <- p +
           ggplot2::geom_contour(data = dx,
           ggplot2::aes(x = x, y = y, z = z),
                        breaks = z50,
                        color = HDR.colour,
                        linewidth = 1.2)+
           ggplot2::theme_minimal()  
       }
       return(p)       
}
################################################################################
################################################################################
################################################################################
################################################################################
# function 6
xy_pit_scatter <- function(x, y = NULL,
                                    method = c("cw", "pc", "sp"), 
                                    density = TRUE,
                                        HDR = TRUE,
                             density.colour = "lightblue",
                              median.colour = "red", 
                                median.size = 3, 
                                 point.size = 1,
                               point.colour = "grey",
                                 HDR.colour = "darkred")
{
   z <- NULL
  if (is.null(y)) 
  {
    if (is.matrix(x) || is.data.frame(x)) 
    {
      y <- x[, 2]
      x <- x[, 1]
    } else 
    {
      stop("Provide either (x, y) or a 2-column matrix/data.frame")
    }
  }
## input 
       n <- length(x)  
  method <- match.arg(method)
       u <- rank(x) / (n + 1)
       v <- rank(y) / (n + 1)
      df <- data.frame(u = u, v = v)
       p <- ggplot2::ggplot(df, ggplot2::aes(x=v, y=u)) +
            ggplot2::geom_point(alpha = 0.5, size = point.size, colour=point.colour) +
            ggplot2::theme_minimal()  
## Optional density contours
  if (density) 
  {
    p <- p + ggplot2::stat_density_2d(color =  density.colour)
  }
  ## ---- Median methods ----      
   mm <- xy_median(v,u, method=method)
    p <- p + ggplot2::geom_point(x = mm[1], y = mm[2], 
                      color =  median.colour,
                      size = median.size)
# ---- 50% HDR (median region) ----
  if (HDR) {
     kde <- MASS::kde2d(v, u, n = 100)
## Convert to data frame
      dx <- expand.grid(x = kde$x, y = kde$y)
    dx$z <- as.vector(kde$z)
## Find density threshold for top 50% mass
    ord <- order(dx$z, decreasing = TRUE)
cumprob <- cumsum(dx$z[ord]) / sum(dx$z)
level_index <- which(cumprob >= 0.5)[1]
    z50 <- dx$z[ord][level_index]
## Add contour corresponding to 50% HDR
    p <- p +
       ggplot2::geom_contour(data = dx,
            ggplot2::aes(x = x, y = y, z = z),
            breaks = z50,
            color = HDR.colour,
            linewidth = 1.2)
  }
  return(p)       
}
################################################################################
################################################################################
################################################################################
################################################################################
# function 7 a
# Empirical CDF function
ECDF_2D <- function(value.x, value.y, data.x, data.y) {
  mean(data.x <= value.x & data.y <= value.y)
}
################################################################################
################################################################################
################################################################################
################################################################################
# function 8
xy_hist <- function(x,y, 
                          colour = hcl.colors(20),
                          points = TRUE,
                    point.colour = "gray",
                       point.size = 0.1,
                         bins = 50  
)
{
  level <- NULL
if (is.null(y)) # data handiling
  {
    if (is.matrix(x) || is.data.frame(x)) 
    {
      y <- x[, 2]
      x <- x[, 1]
    } else 
    {
      stop("Provide either (x, y) or a 2-column matrix/data.frame")
    }
  }  
  df <- data.frame(x=x,y=y)
   p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +
        ggplot2::geom_bin2d(bins = bins) +
        ggplot2::scale_fill_gradient(low = "white", high = "red") +
        ggplot2::labs(title = "2D Histogram (Empirical Density)",
         fill = "Count") 
  if (points)  
    p=p+ ggplot2::geom_point(ggplot2::aes(x = x, y = y), 
                   color =  point.colour,
                   size = point.size)
  p
}
################################################################################
################################################################################
################################################################################
################################################################################
# function 9
xy_pit_hist <- function(x,y, 
                      colour = hcl.colors(20),
                      points = TRUE,
                point.colour = "gray",
                  point.size = 0.1,
                       bins = 50  
)
{
  level <- NULL
  if (is.null(y)) # data handiling
  {
    if (is.matrix(x) || is.data.frame(x)) 
    {
      y <- x[, 2]
      x <- x[, 1]
    } else 
    {
      stop("Provide either (x, y) or a 2-column matrix/data.frame")
    }
  }  
     n <- length(x)  
     u <- rank(x) / (n + 1)
     v <- rank(y) / (n + 1)
    df <- data.frame(u = u, v = v)
     p <- ggplot2::ggplot(df, ggplot2::aes(u, v)) +
    ggplot2::geom_bin2d(bins = bins) +
    ggplot2::scale_fill_gradient(low = "white", high = "red") +
    ggplot2::labs(title = "2D PIT Histogram (Empirical Density)",
                  fill = "Count") 
  if (points)  
    p=p+ ggplot2::geom_point(ggplot2::aes(x = u, y = v), 
                             color =  point.colour,
                             size = point.size)
  p
}
################################################################################
################################################################################
################################################################################
################################################################################
# function 10
 xy_density <- function(x,y, 
                            colour = hcl.colors(20),
                            points = FALSE,
                      point.colour = "black",
                        point.size = 0.1
)
{
   level <- NULL
  if (is.null(y)) # data handiling
  {
    if (is.matrix(x) || is.data.frame(x)) 
    {
      y <- x[, 2]
      x <- x[, 1]
    } else 
    {
      stop("Provide either (x, y) or a 2-column matrix/data.frame")
    }
  }  
  df <- data.frame(x=x,y=y)
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +
       ggplot2::stat_density_2d(ggplot2::aes(fill = ggplot2::after_stat(level)),
                    geom = "polygon",
                    contour = TRUE) +
      ggplot2::scale_fill_viridis_c() +
      ggplot2::labs(title = "Bivariate Kernel Density Estimate",
         fill = "Density") 
  if (points) 
  {
    p = p + ggplot2::geom_point(ggplot2::aes(x = x, y = y), 
                                color =  point.colour,
                       size = point.size, alpha=0.5)
  }
  p +  ggplot2::theme_minimal()   
  return(p)      
}
################################################################################
################################################################################
################################################################################
################################################################################
# function 11
xy_density_plain <- function(x,y, 
                    colour = hcl.colors(20),
                    points = FALSE,
              point.colour = "black",
                point.size = 0.1
)
{
  if (is.null(y)) # data handiling
  {
    if (is.matrix(x) || is.data.frame(x)) 
    {
      y <- x[, 2]
      x <- x[, 1]
    } else 
    {
      stop("Provide either (x, y) or a 2-column matrix/data.frame")
    }
  }  
  df <- data.frame(x=x,y=y)
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +
       ggplot2::stat_density_2d(color = "blue") +
       ggplot2::labs(title = "KDE Contours") 
if (points) 
    {
      p = p + ggplot2::geom_point(ggplot2::aes(x = x, y = y), color =  
                        point.colour,
                        size = point.size, alpha=0.5)
    }
  p +  ggplot2::theme_minimal()   
  return(p)      
}
################################################################################
################################################################################
################################################################################
################################################################################
# function 12
# Using MASS Kernel Densiy  2 dimension 
xy_hist_kde2d <- function(x,y, 
                      colour = hcl.colors(20),
                     points = TRUE,
               point.colour = "gray",
                 point.size = 0.1,
                       bins = 50 
)
{
    z <- NULL
    if (is.null(y)) # data handiling
    {
      if (is.matrix(x) || is.data.frame(x)) 
      {
        y <- x[, 2]
        x <- x[, 1]
      } else 
      {
        stop("Provide either (x, y) or a 2-column matrix/data.frame")
      }
    }  
       kde <- MASS::kde2d(x, y, n = bins)
    kde_df <- expand.grid(x = kde$x, y = kde$y)
  kde_df$z <- as.vector(kde$z)  
         p <- ggplot2::ggplot(kde_df, ggplot2::aes(x, y, fill = z)) +
              ggplot2::geom_tile() +
              ggplot2::scale_fill_viridis_c() +
              ggplot2::labs(title = "KDE from MASS::kde2d",
           fill = "Density") 
# if (points)
#    {
#            p = p + geom_point(aes(x = x, y = y), color =  point.colour,
#                               size = point.size, alpha=0.5)
#    }
         p +  ggplot2::theme_minimal()   
         return(p)        

}
################################################################################
################################################################################
################################################################################
################################################################################
# function 13
xy_ECDF_hist <- function(x,y) 
                        #  points = TRUE,
                        #  point.colour = "gray",
                        #  point.size = 0.1
                        # )
{
       Fn <- NULL
       xg <- seq(min(x), max(x), length = 40)
       yg <- seq(min(y), max(y), length = 40)
     grid <- expand.grid(x = xg, y = yg)
  grid$Fn <- mapply(ECDF_2D, grid$x, grid$y,
                    MoreArgs = list( data.x= x, data.y = y))
        p <-  ggplot2::ggplot(grid, ggplot2::aes(x, y, fill = Fn)) +
              ggplot2::geom_tile() +
              ggplot2::scale_fill_viridis_c() +
              ggplot2::labs(title = "Empirical Bivariate CDF",
              fill = "F(x,y)") +
              ggplot2::theme_minimal()  
 #  if (points)  
  #   df <- data.frame(x=x, y=y)
  #   p=p + geom_point(data=df, ggplot2::aes(x = x, y = y), color =  point.colour,
  #                 size = point.siz)
 return(p)
}

################################################################################
################################################################################
################################################################################
################################################################################
# function 14
xy_ECDF_contour <- function(x,y)  
                 #       points = TRUE,
                 # point.colour = "gray",
                 #    point.size = 0.1)
{
    Fn <- NULL
    xg <- seq(min(x), max(x), length = 40)
    yg <- seq(min(y), max(y), length = 40)
  grid <- expand.grid(x = xg, y = yg)
  grid$Fn <- mapply(ECDF_2D, grid$x, grid$y,
                    MoreArgs = list( data.x= x, data.y = y))
  
  #Heatmap of the empirical CDF
  P <-  ggplot2::ggplot(grid, ggplot2::aes(x, y, z = Fn)) +
        ggplot2::geom_contour_filled() +
        ggplot2::labs(title = "Empirical CDF (Contours)",
         fill = "F(x,y)") +
        ggplot2::theme_minimal()
 return(P)
}
################################################################################
################################################################################
################################################################################
################################################################################
# function 15
xy_pit_density <- function(x, y = NULL,
                         points = FALSE,
                     point.size = 1,
                    point.colour = "grey")
{
  level <- NULL
  if (is.null(y)) 
  {
    if (is.matrix(x) || is.data.frame(x)) 
    {
      y <- x[, 2]
      x <- x[, 1]
    } else 
    {
      stop("Provide either (x, y) or a 2-column matrix/data.frame")
    }
  }
  ## input 
       n <- length(x)  
       u <- rank(x) / (n + 1)
       v <- rank(y) / (n + 1)
      df <- data.frame(u = u, v = v)
       p <- ggplot2::ggplot(df, ggplot2::aes(x=u, y=v)) +
            ggplot2::stat_density_2d(ggplot2::aes(fill = ggplot2::after_stat(level)),
                    geom = "polygon") +
            ggplot2::scale_fill_viridis_c() +
            ggplot2::labs(title = "Smoothed Copula Density",
             fill = "density") 
if (points) 
{
  p = p + ggplot2::geom_point(ggplot2::aes(x = v, y = u), color =  point.colour,
                    size = point.size, alpha=0.5)
}
  p +  ggplot2::theme_minimal()   
  return(p)       
}
################################################################################
################################################################################
################################################################################
################################################################################
# function 16
bivar_pdf  <-  function(pdf,
                        xlim = c(-3, 3),
                        ylim = c(-3, 3),
                           n = 200, 
                        xlab = "x",
                        ylab = "y", 
                        HDV = TRUE,
                        ...) 
{
## Grid
      x <- seq(xlim[1], xlim[2], length.out = n)
      y <- seq(ylim[1], ylim[2], length.out = n)
   grid <- expand.grid(x = x, y = y)
## Evaluate density
 z <- NULL
 grid$z <- mapply(pdf, grid$x, grid$y, ...)
## Approximate probability mass
  dx <- diff(x)[1]
  dy <- diff(y)[1]
## Order by density (HDR idea)
  ord <- order(grid$z, decreasing = TRUE)
  cumprob <- cumsum(grid$z[ord]) * dx * dy
  cumprob <- cumprob / max(cumprob)
## Find threshold for 50%
  idx <- which(cumprob >= 0.5)[1]
  z50 <- grid$z[ord][idx]
## Plot
    ggplot2::ggplot(grid, ggplot2::aes(x, y, z = z)) +
    ggplot2::geom_contour_filled(alpha = 0.8) +
    ggplot2::geom_contour(breaks = z50,
                 color = "red",
                 linewidth = 1.2) +
    ggplot2::labs(title = "50% Probability Region (Median Region)",
                 subtitle = "Red contour = HDR 50%") +
    ggplot2::theme_minimal()
}
################################################################################
################################################################################
################################################################################
################################################################################
# function 17
# the function from chatGPD
# the f() dunction should be a bivariate function
bivar_fun <- function(fun,
                       xlim = c(-3,3),
                       ylim = c(-3,3),
                          n = 100,
                       xlab = "x",
                       ylab = "y",
                       ...)
{
        z <- numeric()
        x <- seq(xlim[1], xlim[2], length.out = n)
        y <- seq(ylim[1], ylim[2], length.out = n)
     grid <- expand.grid(x = x, y = y)
    grid$z <- fun(grid$x, grid$y, ...)
    ggplot2::ggplot(grid, ggplot2::aes(x, y, z = z)) +
    ggplot2::geom_contour_filled() +
    ggplot2::labs(x = "x", y = "y", fill = "Density") +
    ggplot2::theme_minimal()
}
################################################################################
################################################################################
################################################################################
################################################################################
