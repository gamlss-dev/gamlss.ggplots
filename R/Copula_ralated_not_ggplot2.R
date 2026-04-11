################################################################################
################################################################################
################################################################################
################################################################################
# function 1
# Empirical CDF function
# ECDF_2D <- function(value.x, value.y, data.x, data.y) {
#   mean(data.x <= value.x & data.y <= value.y)
# }
################################################################################
################################################################################
################################################################################
################################################################################
# function 1: NOT-GGPLOTS
#plot the empirical CDF surface
xy.ECDF.persp <- function(x,y, 
                          theta = 30, 
                            phi = 30,
                           xlab = "x", 
                           ylab = "y", 
                           zlab = "f(x,y)",
                         colour = "lightblue",
                           main = "ECDF(x,y)")
{
  xg <- seq(min(x), max(x), length = 40)
  yg <- seq(min(y), max(y), length = 40)
  z <- outer(xg, yg, Vectorize(function(a, b) ECDF_2D(a, b, x, y)))  
  persp(xg, yg, z,
        theta = theta, phi = phi,
        col = colour,
        xlab = "x", ylab = "y", zlab = "f(x,y)",
        main = "Bivariate CDF")
}
################################################################################
################################################################################
################################################################################
################################################################################
## function 2:NOT GGPLOTS 
xy.ECDF.image <- function(x,y, 
                          colour = hcl.colors(20),
                            main = "Empirical CDF Heatmap",
                         contour = TRUE,
                          points = TRUE,
                    point.colour = "gray",
                       point.size = 0.1
                          )
{
   z <- NULL
  xg <- seq(min(x), max(x), length = 40)
  yg <- seq(min(y), max(y), length = 40)
   z <- outer(xg, yg, Vectorize(function(a, b) ECDF_2D(a, b, x, y)))  
  image(xg, yg, z,
        col = colour,
        xlab = "x", ylab = "y",
        main = main
        )
if (contour)  contour(xg, yg, z, add = TRUE)
if (points)  points(x,y, col=point.colour, cex=point.size)
}
################################################################################
################################################################################
################################################################################
################################################################################
# function 3 :not GGPLOTS
xy.ECDF.contour <- function(x,y, 
                      colour = hcl.colors(20),
                        main = "Empirical CDF Heatmap",
                      points = TRUE,
                point.colour = "gray",
                  point.size = 0.1
)
{
  rz <- z <- NULL
  xg <- seq(min(x), max(x), length = 40)
  yg <- seq(min(y), max(y), length = 40)
   z <- outer(xg, yg, Vectorize(function(a, b) ECDF_2D(a, b, x, y)))  
  contour(xg, yg, z)
  if (points)  points(x,  y, col=point.colour, cex=point.size)
}
################################################################################
################################################################################
################################################################################
################################################################################
# function 4
xy.density.contour <- function(x,y, 
                             colour = hcl.colors(20),
                               main = "Empirical CDF Heatmap",
                             points = TRUE,
                       point.colour = "gray",
                         point.size = 0.1,
                               bins = 50
)
{
  if (is.null(y)) # data handling
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
    contour(kde$x, kde$y, kde$z)
  if (points)  points(x,y, col=point.colour, cex=point.size)
}
################################################################################
################################################################################
################################################################################
################################################################################
# figure 5
xy.density.image <- function(x,y, 
                           colour = hcl.colors(20),
                             main = "Empirical CDF Heatmap",
                         countour = TRUE,
                           points = TRUE,
                     point.colour = "gray",
                        point.cex = 0.1,
                             bins = 50
)
{
  if (is.null(y)) # data handling
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
  image(kde$x, kde$y, kde$z,
        col = colour,
        xlab = "x", ylab = "y",
        main = main
  )
  if (countour)  contour(kde$x, kde$y, kde$z, add = TRUE)
  if (points)   points(x,y, col=point.colour, cex=point.cex)
}
################################################################################
################################################################################
################################################################################
################################################################################
# figure 6
xy.density.image <- function(x,y, bins=25)
{
  if (is.null(y)) # data handling
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
  persp(x=kde$x, y=kde$y, z=kde$z,
        theta = 30, phi = 30,
        col = "lightblue",
        xlab = "x", ylab = "y", zlab = "F_n(x,y)",
        main = "Empirical Bivariate CDF")
}
################################################################################
################################################################################
################################################################################
################################################################################
# function 8
xy_histogram <- function(x,y, 
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
bivar.fun.contour <- function(f,
                              xlim = c(-3,3),
                              ylim = c(-3,3),
                                 n = 100,
                              xlab = "x",
                              ylab = "y",
                              ...)
{
      gx <- seq(xlim[1], xlim[2], length.out = n)
      gy <- seq(ylim[1], ylim[2], length.out = n)
   grid <- expand.grid(x = gx, y = gy) 
      z <- f(grid$x, grid$y, ...)
      Z <- matrix(z, nrow=n, ncol=n )
 contour(gx, gy, Z, xlab=xlab, ylab=ylab)
}
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# function 10
bivar.fun.image <- function(f,
                            xlim = c(-3,3),
                            ylim = c(-3,3),
                            n = 100,
                            xlab = "x",
                            ylab = "y",
                            main = "f()",
                          colour = hcl.colors(20),
                        countour = TRUE,
                            ...)
{
    gx <- seq(xlim[1], xlim[2], length.out = n)
    gy <- seq(ylim[1], ylim[2], length.out = n)
  grid <- expand.grid(x = gx, y = gy) 
     z <- f(grid$x, grid$y, ...)
     Z <- matrix(z, nrow=n, ncol=n )
     image(gx, gy, Z,
           col = colour,
           xlab = xlab, ylab = ylab,
           main = main
     )
     if (countour)  contour(gx, gy, Z, add = TRUE)
}
################################################################################
################################################################################
################################################################################
################################################################################
# function 11
bivar.fun.persp  <- function(f,
                          xlim = c(-3,3),
                          ylim = c(-3,3),
                             n = 100,
                        colour = "lightblue",
                         theta = 30, 
                           phi = 30,
                          xlab = "x", 
                          ylab = "y", 
                          zlab = "f(x,y)",
                        main = "f(x,y)",
                            ...)
{
  gx <- seq(xlim[1], xlim[2], length.out = n)
  gy <- seq(ylim[1], ylim[2], length.out = n)
  grid <- expand.grid(x = gx, y = gy) 
  z <- f(grid$x, grid$y, ...)
  Z <- matrix(z, nrow=n, ncol=n )
  persp(gx, gy, Z,
         col = colour,
        xlab = xlab, 
        ylab = ylab,
        zlab = "f(x,y)",
        main = "f(x,y)",
       theta = theta, 
         phi = phi)
}
################################################################################
################################################################################
################################################################################
################################################################################
