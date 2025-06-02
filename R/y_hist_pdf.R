y_hist_pdf <- function(y,
                   data,
                   xlab = NULL,
            with.density = TRUE,
                hist.col = "black", 
               hist.fill = "white",
               dens.fill = "#FF6666",
                binwidth = (max(y)-min(y))/20,
                   from, to, title,
                      mu = NULL,
                   sigma = NULL,
                      nu = NULL,
                     tau = NULL,
              nno.points = 201, 
                   alpha = 0.8,
         #       col.fill = hcl.colors(lobs, palette="viridis"),# see hcl.pals()
            size.seqment = 1.5, # for discrete dist
              plot.point = TRUE,#    ''
              size.point = 1,   #    ''
               plot.line = TRUE,#    ''
               size.line = 0.2)

{
################################################################################
  other_prep_data <- function (y) 
  {
    obs <- seq_len(length(y))
    obs <- obs[!is.na(y)]
    y <- y[!is.na(y)]
    out <- data.frame(obs = obs, y = as.numeric(y))
    return(out)
  }  
################################################################################  
  xlab <- if (is.null(xlab)) deparse(substitute(y))
  else xlab
if (missing(y))  stop("the y is not declared")
if (!missing(data)) y <- get(xlab, envir = as.environment(data))
xlimitfrom <- if (missing(from))   min(y) else from
  xlimitto <- if (missing(to))     max(y) else to  
         d <- other_prep_data(y)  
txt.title <- if (missing(title))   paste("Histogram and density plot of",xlab)
         else title         
   gg <- ggplot2::ggplot(d, ggplot2::aes(x=y))+
     ggplot2::geom_histogram(ggplot2::aes(y=ggplot2::after_stat(density)), 
                             binwidth = binwidth, 
                             colour=hist.col, fill=hist.fill)+
     ggplot2::xlim(xlimitfrom, xlimitto)+ 
     ggplot2::stat_function(fun = dLOGSHASH, args = list(mu = mu, sigma = sigma, nu=nu, tau=tau), 
                            color = "blue", size = 1.2) +
     ggplot2::labs( x = "Value", y = "Density") 
   # ggplot2::ggplot(data.frame(y=y), ggplot2::aes(x=y))+
   #   ggplot2::geom_histogram(ggplot2::aes(y=ggplot2::after_stat(density)),
   #                           binwidth = binwidth,
   #                           colour=hist.col, fill=hist.fill) #+
  #   ggplot2::xlab(xlab) + 
  #   ggplot2::xlim(xlimitfrom, xlimitto)+      
  #   ggplot2::ylab("histogram")# + 
  #   ggplot2::geom_segment(mapping =  ggplot2::aes(x=y.var, y=pdfArr, 
  #                                               xend = y.var, yend = 0), 
  #                       color=col.fill[1],  size=size.seqment)
  #  ggplot2::ggtitle(txt.title)   
  # gg <- ggplot2::ggplot(data.frame(x=y), ggplot2::aes(x)) +
  #   ggplot2::geom_histogram(aes(y = ..density..), bins = 30, fill = "white", color = hist.col) +
  #   ggplot2::stat_function(fun = dLOGSHASH, args = list(mu = mu, sigma = sigma, nu=nu, tau=tau), 
  #                 color = "blue", size = 1.2) +
  #   labs(title = "(a)", x = "Value", y = "Density")
  
  # ggplot(data.frame(x=y), aes(x)) +
  #   geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  #   stat_function(fun = dSHASH, args = list(mean = mu, sigma = sigma, nu=nu, tau=tau), 
  #                 color = "red", size = 1.2) +
  #   labs(title = "Histogram with Normal Distribution Fit", x = "Value", y = "Density")
  # ggplot(data.frame(x = c(0, 300)), aes(x)) +
  #   stat_function(fun = funtion(x){1000*dLOGSHASH(x)}, args = list(mu = mu, sigma = sigma, nu=nu, tau=tau),
  #                 color = "blue", size = 1.2) +
  #   labs(title = "Normal Distribution Function (CDF)",
  #        x = "x", y = "F(x)") +
  #   theme_minimal()
  
  gg
}

