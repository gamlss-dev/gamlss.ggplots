################################################################################
################################################################################
################################################################################
################################################################################
#   fitted_pdf()  
#   Mikis Stasinopoulos Bob Rigby Fernanda de Bastiani
#   28 September, 2022 
#   TO DO : I have not checked binomial
#           what about legend? 
################################################################################
################################################################################
################################################################################
################################################################################
fitted_pdf_legend <- function (model,
                        obs,
                        title,
                        from = 0,
                        to = 10,
                        no.points = 201, 
                        alpha = 0.4,
                        col.fill = hcl.colors(lobs, palette="viridis"),# see hcl.pals()"Set 2"
                        size.seqment = 1.5, # for discrete dist
                        plot.point = TRUE,#    ''
                        size.point = 1,   #    ''
                        plot.line = TRUE,#    ''
                        size.line = 0.2, #    ''
                        ...)
{
 gamlss.bi.list <- .binom  
if (!(is.gamlss(model)||is(model,"gamlss2"))) stop("the model should be an gamlss object") 
            obj <- get_family(model)
          fname <- obj$fname  
           type <- obj$type
          nopar <- obj$nopar
          param <- obj$param
           dfun <- obj$dfun
          p_d_f <- obj$p_d_f
      par.names <- param
      txt.title <- if (missing(title))  
                   paste("Fitted pdf's from model",deparse(substitute(model)))
                   else title  
             MM <- if (is.gamlss(model)) {predictAll(model, output="data.frame")[obs,]}
                   else                  {predict(model, type="parameter")[obs,] }
           lobs <- length(obs)
  ## whether binomial type
################################################################################          
  if (fname%in%gamlss.bi.list)  {
    stop("this function is not working at the moment with bonomial errors")
            bd <- model$bd[obs]
            MM <- cbind(MM, bd)
     lastcolMM <- dim(MM)[2]
            to <- max(bd)
        y.var  <-  pdfArr <- da <- list()  
    y.var..i.. <- pdfArr..i.. <- NULL 
  for (i in 1:lobs)
    {
      y.var[[i]] <- 0:MM[i,lastcolMM]
    switch(nopar,
             {
               pdfArr[[i]] <- p_d_f(y.var[[i]],  mu=MM[i,"mu"],  bd=MM[i,"bd"])
             },  
             {
  pdfArr[[i]] <- p_d_f(y.var[[i]], mu=MM[i,"mu"],  sigma=MM[i,"sigma"], bd=MM[i,"bd"])
             },
             {
  pdfArr[[i]] <- p_d_f(y.var[[i]],  mu=MM[i,"mu"], sigma=MM[i,"sigma"], nu=MM[i,"nu"], bd=MM[i,"bd"])
             },
             {
  pdfArr[[i]] <- p_d_f(y.var[[i]],  mu=MM[j,"mu"], sigma=MM[i,"sigma"], nu=MM[i,"nu"], tau=MM[i,"tau"],  bd=MM[i,"bd"])  
             })  
      da[[i]] <- data.frame(y.var[[i]],  pdfArr[[i]])
    }
    da0 <- data.frame(y.var=from:to)
    p11 <- ggplot2::ggplot(data=da0) 
  if (lobs==1) 
    {
      p11 <- p11 + 
        ggplot2::geom_segment(data=da[[1]], mapping = 
                                ggplot2::aes(x=y.var..i.., y=pdfArr..i.., 
                                             xend = y.var..i.., yend = 0), 
                              color=col.fill[1],  size=size.seqment)
    }
    else   
    {  
      for (i in 1:lobs)
      {
        p11 <- p11 + # geom_hline( aes(yintercept = 0)) +
          ggplot2::geom_segment(data=da[[i]], 
                                mapping =  ggplot2::aes(x=y.var..i.., y=pdfArr..i.., 
                                                        xend = y.var..i.., yend = 0), 
                                color=col.fill[i], alpha=alpha, size=size.seqment)
        if (plot.point) p11 <- p11+ 
            ggplot2::geom_point(data=da[[i]],
                                ggplot2::aes(x=y.var..i.., y=pdfArr..i..), size= size.point, color=col.fill[i])
        if (plot.line)  p11 <- p11 + ggplot2::geom_line(data=da[[i]],
                                                        ggplot2::aes(x=y.var..i.., y=pdfArr..i..),  
                                                        size= size.line, color=col.fill[i])
      } 
    }
    p11 = p11 + ggplot2::labs(x = "y", y =  paste0(fname,"(y)"))+
      ggplot2::xlim(from,to)+
      ggplot2::ggtitle( txt.title)     
    return(p11)     
  } # end binomial
################################################################################
# everything else not binomial type
##    whether discrete distribution or not
  y.var <- if ((type=="Discrete")||(type=="discrete"))  
               seq(from, to, by=1)
          else seq(from, to, length=no.points)
  pdfArr <- matrix(0, nrow=length(y.var), ncol=lobs)
  if (is(model,"gamlss"))
  {
    for (j in 1:lobs)
    {
      switch(nopar,
             {
    pdfArr[,j] <- p_d_f(y.var,  mu=MM[j,"mu"])
             },  
             {
    pdfArr[,j] <- p_d_f(y.var, mu=MM[j,"mu"],  sigma=MM[j,"sigma"])
             },
             {
   pdfArr[,j] <- p_d_f(y.var,  mu=MM[j,"mu"], sigma=MM[j,"sigma"], nu=MM[j,"nu"])
             },
             {
   pdfArr[,j] <- p_d_f(y.var,  mu=MM[j,"mu"], sigma=MM[j,"sigma"], nu=MM[j,"nu"], tau=MM[j,"tau"])  
             })  
    }  
  } else 
  {
    for (j in 1:lobs)
    {
      pdfArr[,j] <-  p_d_f(y.var,  par=predict(model, type="parameter")[obs[j],])  
    }  
  }  # end of look over observations
  ################################################################ 
  #da <- data.frame(y.var,  pdfArr
  dat <- data.frame(pdfArr = as.vector(pdfArr), y.var=y.var, 
            obs= gl(n=lobs, k=length(y.var), 
                   labels= as.character(obs)))
  p11 <- ggplot2::ggplot(data=dat, ggplot2::aes(x=y.var, y=pdfArr, 
                                               fill=obs, color=obs)) 
if ((type=="Discrete")||(type=="discrete"))
  {
        p11 <- p11 + geom_segment( xend =dat$y.var, yend = 0, arrow.fill=dat$obs, 
                                   alpha=alpha, size=size.seqment)+
                     ggplot2::labs(x = "y", y =  paste0(fname,"(y)"))+
                     ggplot2::xlim(from,to)+
                     ggplot2::ggtitle( txt.title)
  } else # continuous 
  { # more than one plot
        p11 <-p11 + ggplot2::geom_area(alpha=alpha, show.legend = TRUE)+
                    ggplot2::labs(x = "y", y =  paste0(fname,"(y)"))+
                    ggplot2::xlim(from,to)+
                    ggplot2::ggtitle( txt.title)
  } 
  p11
}
################################################################################
################################################################################
################################################################################
################################################################################
# adding data in 
# fitted_pdf_data <- function(model, obs, from, to, ...)  
# {
#   
#   fitted_pdf(model, obs=obs, from=from, to=to, ...)+
#     ggplot2::geom_vline(xintercept = model$y[obs], colour="gray")
# }
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
