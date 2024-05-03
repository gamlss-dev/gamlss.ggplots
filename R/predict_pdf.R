################################################################
################################################################
################################################################
################################################################
#   fitted_pdf()  
#   Mikis Stasinopoulos Bob Rigby Fernanda de Bastiani
#   23 February, 2021 
#   TO DO : I have not checked binomial
#           what about legend? 
################################################################
################################################################
predict_pdf <- function (model,
                      newdata,
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
  if (missing(newdata)) stop("the argument newdata is required")
## the number of plots  
## whether binomial type
################################################################################
################################################################################
if (fname%in%gamlss.bi.list)  
{
          MM <- if (is.gamlss(model)) {predictAll(model, newdata=newdata, output="data.frame")}
                else                   {predict(model, type="parameter") }
          bd <- MM[,"bd"]
        lobs <- dim(newdata)[1] 
  lastcolMM <- dim(MM)[2]
         to <- max(bd)
     y.var  <- pdfArr <- da <- list()  
 y.var..i.. <- pdfArr..i.. <- NULL 
for (i in 1:lobs)
{
    y.var[[i]] <- 0:MM[i,"bd"]
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
     p11 <- ggplot(data=da0) 
if (lobs==1) 
    {
         p11 <- p11 +  #geom_hline( aes(yintercept = 0)) +
           geom_segment(data=da[[1]], mapping = aes(x=y.var..i.., y=pdfArr..i.., 
                                      xend = y.var..i.., yend = 0), 
                        color=col.fill[1],  size=size.seqment)
    }
      else   
    {  
for (i in 1:lobs)
         {
           p11 <- p11 + # geom_hline( aes(yintercept = 0)) +
             geom_segment(data=da[[i]], 
              mapping =  aes(x=y.var..i.., y=pdfArr..i.., 
                                       xend = y.var..i.., yend = 0), 
                          color=col.fill[i], alpha=alpha, size=size.seqment)
  if (plot.point) p11 <- p11+geom_point(data=da[[i]],
                          aes(x=y.var..i.., y=pdfArr..i..), size= size.point, color=col.fill[i])
  if (plot.line)  p11 <- p11 + geom_line(data=da[[i]],
                          aes(x=y.var..i.., y=pdfArr..i..),  
                          size= size.line, color=col.fill[i])
         } 
    }
     p11 = p11 + labs(x = "y", y =  paste0(fname,"(y)"))+
       xlim(from,to)+
       ggtitle( txt.title)     
return(p11)     
} # end binomial
################################################################################      
################################################################################
     MM <- if (is.gamlss(model)) {predictAll(model, newdata=newdata, output="data.frame")}
           else                  {predict(model, newdata=newdata, type="parameter")}
    lobs <- dim(newdata)[1]
#  if (lobs==1) MM <- matrix(MM, nrow=1, ncol=nopar+1 ) 
# everything else not binomial type
##    whether discrete distribution or not
    y.var <- if ((type=="Discrete")||(type=="discrete"))  
                 seq(from, to, by=1)
             else seq(from, to, length=no.points)
  #if(any(fname%in%.gamlss.bi.list)) bd <- to   
#if (lobs==1) MM <- matrix(MM, nrow=1, ncol=nopar+1 )  
# the matrix to hold the results
     pdfArr <- matrix(0, nrow=length(y.var), ncol=lobs)
# loop over observations

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
          pdfArr[,j] <-  p_d_f(y.var,  par=MM[j,])  
        }  
}  # end of look over observations  
################################################################ 
     da <- data.frame(y.var,  pdfArr)
    p11 <- ggplot2::ggplot(data=da) 
if (type=="Discrete")
{
  if (lobs==1) 
  {
    p11 <- p11 +  
      ggplot2:::geom_segment(mapping = ggplot2::aes(x=y.var, y=pdfArr, xend = y.var, 
                yend = 0), color=col.fill[1],  size=size.seqment)
  }
  else   
  {  
    for (i in 1:lobs)
    {
      p11 <- p11 + 
        ggplot2::geom_segment(mapping =  
        ggplot2::aes(x=.data[["y.var"]],  y=.data[[paste0("X",i)]],
                                xend =.data[["y.var"]],, yend = 0), 
                              color=col.fill[i], alpha=alpha, size=size.seqment)
      
      if (plot.point) p11 <- p11+ ggplot2::geom_point( 
        ggplot2::aes(x=.data[["y.var"]],  y=.data[[paste0("X",i)]]),  
                     size= size.point, color=col.fill[i])
      if (plot.line)  p11 <- p11 + ggplot2::geom_line( 
        ggplot2::aes(x=.data[["y.var"]], y=.data[[paste0("X",i)]]),  
                     size= size.line, color=col.fill[i])
    } 
  }
} else # continuous 
{# one plot 
  if (lobs==1) p11 = p11 +geom_area(fill=col.fill[1], alpha=alpha, aes(x=y.var, y=pdfArr))
  else
  {# more than one plot
    for (i in 1:lobs)
    {
  p11 <-p11 + geom_area(fill=col.fill[i], alpha=alpha, aes_string(x="y.var", y=paste0("X",i)))
    } 
  }
}  
  p11 = p11 + labs(x = "y", y =  paste0(fname,"(y)"))+
              xlim(from,to)+
              ggtitle( txt.title)
p11
}

