################################################################################
################################################################################
################################################################################
################################################################################
#  function equivalent to TD.scaled 
################################################################################
################################################################################
################################################################################
################################################################################ 
model_TD<- function(..., 
                 newdata,
                     plot = TRUE,   # whether plot or print
             text.to.show = NULL,   # whether the labels should be different
                      col = "rosybrown",  # colour for the bars
                  diff.TD = 1000,
                    width = 0.9,  # widrh of the bar 
                    horiz = TRUE,
                    scale = c("[0,1]","[min,max]"),
                        title)   
{
#-------------------------------------------------------------------------------
model_TD_call <- match.call()  #   the function call
        scale <- match.arg(scale)
# type <-  match.arg(type)
        delta <- scaled <- NULL  
       models <- list(...)
     isgamlss <- unlist(lapply(models, inherits, what=c("gamlss","gamlss2")))
if (!any(isgamlss)) stop("some of the objects are not gamlss")
if (length(models)==1) stop("The function needs more than one model")
if (missing(newdata)) 
    stop("The newdata option is needed (you can use the training set)")  
         nN <- dim(newdata)[1] 
          K <- length(models)                   ## how many models
         TD <- rep_len(0, length.out=K)
         MN <- rep_len("",length.out=K )
for (i in 1:K)
      {   
    MN[i] <- paste0(match.call()[[1+i]]) 
   cModel <- class(models[[i]])[1]
    TD[i] <- if (cModel=="gamlss") getTGD(models[[i]], newdata=newdata)$TGD
          else                 -2*logLik(models[[i]], newdata=newdata)
} 
     wmin <- which.min(TD)
      DTD <- TD-TD[wmin]# unfortunately the range can atrocious because of very large AIC's 
       TD <- TD[DTD < diff.TD]
     dAIC <- max(TD, na.rm = TRUE)-TD #(AIC-AIC[which.min(AIC)]) 
     oAIC <- dAIC/(max(TD, na.rm = TRUE)-min(TD,  na.rm = TRUE))
      val <- data.frame(cbind(TD, delta=round(dAIC,3), scaled=round(oAIC,4)))
txt.title <- if (missing(title)) paste("Test Deviance's ordered from",scale) else title   
if  (plot) # if we need to plot
{  val$models <- MN
   gg <- switch(scale,
                "[0,1]"={
                    ggplot2::ggplot(val, ggplot2::aes(models, scaled))+
                    ggplot2::geom_bar(stat="identity", width=width, colour=col)+
                    ggplot2::ggtitle(txt.title) 
                        },
                "[min,max]"={ 
                  ggplot2::ggplot(val, ggplot2::aes(models, delta))+
                    ggplot2::geom_bar(stat="identity", width=width, colour=col)+
                    ggplot2::ggtitle(txt.title)})
  if (horiz) gg <- gg + ggplot2::coord_flip()         
  return(gg)
} else
  val <- val[,]
return(val)
}
# end of model_TD
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################ 
model_TD_lollipop<- function(..., 
                              newdata,
                               plot = TRUE,   # whether plot or print
                            diff.TD = 1000,   # only applies to matrix
                       text.to.show = NULL,   # whether the labels should be different
                                col = "skyblue",  # colour for the bars
                          col.point = "blue",
                          pch.point = 19,
                              width = 0.9,  # widrh of the bar 
                              horiz = TRUE,
                              scale = c("[0,1]","[min,max]"),
                               order.val = TRUE,
                               title)   
{
  #-----------------------------------------------------------------------------
model_TD_call <- match.call()  #   the function call
        scale <- match.arg(scale)
        delta <- scaled <- NULL  
       models <- list(...)
      isgamlss <- unlist(lapply(models, inherits, what=c("gamlss","gamlss2")))
if (!any(isgamlss)) stop("some of the objects are not gamlss")
if (length(models)==1) stop("The function needs more than one model")
if (missing(newdata)) 
          stop("The newdata option is needed (you can use the training set)")  
            nN <- dim(newdata)[1] 
             K <- length(models)                   ## how many models
            TD <- rep_len(0, length.out=K)
            MN <- rep_len("",length.out=K )
for (i in 1:K)
        {   
          MN[i] <- paste0(match.call()[[1+i]]) 
          cModel <- class(models[[i]])[1]
          TD[i] <- if (cModel=="gamlss") getTGD(models[[i]], newdata=newdata)$TGD
          else                 -2*logLik(models[[i]], newdata=newdata)
        }  

            wmin <- which.min(TD)
            DTD <- TD-TD[wmin]# unfortunately the range can atrocious because of very large AIC's  
            TD <- TD[DTD < diff.TD]
            dAIC <- max(TD, na.rm = TRUE)-TD #(AIC-AIC[which.min(AIC)]) 
            oAIC <- dAIC/(max(TD, na.rm = TRUE)-min(TD,  na.rm = TRUE))
            val <- data.frame(cbind(TD, delta=round(dAIC,3), scaled=round(oAIC,4)))
            txt.title <- if (missing(title)) paste("Test Deviance's ordered from",scale) else title 
    if  (plot) # if we need to plot
    {
      val$models <- MN
      val$models = with(val, reorder(models, scaled, median))
      #if (order.val) valo <- val[order(val$scaled),]
      gg <- switch( scale,
                    "[0,1]"={
             ggplot2::ggplot(val, ggplot2::aes(x=models, y=scaled))+
             ggplot2::geom_segment( ggplot2::aes(x=models, xend=models, y=0, 
                                                    yend=scaled), color=col)+
             ggplot2::geom_point( color=col.point, size=4, alpha=0.6, 
                                             pch=pch.point) +# + 
             ggplot2::labs(y="scaled AIC")
                    },
                    "[min,max]"={ 
             ggplot2::ggplot(val, ggplot2::aes(models, delta))+
             ggplot2::geom_segment( ggplot2::aes(x=models, xend=models, y=0, 
                                                    yend=delta), color=col)+
             ggplot2::geom_point( color=col.point, size=4, alpha=0.6,  
                                                    pch=pch.point)  
                    })
      if (horiz) gg <- gg + ggplot2::coord_flip()         
      return(gg)
    } else
      val <- val[,]
    return(val)
}
# end of model_GAIC
################################################################################
################################################################################
################################################################################
################################################################################
