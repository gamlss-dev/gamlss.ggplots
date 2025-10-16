# create 08-6-21
#--------------------------------------------------------------
# TO DO
# i)   what action we should have with factors? fixed
# ii)  binomial is excluded at the meoment 
# iii) waht about  count data
# v)   different colour scheme? 
# -------------------------------------------------------------
pe_pdf_fv <- function (obj = NULL, 
                        term = NULL, 
                    y.points = 100,
                    x.points = 10, 
                        data = NULL, 
                       scale = NULL, 
                         how = c("median", "last"), 
                    scenario = list(), 
                        size = 0.1, # the size of the line
                  horizontal = TRUE, 
                    col.fill = hcl.colors(lqq, palette="viridis"),
                       alpha = 0.6,
                       title) 
{
if (is.null(obj) || !class(obj)[1] == "gamlss") 
    stop("Supply a standard GAMLSS model in obj")
if (is.null(term)) stop("The model term is not set")
       how <- match.arg(how)
  x <-  y <- width <- mu <- NULL
if (any(grepl("data", names(obj$call)))) 
  {
      DaTa <- if (startsWith(as.character(obj$call["data"]), "na.omit")) 
                   eval(parse(text = as.character(obj$call["data"])))
              else get(as.character(obj$call["data"]))
  }
else if (is.null(data)) 
    stop("The data argument is needed in obj")
   v.names <- names(DaTa)
       pos <- which(v.names==term)
if (pos<1) stop("supply a  term")
if (is.factor(DaTa[,pos])) 
   {
         xvar <- levels(DaTa[,pos])
     x.points <- nlevels(DaTa[,pos])
 it.is.factor <- TRUE
   } else
   {
         xvar <-  seq(from = min(DaTa[,pos]), to=max(DaTa[,pos]), length.out=x.points)
 it.is.factor <- FALSE
   }                 
    dat.temp <- as.data.frame(matrix(0, nrow = dim(DaTa)[1] + x.points, ncol = dim(DaTa)[2]))
names(dat.temp) <- names(DaTa)
if (pos < 1)  stop("supply a term")
  for (i in 1:dim(dat.temp)[2]) {
    if (pos == i) 
      {
      dat.temp[, i] <- c(DaTa[, i], xvar)
      }
    else 
      {
      ma <- scenario[[v.names[i]]]
if (is.null(ma)) 
        {
        if (how == "median") 
          {
          if (is.character(DaTa[,i])) DaTa[,i] <- as.factor(DaTa[,i])
          ma <- if (is.factor(DaTa[, i])) 
            levels(DaTa[, i])[which.max(table(DaTa[, i]))]
          else median(DaTa[, i])
          }
        if (how == "last") {
          if (is.character(DaTa[,i])) DaTa[,i] <- as.factor(DaTa[,i])
          ma <- if (is.factor(DaTa[, i])) 
            levels(DaTa[, i])[which.max(table(DaTa[, i]))]
          else tail(DaTa[, i], 1)
          }
        }
      dat.temp[, i] <- c(DaTa[, i], rep(ma, x.points))
      }
  }
# family -------------------------------------------------
      pdf <- obj$family[1]
   binom  <- pdf%in%gamlss::.gamlss.bi.list # whether binomial
   if (binom) stop("binomial type response is non implemented yet")
   if (binom) {bd <- obj$bd ; Y <- obj$y}
     dfun <- paste("d", obj$family[[1]],sep="")
     lpar <- eval(parse(text=pdf))()$nopar
       pp <-  predictAll(obj, newdata = tail(dat.temp, x.points), output="matrix")
  # get the x.points 
  # if (binom)
  #      {
  #      pp <-  predictAll(obj, newdata = tail(dat.temp, x.points), output="matrix")
  #        DevIncr <- switch(lpar, 
  #                          fn( Y[i], mu = pp[,"mu"], bd=bd[i]),   # 1
  #                          fn( Y[i], mu = pp[,"mu"],              # 2
  #                              sigma = pp[,"sigma"], bd=bd[i]),                        
  #                          fn( Y[i], mu = pp[,"mu"],              # 3
  #                              sigma = pp[,"sigma"],
  #                              nu = pp[,"nu"], bd=bd[i]),
  #                          fn( Y[i], mu = pp[,"mu"],              # 4
  #                              sigma = pp[,"sigma"],  
  #                              nu = pp[,"nu"],
  #                              tau = pp[,"tau"],bd=bd[i]))
  #      } else
  #      {
  TypeDist <- eval(parse(text=pdf))()$type
   if (TypeDist=="Discrete") 
   {
     yvar <- seq(from =  min(obj$y), to=max(obj$y), 1 )  
   } else
   {
     yvar <- seq(from = min(obj$y), to=max(obj$y), length.out=y.points )   
   }   
    # yvar <- seq(from = min(obj$y), to=max(obj$y), length.out=y.points )    
      ppp <- as.data.frame(cbind(pp, xvar=xvar))  
       qq <- list()
      lqq <- length(xvar) 
if (lqq==1) # if only one x.points 
{
  qq [[1]] <- switch(lpar, 
                     eval(call(dfun, p= yvar, mu=pp[,"mu"])),       # 1
                     eval(call(dfun, p= yvar, mu=pp[,"mu"], sigma=pp[,"sigma"])),        # 2
                     eval(call(dfun, p= yvar, mu=pp[,"mu"], sigma=pp[,"sigma"],  nu=pp[,"nu"])),  # 3                   
                     eval(call(dfun, p= yvar, mu=pp[,"mu"], sigma=pp[,"sigma"],  nu=pp[,"nu"], tau=pp[,"tau"]))) # 4
 } else
 {
  for (i in 1:lqq)
  {
  qq[[i]] <- switch(lpar, 
                eval(call(dfun, x= yvar, mu=pp[,"mu"][i])),       # 1
                eval(call(dfun, x= yvar, mu=pp[,"mu"][i], sigma=pp[,"sigma"][i])),        # 2
                eval(call(dfun, x= yvar, mu=pp[,"mu"][i], sigma=pp[,"sigma"][i],  nu=pp[,"nu"][i])),  # 3                   
                eval(call(dfun, x= yvar, mu=pp[,"mu"][i], sigma=pp[,"sigma"][i],  nu=pp[,"nu"][i], tau=pp[,"tau"][i]))) # 4
  }
 }  
xaxislabel <- paste0("PE_pdf(", term, ")")
    
 txt.title <- if (missing(title))
{
  paste("Partial fitted distributions for", term,"from model", deparse(substitute(obj)))
} else title
   height <- unlist(qq)
        x <- rep(yvar,lqq)
        y <- if (TypeDist == "Discrete") as.vector(t(replicate(max(yvar) + 1, xvar)))
             else as.vector(t(replicate(y.points, xvar)))
 if (horizontal)
  {
      da <- data.frame(x = x, y = y, height)
   scale <- if (is.null(scale)) 
   {
     if (it.is.factor)  2/max(da$height) 
     else  diff(da$x)[1]/max(da$height) 
   } else scale
   
      pp <- ggplot(da, aes(x, y, height = height, group = y)) + 
            geom_ridgeline(scale=scale, fill= col.fill[as.factor(da$y)],
                       alpha = alpha, size = size)+
            ggtitle(txt.title) + xlab(xaxislabel)+ ylab(term)

  } else 
  {
      da <- data.frame(x= y, y = x, width=height)
   scale <- if (is.null(scale)) diff(da$y)[1]/max(da$width) else scale
      pp <- ggplot(da, aes(x, y, width = width, group = x)) + 
           geom_point(data=ppp, aes(x =xvar, y=mu, group=xvar))+
             geom_vridgeline(scale=scale, 
                fill = col.fill[as.factor(da$x)], alpha = alpha, size = size) +
      ggtitle(txt.title) + xlab(term ) + ylab(xaxislabel)
  }
  return(pp)
}

