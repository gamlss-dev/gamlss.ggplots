################################################################################
################################################################################
################################################################################
################################################################################
# require(yaImpute)
################################################################################
################################################################################
################################################################################
################################################################################
# this is the version based on the function ALEPlot() of packege ALEPlot
# It changes the order of the arguments from the original 
# ale_param1() 
################################################################################
################################################################################
################################################################################
################################################################################
# X is the data 
# X.model is the fitted gamlss model 
#  j is the number of variables 
# the function should look similar to pe_param
#   obj = NULL, term = NULL, data = NULL, n.points = 100, 
#   parameter = c("mu", "sigma", "nu", "tau"), 
#        type = c("parameter", "eta"), 
#    scenario = list(),  
#         how = c("median", "last", "fixed"),
#         col = "darkblue", size = 1.3, name.obj = NULL,
#    rug.plot = TRUE, rug.col = "gray", rug.size = 0.5,  
#   data.plot = FALSE, data.col = "lightblue", 
#   data.size = 0.1, factor.size = 15,
#  data.alpha = 0.9, bins = 30, 
#      filled = FALSE, ylim = NULL,
# title
################################################################################
################################################################################
################################################################################
################################################################################
ale_param <- function (obj, term, data, 
                        K = 40, 
                  NA.plot = TRUE, 
                     plot = TRUE,
                parameter = c("mu", "sigma", "nu", "tau"),
                     type = c("parameter", "eta"),
                      col = "darkblue",
               linewidth = 1.3,
                 name.obj = NULL,
                 rug.plot = TRUE,
                  rug.col = "gray",
                 rug.size = 0.5,
              factor.size = 15,
                     ylim = NULL,
                     bins = 30, # for contour plot
                   filled = FALSE, #for contour plot
                    title # whether to plot
                      ) 
{
if (!missing(obj)&&!(inherits(obj,c("gamlss", "gamlss2")))) 
    stop("the model is not a gamlss model")  
if (is.null(term))  stop("The model term is not set")
         term <- as.character(term) 
         type <- match.arg(type)
    parameter <- match.arg(parameter)
if (inherits(obj, "gamlss"))
    {
      if (any(grepl("data", names(obj$call)))) 
      {
        DaTa <- if (startsWith(as.character(obj$call["data"]), "na.omit"))
          eval(parse(text=as.character(obj$call["data"]))) 
        else get(as.character(obj$call["data"]))	
        v.names <- names(DaTa)
       type <- if (type=="parameter") "response" else "link" 
        
      }
      else if (missing(data)) stop("The data argument is needed in obj")   
      
    } else
    {
      DaTa <-model.frame(obj)
      v.names <-  colnames(DaTa) # names(DaTa)
      type <- if (type=="eta") "link" else "parameter" 
    }  
          X <-  DaTa
          N <- dim(X)[1]
          d <- dim(X)[2]
          y <- NULL
   name.obj <-  if (is.null(name.obj))  deparse(substitute(obj)) else name.obj
################################################################################      
################################################################################      
if (length(term) == 1) 
 {
################################################################################
# if factor
################################################################################  
if (inherits(X[, term], "factor")) 
  {
       X[, term] <- droplevels(X[, term])
      x.count <- as.numeric(table(X[, term]))
       x.prob <- x.count/sum(x.count)
            K <- nlevels(X[, term])
        D.cum <- matrix(0, K, K)
            D <- matrix(0, K, K)
for (j in setdiff(1:d, term)) 
  {
    if (inherits(X[, j],"factor")) 
      {
          A = table(X[, term], X[, j])
          A = A/x.count
          for (i in 1:(K - 1)) 
            {
            for (k in (i + 1):K) 
              {
              D[i, k] = sum(abs(A[i, ] - A[k, ]))/2
              D[k, i] = D[i, k]
              }
             }
          D.cum <- D.cum + D
       } else 
       {
          q.x.all <- quantile(X[, j], probs = seq(0, 
                             1, length.out = 100), na.rm = TRUE, names = FALSE)
          x.ecdf = tapply(X[, j], X[, term], ecdf)
          for (i in 1:(K - 1)) 
            {
            for (k in (i + 1):K) 
              {
              D[i, k] = max(abs(x.ecdf[[i]](q.x.all) - 
                                  x.ecdf[[k]](q.x.all)))
              D[k, i] = D[i, k]
              }
           }
          D.cum <- D.cum + D
        }
}
          D1D <- cmdscale(D.cum, k = 1)
      ind.ord <- sort(D1D, index.return = T)$ix
      ord.ind <- sort(ind.ord, index.return = T)$ix
    levs.orig <- levels(X[, term])
     levs.ord <- levs.orig[ind.ord]
        x.ord <- ord.ind[as.numeric(X[, term])]
 row.ind.plus <- (1:N)[x.ord < K]
  row.ind.neg <- (1:N)[x.ord > 1]
       X.plus <- X
        X.neg <- X
X.plus[row.ind.plus, term] <- levs.ord[x.ord[row.ind.plus] + 1]
  X.neg[row.ind.neg, term] <- levs.ord[x.ord[row.ind.neg] - 1]
        y.hat <- predict.gamlss(object = obj, newdata = X,                     
                         parameter=parameter, type=type)
   y.hat.plus <- predict.gamlss(object = obj, newdata = X.plus[row.ind.plus,], 
                         parameter=parameter, type=type)
    y.hat.neg <- predict.gamlss(object = obj, newdata = X.neg[row.ind.neg, ],  
                         parameter=parameter, type=type)
   Delta.plus <- y.hat.plus - y.hat[row.ind.plus]
    Delta.neg <- y.hat[row.ind.neg] - y.hat.neg
        Delta <- as.numeric(tapply(c(Delta.plus, Delta.neg), 
                                 c(x.ord[row.ind.plus], x.ord[row.ind.neg] - 1), 
                                 mean))
           fJ <- c(0, cumsum(Delta))
           fJ <- fJ - sum(fJ * x.prob[ind.ord])
            x <- levs.ord
         DaTa <- data.frame(x=x, fJ=fJ)
   yaxislabel <- if (type=="parameter") paste0("ALE_param(", term, ")")
                 else   paste0("ALE_eta](", term, ")")
         txt.title <- if (missing(title))  
           paste("ALE of",term, "for", parameter, "for model", name.obj)
         else title
          gg <-  ggplot(data=DaTa, aes(x, fJ)) +  
                 geom_col()+
              ylab(yaxislabel)+ xlab(term)+ ggtitle(txt.title)
if (plot) 
 {
            suppressWarnings(return(gg))
 } else 
 {
            return(DaTa)
 }  
}
################################################################################
# if continuous
################################################################################  
    else if (inherits(X[, term], "numeric") | inherits(X[, term],
             "integer")) {
            z <- c(min(X[, term]), as.numeric(quantile(X[, term], seq(1/K, 
                                              1, length.out = K), type = 1)))
            z <- unique(z)
            K <- length(z) - 1
           fJ <- numeric(K)
           a1 <- as.numeric(cut(X[, term], breaks = z, include.lowest = TRUE))
           X1 <- X
           X2 <- X
      X1[, term] <- z[a1]
      X2[, term] <- z[a1 + 1]
     #  y1 = predict.gamlss(obj, newdata = X1, parameter="mu", type="eta")
     # y2 = predict.gamlss(obj, newdata = X2, parameter="mu", type="eta")
if (inherits(obj,"gamlss"))
{
  y.hat1 <- predict.gamlss(obj, newdata = X1, parameter=parameter, type=type)
  y.hat2 <- predict.gamlss(obj, newdata = X2, parameter=parameter, type=type)
} else
{
  y.hat1 <- predict(obj, newdata = X1, model=parameter, type=type)
  y.hat2 <- predict(obj, newdata = X2, model=parameter, type=type)
}  
        Delta <- y.hat2 - y.hat1
        Delta <- as.numeric(tapply(Delta, a1, mean))
           fJ <- c(0, cumsum(Delta))
           b1 <- as.numeric(table(a1))
           fJ <- fJ - sum((fJ[1:K] + fJ[2:(K + 1)])/2 * b1)/sum(b1)
            x <- z
   yaxislabel <- if (type=="parameter") paste0("ALE_param(", term, ")")
                 else   paste0("ALE_eta](", term, ")")
    txt.title <- if (missing(title))  
                  paste("ALE of",term, "for", parameter, "for model", name.obj)
                 else title
          DaTa <- data.frame(x=x, fJ=fJ)
           gg <-  ggplot(data=DaTa) +  
                  geom_line( aes(x=x, y=fJ), color=col, linewidth=linewidth) +
                  ylab(yaxislabel)+ xlab(term)+ ggtitle(txt.title)
  if ( rug.plot)
    {
           gg <- gg +
                 geom_rug(data=DaTa, aes(x=x), col=rug.col, size=rug.size)
    }
    if ( !is.null(ylim))
    {
           gg <- gg + ylim(ylim)
    }  
if (plot) 
    {
    suppressWarnings(return(gg))
    } else 
    {
             return(DaTa)
    }  
    }
    else print("error:  class(X[,term]) must be either factor or numeric or integer")
} # length equql 1 finish here ################################################# 
################################################################################
# if length is 2
################################################################################   
else if (length(term) == 2) 
{
 if (inherits(X[, term[2]],"numeric") & inherits(X[, term[2]], "integer")) 
   {
      print("error: X[,term[2]] must be numeric or integer. Only X[,term[1]] can be a factor")
   }
 if (inherits(X[, term[1]], "factor")) 
   {
      X[, term[1]] <- droplevels(X[, term[1]])
        x.count <- as.numeric(table(X[, term[1]]))
         x.prob <- x.count/sum(x.count)
             K1 <- nlevels(X[, term[1]])
          D.cum <- matrix(0, K1, K1)
              D <- matrix(0, K1, K1)
    for (j in setdiff(1:d, term[1])) 
      {
        if (inherits(X[, j],"factor")) 
          {
             A <- table(X[, term[1]], X[, j])
             A <- A/x.count
          for (i in 1:(K1 - 1)) 
            {
            for (k in (i + 1):K1) 
              {
              D[i, k] <- sum(abs(A[i, ] - A[k, ]))/2
              D[k, i] <- D[i, k]
              }
            }
          D.cum <- D.cum + D
        }
        else 
        {
          q.x.all <- quantile(X[, j], probs = seq(0, 1, length.out = 100), 
                              na.rm = TRUE, names = FALSE)
           x.ecdf <- tapply(X[, j], X[, term[1]], ecdf)
          for (i in 1:(K1 - 1)) 
            {
            for (k in (i + 1):K1) 
              {
              D[i, k] = max(abs(x.ecdf[[i]](q.x.all) - 
                                  x.ecdf[[k]](q.x.all)))
              D[k, i] = D[i, k]
              }
            }
          D.cum <- D.cum + D
        }
    }
            D1D <- cmdscale(D.cum, k = 1)
        ind.ord <- sort(D1D, index.return = T)$ix
        ord.ind <- sort(ind.ord, index.return = T)$ix
      levs.orig <- levels(X[, term[1]])
       levs.ord <- levs.orig[ind.ord]
          x.ord <- ord.ind[as.numeric(X[, term[1]])]
             z2 <- c(min(X[, term[2]]), as.numeric(quantile(X[, term[2]], 
                                      seq(1/K, 1, length.out = K), type = 1)))
             z2 <- unique(z2)
             K2 <- length(z2) - 1
             a2 <- as.numeric(cut(X[, term[2]], breaks = z2, include.lowest = TRUE))
   row.ind.plus <- (1:N)[x.ord < K1]
            X11 <- X
            X12 <- X
            X21 <- X
            X22 <- X
      X11[row.ind.plus, term[2]] <- z2[a2][row.ind.plus]
      X12[row.ind.plus, term[2]] <- z2[a2 + 1][row.ind.plus]
      X21[row.ind.plus, term[1]] <- levs.ord[x.ord[row.ind.plus] +                                          1]
      X22[row.ind.plus, term[1]] <- levs.ord[x.ord[row.ind.plus] + 1]
      X21[row.ind.plus, term[2]] <- z2[a2][row.ind.plus]
      X22[row.ind.plus, term[2]] <- z2[a2 + 1][row.ind.plus]
if(inherits(obj, "gamlss"))
{
  y.hat11 <- predict.gamlss(object = obj, newdata = X11[row.ind.plus,], 
                            parameter=parameter, type=type)
  y.hat12 <- predict.gamlss(object = obj, newdata = X12[row.ind.plus,], 
                            parameter=parameter, type=type)
  y.hat21 <- predict.gamlss(object = obj, newdata = X21[row.ind.plus,], 
                            parameter=parameter, type=type)
  y.hat22 <- predict.gamlss(object = obj, newdata = X22[row.ind.plus,], 
                            parameter=parameter, type=type)
} else 
  
{
  y.hat11 <- predict(object = obj, newdata = X11[row.ind.plus,], 
                            model=parameter, type=type)
  y.hat12 <- predict(object = obj, newdata = X12[row.ind.plus,], 
                            model=parameter, type=type)
  y.hat21 <- predict(object = obj, newdata = X21[row.ind.plus,], 
                            model=parameter, type=type)
  y.hat22 <- predict(object = obj, newdata = X22[row.ind.plus,], 
                            model=parameter, type=type) 
}  
        
     Delta.plus <- (y.hat22 - y.hat21) - (y.hat12 - y.hat11)
    row.ind.neg <- (1:N)[x.ord > 1]
            X11 <- X
            X12 <- X
            X21 <- X
            X22 <- X
X11[row.ind.neg, term[1]] <- levs.ord[x.ord[row.ind.neg] - 1]
X12[row.ind.neg, term[1]] <- levs.ord[x.ord[row.ind.neg] - 1]
X11[row.ind.neg, term[2]] <- z2[a2][row.ind.neg]
X12[row.ind.neg, term[2]] <- z2[a2 + 1][row.ind.neg]
X21[row.ind.neg, term[2]] <- z2[a2][row.ind.neg]
X22[row.ind.neg, term[2]] <- z2[a2 + 1][row.ind.neg]
       y.hat11 <- predict.gamlss(obj, newdata = X11[row.ind.neg,], parameter=parameter, type=type)
       y.hat12 <- predict.gamlss(obj, newdata = X12[row.ind.neg,], parameter=parameter, type=type)
       y.hat21 <- predict.gamlss(obj, newdata = X21[row.ind.neg,], parameter=parameter, type=type)
       y.hat22 <- predict.gamlss(obj, newdata = X22[row.ind.neg,], parameter=parameter, type=type)
     Delta.neg <- (y.hat22 - y.hat21) - (y.hat12 - y.hat11)
         Delta <- as.matrix(tapply(c(Delta.plus, Delta.neg), 
                             list(c(x.ord[row.ind.plus], x.ord[row.ind.neg] - 1), 
                                  a2[c(row.ind.plus, row.ind.neg)]), mean))
      NA.Delta <- is.na(Delta)
        NA.ind <- which(NA.Delta, arr.ind = T, useNames = F)
  if (nrow(NA.ind) > 0) 
    {
        notNA.ind <- which(!NA.Delta, arr.ind = T, useNames = F)
        range1 <- K1 - 1
        range2 <- max(z2) - min(z2)
          Z.NA <- cbind(NA.ind[, 1]/range1, (z2[NA.ind[, 2]] + 
                                      z2[NA.ind[, 2] + 1])/2/range2)
       Z.notNA <- cbind(notNA.ind[, 1]/range1, (z2[notNA.ind[, 2]] + 
                                      z2[notNA.ind[, 2] + 1])/2/range2)
          nbrs <- yaImpute::ann(Z.notNA, Z.NA, k = 1, verbose = F)$knnIndexDist[, 1]
 Delta[NA.ind] <- Delta[matrix(notNA.ind[nbrs, ], ncol = 2)]
      }
           fJ <- matrix(0, K1 - 1, K2)
           fJ <- apply(t(apply(Delta, 1, cumsum)), 2, cumsum)
           fJ <- rbind(rep(0, K2), fJ)
           fJ <- cbind(rep(0, K1), fJ)
            b <- as.matrix(table(x.ord, a2))
           b2 <- apply(b, 2, sum)
        Delta <- fJ[, 2:(K2 + 1)] - fJ[, 1:K2]
      b.Delta <- b * Delta
    Delta.Ave <- apply(b.Delta, 2, sum)/b2
          fJ2 <- c(0, cumsum(Delta.Ave))
        b.ave <- matrix((b[1:(K1 - 1), ] + b[2:K1, ])/2, K1 -1, K2)
           b1 <- apply(b.ave, 1, sum)
        Delta <- matrix(fJ[2:K1, ] - fJ[1:(K1 - 1), ], K1 -1, K2 + 1)
      b.Delta <- matrix(b.ave * (Delta[, 1:K2] + Delta[,2:(K2 + 1)])/2, K1 - 1, K2)
    Delta.Ave <- apply(b.Delta, 1, sum)/b1
          fJ1 <- c(0, cumsum(Delta.Ave))
           fJ <- fJ - outer(fJ1, rep(1, K2 + 1)) - outer(rep(1, K1), fJ2)
          fJ0 <- sum(b * (fJ[, 1:K2] + fJ[, 2:(K2 + 1)])/2)/sum(b)
           fJ <- fJ - fJ0
            x <- list(levs.ord, z2)
            K <- c(K1, K2)
  image(1:K1, x[[2]], fJ, xlab = paste("x_", term[1], 
        " (", names(X)[term[1]], ")", sep = ""), ylab = paste("x_", 
           term[2], " (", names(X)[term[2]], ")", sep = ""), ylim = range(z2), 
            yaxs = "i")
      contour(1:K1, x[[2]], fJ, add = TRUE, drawlabels = TRUE)
      axis(side = 1, labels = x[[1]], at = 1:K1, las = 3, padj = 1.2)
  if (NA.plot == FALSE) {
        if (nrow(NA.ind) > 0) {
          NA.ind = which(b == 0, arr.ind = T, useNames = F)
          rect(xleft = NA.ind[, 1] - 0.5, ybottom = z2[NA.ind[, 
               2]], xright = NA.ind[, 1] + 0.5, ytop = z2[NA.ind[, 
                                    2] + 1], col = "black")
        }
      }
    }
  else if (inherits(X[, term[1]],"numeric") | inherits(X[, term[1]],
             "integer")) 
    {
      z1 <- c(min(X[, term[1]]), as.numeric(quantile(X[, term[1]], 
                                                 seq(1/K, 1, length.out = K), type = 1)))
      z1 <- unique(z1)
      K1 <- length(z1) - 1
      a1 <- as.numeric(cut(X[, term[1]], breaks = z1, include.lowest = TRUE))
      z2 <- c(min(X[, term[2]]), as.numeric(quantile(X[, term[2]], 
                                 seq(1/K, 1, length.out = K), type = 1)))
      z2 <- unique(z2)
      K2 <- length(z2) - 1
      fJ <- matrix(0, K1, K2)
      a2 <- as.numeric(cut(X[, term[2]], breaks = z2, include.lowest = TRUE))
     X11 <- X
     X12 <- X
     X21 <- X
     X22 <- X
      X11[, term] <- cbind(z1[a1], z2[a2])
      X12[, term] <- cbind(z1[a1], z2[a2 + 1])
      X21[, term] <- cbind(z1[a1 + 1], z2[a2])
      X22[, term] <- cbind(z1[a1 + 1], z2[a2 + 1])
if(inherits(obj,"gamss"))
{
  y.hat11 <- predict.gamlss(obj, newdata = X11, parameter=parameter, type=type)
  y.hat12 <- predict.gamlss(obj, newdata = X12, parameter=parameter, type=type)
  y.hat21 <- predict.gamlss(obj, newdata = X21, parameter=parameter, type=type)
  y.hat22 <- predict.gamlss(obj, newdata = X22, parameter=parameter, type=type)
} else 
{
  y.hat11 <- predict(obj, model=parameter, newdata = X11, type=type)
  y.hat12 <- predict(obj, model=parameter, newdata = X12, type=type)
  y.hat21 <- predict(obj, model=parameter, newdata = X21, type=type)
  y.hat22 <- predict(obj, model=parameter,newdata = X22,  type=type)
}  
       
         Delta <- (y.hat22 - y.hat21) - (y.hat12 - y.hat11)
         Delta <- as.matrix(tapply(Delta, list(a1, a2), mean))
      NA.Delta <- is.na(Delta)
        NA.ind <- which(NA.Delta, arr.ind = T, useNames = F)
if (nrow(NA.ind) > 0) 
    {
     notNA.ind <- which(!NA.Delta, arr.ind = T, useNames = F)
        range1 <- max(z1) - min(z1)
        range2 <- max(z2) - min(z2)
          Z.NA <- cbind((z1[NA.ind[, 1]] + z1[NA.ind[, 1] + 
                        1])/2/range1, (z2[NA.ind[, 2]] + z2[NA.ind[, 
                                                  2] + 1])/2/range2)
       Z.notNA <- cbind((z1[notNA.ind[, 1]] + z1[notNA.ind[,1] + 1])/2/range1, 
                        (z2[notNA.ind[, 2]] + z2[notNA.ind[,2] + 1])/2/range2)
          nbrs <- yaImpute::ann(Z.notNA, Z.NA, k = 1, verbose = F)$knnIndexDist[,1]
 Delta[NA.ind] <- Delta[matrix(notNA.ind[nbrs,], ncol = 2)]
      }
            fJ <- apply(t(apply(Delta, 1, cumsum)), 2, cumsum)
            fJ <- rbind(rep(0, K2), fJ)
            fJ <- cbind(rep(0, K1 + 1), fJ)
             b <- as.matrix(table(a1, a2))
            b1 <- apply(b, 1, sum)
            b2 <- apply(b, 2, sum)
         Delta <- fJ[2:(K1 + 1), ] - fJ[1:K1, ]
       b.Delta <- b * (Delta[, 1:K2] + Delta[, 2:(K2 + 1)])/2
     Delta.Ave <- apply(b.Delta, 1, sum)/b1
           fJ1 <- c(0, cumsum(Delta.Ave))
         Delta <- fJ[, 2:(K2 + 1)] - fJ[, 1:K2]
       b.Delta <- b * (Delta[1:K1, ] + Delta[2:(K1 + 1), ])/2
     Delta.Ave <- apply(b.Delta, 2, sum)/b2
           fJ2 <- c(0, cumsum(Delta.Ave))
            fJ <- fJ - outer(fJ1, rep(1, K2 + 1)) - outer(rep(1, K1 + 1), fJ2)
           fJ0 <- sum(b * (fJ[1:K1, 1:K2] + fJ[1:K1, 2:(K2 + 1)] + 
                           fJ[2:(K1 + 1), 1:K2] + fJ[2:(K1 + 1), 2:(K2+1)])/4)/sum(b)
            fJ <- fJ - fJ0
             x <- list(z1, z2)
             K <- c(K1, K2)
     txt.title <- if (missing(title))  
                paste("ALE of", term[[1]], "and", term[[2]], "for", parameter, "for model", name.obj)
                   else title
            d1 <- expand.grid(x = x[[1]], y = x[[2]])
            da <- data.frame(z=as.vector(fJ), d1)
            pp <- ggplot(da, aes(x, y))
            pp <- if (filled)  pp + geom_contour_filled(aes(z = z), bins=bins/3)
                  else         pp + geom_contour(aes(z = z), bins=bins, linewidth=linewidth,  colour=col)
            pp <- pp + ggtitle(txt.title)        
if (plot) {
        suppressWarnings(return(pp))
          } else 
          {
                return(da)
          }  
    }
    else print("error:  class(X[,term[1]]) must be either factor or numeric/integer")
}
  else print("error:  term must be a vector of length one or two")

  # invisible(list(K = K, x.values = x, f.values = fJ))
}
################################################################################
################################################################################
################################################################################
ale_param_grid <- function(model, terms, data, maxcol=2, maxrow=3, ylim=NULL, ...)
{  
################################################################################ 
  define_region <- function(row, col){
    viewport(layout.pos.row=row, layout.pos.col=col) }
################################################################################  
# function starts
lterms <- length(terms)
if (lterms  >   maxcol*maxrow) stop("increase the maxcol or maxrow")   
  norow <- ceiling(lterms/maxcol)
  nocol <- if (norow == 1)  lterms  else  maxcol    
  IJ <- expand.grid(j=1:nocol, i=1:norow)
grid.newpage()
  pushViewport(viewport(layout=grid.layout(nrow=norow,ncol=nocol)))   
  GG <- list()
  # for (i in xs ) gg[[i]] <-pe_pdf(linear_3, term=i, title=i)
  for (p  in 1:lterms) 
  {
    title.term <- if (length(terms[[p]])==1) terms[[p]]
    else paste(terms[[p]], collapse=":")  
    GG[[title.term]] <- ale_param(model, term=terms[[p]], data=data,
                                 title= title.term, ylim=ylim, ...)
    print(GG[[title.term]], vp=define_region(IJ$i[p], IJ$j[p]))
  }
}
################################################################################
################################################################################
################################################################################
################################################################################
predict.gamlss <- function(object, 
                           what = c("mu", "sigma", "nu", "tau"), 
                           parameter = NULL,
                           newdata = NULL, 
                           type = c("link", "response", "terms"), # terms not working 
                           terms = NULL, 
                           se.fit = FALSE, 
                           data = NULL, ...)                                                                  
{
## this little function put data frames together 
##  originated from an the R-help reply by B. Ripley
##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
##-------- concat starts here
  concat <- function(..., names=NULL) 
  { 
    tmp <- list(...) 
    if(is.null(names)) names <- names(tmp) 
    if(is.null(names)) names <- sapply( as.list(match.call()), deparse)[-1] 
    if( any( 
      sapply(tmp, is.matrix) 
      | 
      sapply(tmp, is.data.frame) ) ) 
    { 
      len <- sapply(tmp, function(x) c(dim(x),1)[1] ) 
      len[is.null(len)] <- 1 
      data <- rbind( ... ) 
    } 
    else 
    { 
      len <- sapply(tmp,length) 
      data <- unlist(tmp)     
    } 
    namelist <- factor(rep(names, len), levels=names)          
    return( data.frame( data, source=namelist) ) 
  } 
  ##----------concat finish here
  ##--------------------------------------------------------------------------------------------
  ##--------------------------------------------------------------------------------------------
  ##   main function starts here
  ##----------------------------
  ## If no new data just use lpred() and finish
  if (is.null(newdata))  # 
  {
    predictor<- lpred(object, what = what, type = type, terms = terms, se.fit = se.fit, ... )
    return(predictor)
  }
  ## at the moment se.fit is not supported for new data
  if (se.fit) 
    warning(" se.fit = TRUE is not supported for new data values at the moment \n")
  ##  stop if newdata is not data frame
  ## note that atomic is not working here so better to take it out Mikis 23-10-13 
  ## if (!(is.atomic(newdata) | inherits(newdata, "data.frame")))
  if (!(inherits(newdata, "data.frame")))
    stop("newdata must be a data frame ") # or a frame mumber
  ## getting which parameter and type   
  what <- if (!is.null(parameter))  {
    match.arg(parameter, choices=c("mu", "sigma", "nu", "tau"))} else  match.arg(what)
  type <- match.arg(type)
  ## get the original call 
  Call <- object$call
  ## we need both the old and the new data sets
  ## the argument data can be provided by predict
  data<- data1 <- if (is.null(data))
  {        ## if it is not provided then get it from the original call
    if (!is.null(Call$data)) eval(Call$data) 
    else stop("define the original data using the option data") 
  }
  else data # if it provide get it 
  ## keep only the same variables 
  ## this assumes that all the relevant variables will be in newdata
  ## what happens if not?
  data <- data[match(names(newdata),names(data))]  
  ## merge the two data together
  data <- concat(data,newdata)
  ## get the formula 
  parform <- formula(object, what)# object[[paste(what, "formula", sep=".")]]
  ## put response to NULL
  if (length(parform)==3)
    parform[2] <- NULL
  ## define the terms 
  Terms <- terms(parform)
  ## get the offset
  offsetVar <- if (!is.null(off.num <- attr(Terms, "offset"))) # new 
    eval(attr(Terms, "variables")[[off.num + 1]], data) 
  ## model frame 
  m <- model.frame(Terms, data, xlev = object[[paste(what,"xlevels",sep=".")]])           
  ## model design matrix y and w 
  X <- model.matrix(Terms, data, contrasts = object$contrasts)
  y <- object[[paste(what,"lp",sep=".")]] 
  w <- object[[paste(what,"wt",sep=".")]] 
  ## leave for future checks
  #  aN <- dim(newdata)[1]
  #zeros <- rep(0,aN)
  #ones <- rep(1,aN)
  #yaug <- as.vector(c(y,zeros))
  #waug <- as.vector(c(w,zeros))
  ## for keeping only the original data
  onlydata <- data$source == "data" # TRUE or FALSE
  ## whether additive terms are involved in the fitting 
  smo.mat <- object[[paste(what,"s",sep=".")]]
  ## if offset take it out from fitting
  if (!is.null(off.num))
    y <- (y - offsetVar[onlydata])
  ## if smoothing 
  if (!is.null(smo.mat))
  {
    n.smooths <- dim(smo.mat)[2]
    y <- (y - smo.mat %*% rep(1, n.smooths))
  }
  ## refit the model
  refit <- lm.wfit(X[onlydata,  , drop = FALSE], y, w)
  ## ckeck the residuals if they are zero
  ##if (any(abs(resid(refit))>1e-005)) 
  if (abs(sum(resid(refit)))>1e-001||abs(sum(coef(object, what=what)-coef(refit), na.rm=TRUE))>1e-005)
    warning(paste("There is a discrepancy  between the original and the re-fit",
                  " \n used to achieve 'safe' predictions \n ", sep = "" ))  
  ## this is disturbing fit and refit have different coefficients  why?
  ## fit <- lm.wfit(X, yaug, waug)
  ## get the coefficients
  coef <- refit$coef         ## save the coefficints
  nX <- dimnames(X)        ## the names of rows and columns 
  rownames <- nX[[1]][!onlydata] ## only the newdata rows
  nrows <- sum(!onlydata)     ## the number of rows in the new data
  nac <- is.na(coef)        ## whether they are NA in coefficients
  assign.coef <- attr(X, "assign")  ## X is a matrix
  collapse <- type != "terms"## !collapse is for whether type is not "terms"     
  Xpred <- X[!onlydata,]
  Xpred <- matrix(Xpred, nrow=nrows) # I think this probably is not needed sinse allready a matrix
  # I will check this later      
  if (!collapse)       ## whether type=="terms" 
  { 
    aa <- attr(X, "assign")
    ll <- attr(Terms, "term.labels")
    if (attr(Terms, "intercept") > 0)  ll <- c("(Intercept)", ll)
    aaa <- factor(aa, labels = ll)
    asgn <- split(order(aa), aaa)
    hasintercept <- attr(Terms, "intercept") > 0
    p <- refit$qr$rank  
    p1 <- seq(len = p)
    piv <- refit$qr$pivot[p1]
    if (hasintercept) 
    {
      asgn$"(Intercept)" <- NULL
      avx <- colMeans(X[onlydata, ])
      termsconst <- sum(avx[piv] * coef[piv])
    }
    # TT <- sum(onlydata)
    # xbar <- drop(array(1/TT, c(1, TT)) %*% X[onlydata, !nac])
    nterms <- length(asgn)
    #    if (nterms > 0) 
    # define the prediction matrix
    pred <- matrix(ncol = nterms, nrow = nrows)
    dimnames(pred) <- list(rownames(newdata), names(asgn))
    #          if (se.fit ) 
    #          {
    #              ip <- matrix(ncol = nterms, nrow = NROW(X))
    #              dimnames(ip) <- list(rownames(X), names(asgn))
    #              Rinv <- qr.solve(qr.R(obj[[paste(what,"qr",sep=".")]])[p1, p1])
    #          }
    if (hasintercept) 
      Xpred <- sweep(Xpred, 2, avx)
    unpiv <- rep.int(0, NCOL(Xpred))
    unpiv[piv] <- p1
    for (i in seq(1, nterms, length = nterms)) 
    {
      iipiv <- asgn[[i]]
      ii <- unpiv[iipiv]
      iipiv[ii == 0] <- 0
      pred[, i] <- if (any(iipiv > 0)) # ms Thursday, May 1, 2008 at 10:12
        Xpred[, iipiv, drop = FALSE] %*% coef[iipiv]
      else 0
      #              if (se.fit ) 
      #                ip[, i] <- if (any(iipiv > 0)) 
      #                  as.matrix(X[, iipiv, drop = FALSE] %*% Rinv[ii, 
      #                    , drop = FALSE])^2 %*% rep.int(1,p)
      #                else 0
    }
    attr(pred, "constant") <- if (hasintercept)  termsconst
    else 0     
    #   Xpred <- Xpred - outer(rep(1, sum(!onlydata)), avx)
    if (!is.null(terms)) 
    {   
      pred <- pred[, terms, drop = FALSE]
      #   if (se.fit) 
      #     ip <- ip[, terms, drop = FALSE]
    }
  } ## end if for terms 
  else  ## if type is not terms but "link" or "response" 
  { 
    pred <- drop(Xpred[, !nac, drop = FALSE] %*% coef[!nac])
    if (!is.null(off.num) && collapse)
      pred <- pred + offsetVar[!onlydata]   
  }
  ## 
  ## now the smoothing part
  ##
  if (!is.null(smo.mat))
  {
    #    cat("new prediction", "\n")
    smooth.labels <- dimnames(smo.mat)[[2]]       ## getting the labels i.e. "pb(Fl)" "pb(A)"
    pred.s <- array(0, c(nrows, n.smooths), list(names(pred), 
                                                 dimnames(smo.mat)[[2]])) ## creating the prediction matrix 
    # smooth.labels[smooth.labels%in%colnames(X)]    
    # smooth.wanted <- smooth.labels[match(smooth.labels, colnames(X), 0) > 0] 
    ## getting the smoothing call
    smooth.calls <- lapply(m[smooth.labels], attr, "call") # i.e $`pb(Fl)`
    #     gamlss.pb(data[["pb(Fl)"]], z, w)
    data <- subset(m, onlydata, drop=FALSE)        ## get the  original data
    attr(data, "class") <- NULL                                   ## note that m is the data.frame with all data 
    new.m <- subset(m, !onlydata, drop=FALSE)       ## get the new data
    attr(new.m, "class") <- NULL
    residuals <-  if (!is.null(off.num)) object[[paste(what,"wv",sep=".")]] - object[[paste(what,"lp",sep=".")]]+offsetVar[onlydata]
    else object[[paste(what,"wv",sep=".")]] - object[[paste(what,"lp",sep=".")]]
    for(TT in smooth.labels)
    { 
      if (is.matrix(m[[TT]])) # the problem is that for some smoother the m[[TT]] is a matrix (for example pvc())
      { # MS 27-6-11         # in this case  we have to protect the dim attributes of data[[tt]]
        nm <- names(attributes(m[[TT]])) # first we get the names of all attributes 
        attributes(data[[TT]]) <- attributes(m[[TT]])[nm[-c(1,2)]]# then we pass all but
      }                                 # 1 and 2 i.e. dim and names
      else   attributes(data[[TT]]) <- attributes(m[[TT]])
      Call <- smooth.calls[[TT]] # 
      Call$xeval <- substitute(new.m[[TT]], list(TT = TT))
      z <- residuals + smo.mat[, TT]
      # debug(gamlss.pvc)
      pred.s[, TT] <- eval(Call)
    }
    if(type == "terms")
    {
      # pred[, smooth.wanted] <- pred[, smooth.wanted] + pred.s[, smooth.wanted]
      pred[, smooth.labels] <- pred[, smooth.labels] + pred.s[, smooth.labels]
    }  
    else pred <- drop(pred + pred.s %*% rep(1, n.smooths)) 
  }
  if(type == "response") 
  {
    FAM <- eval(object$call$family)# 
    if (!is(FAM,"gamlss.family"))
    {
      FAM <- family(object)[1]
    }
    # else
    # { 
    FAM <- as.gamlss.family(FAM)# this should get a gamlss family but not alway
    pred <- FAM[[paste0(what,".linkinv")]](pred) 
  }  
  pred
}
################################################################################
################################################################################
################################################################################
################################################################################

