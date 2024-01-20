#  
# # nfolds <- 5
# 
# table(rep_len(1:nfolds, length.out= dim(aids)[1]))
# table(sample(rep_len(1:nfolds, length.out= dim(aids)[1]), replace=FALSE))
# 
 # sample(rep_len(1:nfolds, length.out= dim(aids)[1]), replace=FALSE)
 # sample(rep_len(1:10, length.out= 10), replace=F)
 # sample(rep_len(1:10, length.out= 10), replace=T)
# 
# sapply(sample(rep_len(1:nfolds, length.out= dim(aids)[1]), replace=FALSE),"!=",1:nfolds)
# t(sapply(sample(rep_len(1:nfolds, length.out= dim(aids)[1]), replace=FALSE),"!=",1:nfolds))
# as.data.frame(t(sapply(sample(rep_len(1:nfolds, length.out= dim(aids)[1]), replace=FALSE),"!=",1:nfolds)))
# lapply(as.data.frame(t(sapply(sample(rep_len(1:nfolds, length.out= dim(aids)[1]), replace=FALSE),"!=",1:nfolds))), which)

# data_indexing <- function(data, kfolds=2, bootstrap=FALSE)
# {
#   dD <- dim(data)
#  if (bootstrap)  
#    {
#    mm <-    lapply(as.data.frame(t(sapply(sample(rep_len(1:dD[1], length.out= dD[1]), replace=TRUE),"!=",1:kfolds))), which)    
#    } else 
#    {
# mm <-    lapply(as.data.frame(t(sapply(sample(rep_len(1:kfolds, length.out= dD[1]), replace=FALSE),"!=",1:kfolds))), which) 
#    } 
# mm
# }
