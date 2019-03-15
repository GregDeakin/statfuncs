#' @import data.table
varcheck <- function(DT,
                     value,
                     group ) {
     
  requireNamespace("data.table", quietly = TRUE)
  DT <- as.data.table(DT)
  #DT[,(var(value)),by=group][,colMeans(.SD),.SDcols="V1"]
  #exp1[,.(.N*(mean(prop_f1)-mean(DT$value))^2),by=l1][,sum(.SD)/(.N-1),.SDcols="V1"]
  
  within  <- mean(DT[,(var(get(value))),by=get(group)][[2]])
  
  between <- sum(DT[,.(.N*(mean(get(value))-mean(DT[[value]]))^2),by=get(group)][[2]])/(length(unique(DT[[group]]))-1)
  
  return(c(within=within,between=between,ratio=between/within))
  
}
