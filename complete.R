corr <- function(directory, threshold = 0) {
  ##threshold is a numeric vector length 1 which indicates the n number of completely observed observations required to compute        the correlation between nitrate and sulfate; default is 0
  
  ##Return a numeric vector of correlation
  
  all <- complete(directory)
  
  if (sum(which(all$nobs>threshold))==0){
    
    NULL
  }
  
  else {
    thresh_index <- as.numeric(all[which(all$nobs>threshold),]$id)
    
    names <- list.files(directory)[thresh_index]
    read <- lapply(paste(directory,"/",names, sep = ""),read.csv)
    
    
    
    return(unlist(lapply(read, function(x){cor(x[,2],x[,3],use="pairwise.complete.obs")})))
  }
}

# 8. What value is printed at end of the following code?
cr <- corr("specdata")                
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

