

#Vector of mean variances
#How many times should CV be repeated
B <- 10
#Size of dataframe
l <- 100
errors <- vector(length=(l-2))
for(k in 2:(l-1)){
  error <- vector(length=(B))
  x <- seq(0, 5, (5)/l)
  y <- as.numeric(lapply(x, FUN=function(xi){return(xi + (runif(1, 0, 5)))}))
  for(i in 1:B){
    error[i] <- as.numeric(simpleCV(x, y, k, expModel,  plots=FALSE)$var.error)
  }
  errors[k-1] <- mean(error)
}
plot(errors, ylab ="Variance of Fold", xlab="Fold", main="Errors for each fold in Cross Validation of Linear Model")