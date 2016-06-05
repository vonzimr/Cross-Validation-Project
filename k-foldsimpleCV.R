
library(Metrics)
source("models.r")

#Note that I'm assuming the columns of the dataframe are labeled 'x' and 'y',
#There's probably a better way of doing this but for now just make sure
#the data frame you input reflects that.
options(warn=-1)

trSet <- function(df, te){
    unorderedTrain <- df[!(df$x %in% te$x), ]
    return(unorderedTrain[order(unorderedTrain$x, decreasing=FALSE),])
}
simpleCV <- function(x, y, k, modelF, plots=FALSE){
  df <- data.frame(x, y)
  shuffledDF <- vector(length=k)
  testSets  <- vector(length=k)
  trainSets <- vector(length=k)
  models <- vector(length=k)
  errors <- double(length=k)

  shuffledDF <- df[sample(nrow(df)),]
  testSets <- split(shuffledDF,1:k)
  trainSets <- lapply(testSets, function(x){return(trSet(df, x))})
  models <- lapply(trainSets ,function(df){modelF(df$x, df$y)})
  errors <- sapply(1:k, function(i){mse(models[[i]](testSets[[i]]$x), testSets[[i]]$y)}) 
  return(data.frame(mean(errors), var(errors)))

}
simpleCV2 <- function(x, y, k, createModelFunction, plots=TRUE){
  df <- data.frame(x, y)
  #Set up the Plot Layout
  if(plots){
    plot.new()
    layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
  }
  #These are just convenience variables for the x and y axis range so it looks prettier
  xl <- c(min(df$x, na.rm=TRUE), max(df$x,na.rm=TRUE))
  yl <- c(min(df$y,na.rm=TRUE), max(df$y,na.rm=TRUE))
  
  #Split the data frame into the number of folds specified by k input
  shuffledDF <- df[sample(nrow(df)),]
  folds <- split(shuffledDF,1:k)
  #Initialize an error vector so we can find the mean and standard deviation
  error <- double(length=k)
  #Start calculating folds
  for(i in 1:length(folds)){
    fold <- folds[[i]]
    test <- fold[order(fold$x, decreasing=FALSE),]
    
    #Need to order the train set so that it plays nicely with the model
    unorderedTrain <- df[!(df$x %in% fold$x), ]
    train <- unorderedTrain[order(unorderedTrain$x, decreasing=FALSE),]
    
    #Use the model function to change the type of model being used!
    modelF <- createModelFunction(train$x, train$y)
    

    #add error of current fold
    error[i] <- mse(test$y[!is.na(test$y)], modelF(test$x[!is.na(test$x)]))

    #Nice looking plots
    if(plots){
      plot(df, main="Approximation of Full Data Set",xlim=xl, ylim=yl)
      lines(sort(df$x), modelF(sort(df$x)))
      plot(train, main=sprintf("Train set %s", i),xlim=xl, ylim=yl)
      lines(sort(df$x), modelF(sort(df$x)))
      plot(test, main=sprintf("Test Fold %s with Error: %f", i, error[i]),xlim=xl, ylim=yl)
      lines(sort(df$x), modelF(sort(df$x)))
    }
  }
  return(data.frame(mean(error), var(error)))
}




simpleModelSelection <- function(x, y, k, plots=FALSE){
  models <- c(expModel, linModel, quadModel, linterpModel)
  modelSelection <- c("Exponential", "Linear", "Quadratic", "Interpolation")
  modelFits <- sapply(models, FUN=function(model){simpleCV(x, y, k, model, plots)})
  return(data.frame(modelSelection, t(modelFits)))
}


######################################
#Investigating the variance of K-fold#
#Cross Validation from k=2 to n-1    #
######################################
plot.new()
par(mfrow=c(1,1))
#Create a simple slightly random linear set of points
l <- 10
B <- 10
x <- seq(0, 5, (5)/l)
y <- as.numeric(lapply(x, FUN=function(xi){return(xi + (runif(1, 0, 1)))}))
plot(x,y)
#Check that a linear model is most appropriate (as it should be)
simpleModelSelection(x, y, 10)

errors <- vector(length=(l-2))
for(k in 2:(l-1)){
  error <- vector(length=(B))
  #Create a simple slightly random set of points
  x <- seq(0, 5, (5)/l)
  y <- as.numeric(lapply(x, FUN=function(xi){return(xi + (runif(1, 0, .1)))}))
  for(i in 1:B){
    error[i] <- as.numeric(simpleCV(x, y, k, linModel,  plots=FALSE)$mean.error)
  }
  errors[k-1] <- mean(error)
}
plot(errors, ylab ="Variance of Fold", xlab="Fold", main="Errors for each fold in Cross Validation of Linear Model")

#Running Cross Validation on Errors
x <- 1:length(errors)

simpleModelSelection(x, errors, 10)
simpleCV(x, errors, 10, linModel)


####################################
#Accuracy of Classication of Model #
####################################
l <- 100
B <- 100
x <- seq(0, 5, (5)/l)
y <- as.numeric(lapply(x, FUN=function(xi){return(xi + (runif(1, 0, 1)))}))
#We are aware that this should be classified as a linear model
accuracyEstimate <- function(k){
    select <- simpleModelSelection(x, y, k)
    classification <- select[select$mean.error == min(as.numeric(select$mean.error)),]$modelSelection
    return(as.numeric(classification == "Linear"))
}
errorK <- double(l-2)
for(i in 2:(l-1)){
    errorK[i-1] <- mean(replicate(100, accuracyEstimate(i)))
}                                                            


######################################
#Example Output and Demonstration of #
#Cross Validation for Model Selection#
######################################

attach(mtcars)
plot(mtcars)

#Try Plotting Disp and Wt together
plot.new()
par(mfrow=c(1,1))
plot(disp, wt)

#Notice that the linear model is actually a more accurate fit. The cluster of values in the corner would
#make other models possess a less accurate fit
simpleModelSelection(disp, wt, 10)
#Take a closer look
simpleCV(disp, wt, 5, expModel)
simpleCV(disp, wt, 5, linModel)


