#Models
expModel <- function(x, y){
  df <- data.frame(x, y)
  model <- lm(log(y) ~ x, df)
  c1 <- model$coeff[1]
  c2 <- model$coeff[2]
  return(function(x){as.numeric(exp(c1 + c2*x))})
}
linModel <- function(x, y){
  df <- data.frame(x, y)
  model <- lm(y~x, df)
  return(function(x){as.numeric(model$coeff[1] + model$coeff[2]*x)})
}

quadModel <- function(x, y){
  df <- data.frame(x, y)
  model <- lm(y~x + I(x^2), df)
  return(function(x){as.numeric(model$coeff[1] + model$coeff[2]*x + model$coeff[3]*x^2)})
}
polyModel <- function(x, y){
  df <- data.frame(x, y)
  model <- lm(y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5)+I(x^6)+I(x^7), df)
  return(function(x){as.numeric(model$coeff[1] + model$coeff[2]*x + model$coeff[3]*x^2+ model$coeff[4]*x^3 + model$coeff[5]*x^4 + model$coeff[6]*x^5+ model$coeff[7]*x^6+ model$coeff[8]*x^7)})
}
linterpModel <- function(x, y){
model <- approxfun(x, y)
f <- function(x){
  if(is.na(model(x))){
    return(0)
  } else{
    return(model(x))
  }
}
return(function(x){as.numeric(lapply(x, FUN=f))})
}



################################################
#These models instead return their coefficients#
#In order to show perturbation                 #
#of the functions themselves                   #
################################################
linModelc <- function(x, y){
  df <- data.frame(x, y)
  model <- lm(y~x, df)
  return(as.numeric(c(model$coeff[1], model$coeff[2])))
}

quadModelc <- function(x, y){
  df <- data.frame(x, y)
  model <- lm(y~x + I(x^2), df)
  return(as.numeric(c(model$coeff[1], model$coeff[2], model$coeff[3])))
}

cubicModelc <- function(x, y){
  df <- data.frame(x, y)
  model <- lm(y~x+I(x^2)+I(x^3), df)
  return(as.numeric(c(model$coeff[1], model$coeff[2], 
                      model$coeff[3], model$coeff[4])))
}

quartModelc<- function(x, y){
  df <- data.frame(x, y)
  model <- lm(y~x+I(x^2)+I(x^3)+I(x^4), df)
  return(as.numeric(c(model$coeff[1], model$coeff[2], 
                      model$coeff[3], model$coeff[4],
                      model$coeff[5])))
}


quintModelc <- function(x, y){
  df <- data.frame(x, y)
  model <- lm(y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5), df)
  return(as.numeric(c(model$coeff[1], model$coeff[2], 
                      model$coeff[3], model$coeff[4],
                      model$coeff[5], model$coeff[6])))
}
sextModelc <- function(x, y){
  df <- data.frame(x, y)
  model <- lm(y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5)+ +I(x^6), df)
  return(as.numeric(c(model$coeff[1], model$coeff[2], 
                      model$coeff[3], model$coeff[4],
                      model$coeff[5], model$coeff[6],
                      model$coeff[7])))
}

septModelc <- function(x, y){
  df <- data.frame(x, y)
  model <- lm(y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5)+ +I(x^6) + I(x^7), df)
  return(as.numeric(c(model$coeff[1], model$coeff[2], 
                      model$coeff[3], model$coeff[4],
                      model$coeff[5], model$coeff[6],
                      model$coeff[7], model$coeff[8])))
}
