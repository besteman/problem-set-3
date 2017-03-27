rm(list = ls())

f <- function(theta){
  
  a <- ((n*yBar)/theta)
  
  b <- (n / (log(1-theta) * (1 - theta)))
  
  return(a + b)
  
}

g <-function(theta){
  
  a <- (-n*yBar / theta^2)
  
  b <- (n + n * (log(1 - theta)))
  
  c <-  (1 - theta)^2 * (log(1 - theta))^2
  
  return(a + (b / c))
  
  
}


Y    <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
observedFreq <- c(710, 175, 74, 23, 10, 4, 2, 1, 1)

dataModel <- data.frame(Y, observedFreq)

model <- glm(dataModel , family = poisson(link = "log"))

print(model)

summaryModel <- summary(model)

print(summaryModel)

startPoint <- 0.5

s <- TRUE

n <- 1000

yBar <- ((710 * 1) + (175 * 2) + (74 * 3) + (23 * 4) + (10 * 5) + (4 * 6) + (2 + 7) + (1 + 8) + (1 + 9)) / n
  
while (s == TRUE) {
  
  
                 
    newTheta <- startPoint - f(startPoint) / g(startPoint)
    
    if(startPoint - newTheta < .Machine$double.eps) {
      
      break
      
    }
    
}

print(newTheta)


- g(newTheta)

