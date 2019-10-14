library(boot)
library(bootstrap)


############### First question part 1
n  <-  nrow(scor)
cov.hat  <-  cov(scor)
eigenvalue.hat  <-  eigen(cov.hat)
theta.hat  <-  eigenvalue.hat[1]/sum(eigenvalue.hat) 

B  <-  50
theta.b  <-  numeric(B)
for (i in 1:B) {
  i   <-   sample(1:n, size = n, replace = TRUE)
  cov.b  <-  cov(scor[i,])
  eigenvalue.b  <-  eigen(cov.b)$values
  theta.b[i]  <-  eigenvalue.b[1]/sum(eigenvalue.b)
}
bias.b  <-  mean(theta.b-theta.hat)
se.b  <-  sd(theta.b)
print(bias.b)
print(se.b)

################ First question part 2
theta.jack <- numeric(n)
for(i in 1:n){
  cov.jack <- cov(scor[-i,])
  eigenvalue.jack <- eigen(cov.jack)$values
  theta.jack[i] <- eigenvalue.jack[1]/sum(eigenvalue.jack)
}
bias.jack <- (n-1)*(mean(theta.jack)-theta.hat)
print(bias.jack)
se.jack <- sqrt((n-1) *mean((theta.jack - mean(theta.jack))^2))
print(se.jack)



################ Second question 
data(scor,package="bootstrap")
theta.boot<-function(scor,i){
  eigenvalue<-eigen(cov(scor[i,],scor[i,]))$values
  eigenvalue[1]/sum(eigenvalue)
}
boot.obj<-boot(scor, theta.boot,R=2000)
print(boot.ci(boot.obj,type = c("perc","bca")))
