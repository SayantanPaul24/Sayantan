
# p>n but G<n case
#Example-1
#n=50,G=15,p=60

library(mvtnorm)
library(MASS)
mu1<-rep(0,4)

n<-50

rho<-0.5
Sigma1 <- matrix(c(1,rep(rho,4),1,rep(rho,4),1,rep(rho,4),1),4,4,byrow = T)


x1<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x1[i,]<- rmvnorm(1,mu1,Sigma1)
}
  
}
x2<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x2[i,]<- rmvnorm(1,mu1,Sigma1)
}
  
}
x3<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x3[i,]<- rmvnorm(1,mu1,Sigma1)
}
  
}
x4<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x4[i,]<- rmvnorm(1,mu1,Sigma1)
}
  
}
x5<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x5[i,]<- rmvnorm(1,mu1,Sigma1)
}
  
}
x6<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x6[i,]<- rmvnorm(1,mu1,Sigma1)
}
  
}
x7<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x7[i,]<- rmvnorm(1,mu1,Sigma1)
}
  
}
x8<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x8[i,]<- rmvnorm(1,mu1,Sigma1)
}
  
}

x9<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x9[i,]<- rmvnorm(1,mu1,Sigma1)
}
  
}

x10<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x10[i,]<- rmvnorm(1,mu1,Sigma1)
}
  
}

x11<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x11[i,]<- rmvnorm(1,mu1,Sigma1)
}
  
}

x12<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x12[i,]<- rmvnorm(1,mu1,Sigma1)
}
  
}

x13<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x13[i,]<- rmvnorm(1,mu1,Sigma1)
}
  
}

x14<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x14[i,]<- rmvnorm(1,mu1,Sigma1)
}
  
}

x15<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x15[i,]<- rmvnorm(1,mu1,Sigma1)
}
  
}


X<- cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15)

dim(X)
beta<-c(rep(0.2,4),rep(0,32),rep(0,4),rep(1.4,4),rep(0.4,4),rep(0.4,4),rep(0.6,4),rep(1.2,4))
length(beta)
y<-c()
for (i in 1:n) {y[i]<- rnorm(1,t(X[i,])%*%beta,1)

}



library(basad)
#-----------------------------------------------------------
#Example 1: Run the default setting of the Guassian priors
#-----------------------------------------------------------
obj <- basad(x = X, y = y)
print(obj)


library(R2WinBUGS)

data_5 <- list (n=n,x1=x1,x2=x2,x3=x3,x4=x4,x5=x5,x6=x6,x7=x7,x8=x8,x9=x9,x10=x10,x11=x11,x12=x12,x13=x13,x14=x14,x15=x15,y=y)
inits<- function(){
  list(beta1=c(0.19,0.21,0.23,0.24),beta2=c(0.15,0.25,0.15,0.1),beta3=c(0.01,0.24,0.04,-0.1),beta4=c(-0.23,-0.41,-0.1,0.13),beta6=c(-0.04,-0.13,-0.1,-0.3),
       beta5=c(0.05,0.05,-0.-4,-0.13),beta7=c(-0.12,-0.11,-0.1,-0.34),beta8=c(0.02,-0.04,-0.1,-0.3),beta10=c(2.12,2.1,1.93,1.88),beta9=c(0.1,0.2,0.24,0.13),beta11=c(1.41,1.35,1.38,1.42),beta12=c(0.43,0.41,0.43,0.42),beta13=c(0.25,0.49,0.515,0.534),beta14=c(0.61,0.62,0.61,0.62),beta15=c(1.2,1.23,1.19,1.18),k=c(0.3792589886, 0.0002829813,0.9000829897,0.7111138,0.5986663,0.9892697, 0.9641334,0.09788359, 0.9307924, 0.46171233,0.2935979,0.009434765, 0.549972310,0.2653387,0.2256056),gamma=0.887321871)
}


Modi1.sim <- bugs(data_5, inits, model.file = "C:/Users/Sayantan/Documents/Modified Group Horseshoe n=G=13,p=52.bug",
                  parameters = c("beta1", "beta2", "beta3","beta4","beta5","beta6","beta7","beta8","beta9","beta10","beta11","beta12","beta13","beta14","beta15"),
                  n.chains = 1,  bugs.directory = "C:/Users/Sayantan/Downloads/winbugs143_unrestricted/winbugs14_full_patched/WinBUGS14/",DIC = FALSE)

print(Modi1.sim)
betahat1
betahat11
betahat15

ginv(t(X)%*%X)%*%t(X)%*%y
library(pracma)
pinv(t(X)%*%X)%*%t(X)%*%y

solve(t(x1)%*%x1)%*%t(x1)%*%y
solve(t(x10)%*%x10)%*%t(x10)%*%y
solve(t(x11)%*%x11)%*%t(x11)%*%y
solve(t(x14)%*%x14)%*%t(x14)%*%y
solve(t(x15)%*%x15)%*%t(x15)%*%y

#Usual GH
t_1<-Sys.time()
Modi2.sim <- bugs(data_5, inits, model.file = "C:/Users/Sayantan/Documents/Usual Group Horseshoe n=G=13,p=52.bug",
                  parameters = c("beta1", "beta2", "beta3","beta4","beta5","beta6","beta7","beta8","beta9","beta10","beta11","beta12","beta13","beta14","beta15"),
                  n.chains = 1,  bugs.directory = "C:/Users/Sayantan/Downloads/winbugs143_unrestricted/winbugs14_full_patched/WinBUGS14/",DIC = FALSE)

t_2<- Sys.time()
t_2-t_1

Modi2.sim$n.iter
Modi2.sim$n.sims

Modi1.sim$n.sims


print(Modi2.sim)

inits_new<- function(){
  list(beta1=c(2.1,2.2,2.3,2.4),beta2=c(0.15,0.25,0.15,0.1),beta3=c(0.01,0.24,0.04,-0.1),beta4=c(-0.23,-0.41,-0.1,0.13),beta6=c(-0.04,-0.13,-0.1,-0.3),
       beta5=c(0.05,0.05,-0.-4,-0.13),beta7=c(-0.12,-0.11,-0.1,-0.34),beta8=c(0.02,-0.04,-0.1,-0.3),beta10=c(2.12,2.1,1.93,1.88),beta9=c(0.1,0.2,0.24,0.13),beta11=c(0.41,0.35,0.38,0.42),beta12=c(0.03,0.1,0.3,0.2),beta13=c(1.25,1.49,1.515,1.534),beta14=c(0.1,0.2,0.1,-0.2),beta15=c(0.2,0.23,0.19,0.18),k=c(0.3792589886, 0.0002829813,0.9000829897,0.7111138,0.5986663,0.9892697, 0.9641334,0.09788359, 0.9307924, 0.46171233,0.2935979,0.009434765, 0.549972310,0.2653387,0.2256056))
}

Modi3.sim <- bugs(data_5, inits_new, model.file = "C:/Users/Sayantan/Documents/Modified Group Horseshoe(EB) n=G=13,p=52.bug",
                  parameters = c("beta1", "beta2", "beta3","beta4","beta5","beta6","beta7","beta8","beta9","beta10","beta11","beta12","beta13","beta14","beta15"),
                  n.chains = 1,  bugs.directory = "C:/Users/Sayantan/Downloads/winbugs143_unrestricted/winbugs14_full_patched/WinBUGS14/",DIC = FALSE)


print(Modi3.sim)

Modi4.sim <- bugs(data_5, inits_new, model.file = "C:/Users/Sayantan/Documents/Usual Group Horseshoe(EB) n=G=13,p=52.bug",
                  parameters = c("beta1", "beta2", "beta3","beta4","beta5","beta6","beta7","beta8","beta9","beta10","beta11","beta12","beta13","beta14","beta15"),
                  n.chains = 1,  bugs.directory = "C:/Users/Sayantan/Downloads/winbugs143_unrestricted/winbugs14_full_patched/WinBUGS14/",DIC = FALSE)

print(Modi4.sim)


betahat1
betahat11
betahat13
betahat15


library(basad)
#-----------------------------------------------------------
#Example 1: Run the default setting of the Guassian priors
#-----------------------------------------------------------
obj <- basad(x = X, y = y,standardize = F)
dim(X)
length(y)
print(obj)



#Modified Group Horseshoe 1/G < tau < 1

library(mvtnorm)
library(MASS)
mu1<-rep(0,4)

n<-50

rho<-0.5
Sigma1 <- matrix(c(1,rep(rho,4),1,rep(rho,4),1,rep(rho,4),1),4,4,byrow = T)


x1<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x1[i,]<- rmvnorm(1,mu1,Sigma1)
}
  
}
x2<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x2[i,]<- rmvnorm(1,mu1,Sigma1)
}
  
}
x3<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x3[i,]<- rmvnorm(1,mu1,Sigma1)
}
  
}
x4<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x4[i,]<- rmvnorm(1,mu1,Sigma1)
}
  
}
x5<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x5[i,]<- rmvnorm(1,mu1,Sigma1)
}
  
}
x6<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x6[i,]<- rmvnorm(1,mu1,Sigma1)
}
  
}
x7<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x7[i,]<- rmvnorm(1,mu1,Sigma1)
}
  
}
x8<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x8[i,]<- rmvnorm(1,mu1,Sigma1)
}
  
}

x9<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x9[i,]<- rmvnorm(1,mu1,Sigma1)
}
  
}

x10<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x10[i,]<- rmvnorm(1,mu1,Sigma1)
}
  
}

x11<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x11[i,]<- rmvnorm(1,mu1,Sigma1)
}
  
}

x12<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x12[i,]<- rmvnorm(1,mu1,Sigma1)
}
  
}

x13<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x13[i,]<- rmvnorm(1,mu1,Sigma1)
}
  
}

x14<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x14[i,]<- rmvnorm(1,mu1,Sigma1)
}
  
}

x15<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x15[i,]<- rmvnorm(1,mu1,Sigma1)
}
  
}


X<- cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15)

dim(X)
beta<-c(rep(0.2,4),rep(0,32),rep(0,4),rep(1.4,4),rep(0.4,4),rep(0.4,4),rep(0.6,4),rep(1.2,4))
length(beta)
y<-c()
for (i in 1:n) {y[i]<- rnorm(1,t(X[i,])%*%beta,1)

}



library(basad)
#-----------------------------------------------------------
#Example 1: Run the default setting of the Guassian priors
#-----------------------------------------------------------
obj <- basad(x = X, y = y)
print(obj)


library(R2WinBUGS)

data_5 <- list (n=n,x1=x1,x2=x2,x3=x3,x4=x4,x5=x5,x6=x6,x7=x7,x8=x8,x9=x9,x10=x10,x11=x11,x12=x12,x13=x13,x14=x14,x15=x15,y=y)
inits<- function(){
  list(beta1=c(0.19,0.21,0.23,0.24),beta2=c(0.15,0.25,0.15,0.1),beta3=c(0.01,0.24,0.04,-0.1),beta4=c(-0.23,-0.41,-0.1,0.13),beta6=c(-0.04,-0.13,-0.1,-0.3),
       beta5=c(0.05,0.05,-0.-4,-0.13),beta7=c(-0.12,-0.11,-0.1,-0.34),beta8=c(0.02,-0.04,-0.1,-0.3),beta10=c(2.12,2.1,1.93,1.88),beta9=c(0.1,0.2,0.24,0.13),beta11=c(1.41,1.35,1.38,1.42),beta12=c(0.43,0.41,0.43,0.42),beta13=c(0.25,0.49,0.515,0.534),beta14=c(0.61,0.62,0.61,0.62),beta15=c(1.2,1.23,1.19,1.18),k=c(0.3792589886, 0.0002829813,0.9000829897,0.7111138,0.5986663,0.9892697, 0.9641334,0.09788359, 0.9307924, 0.46171233,0.2935979,0.009434765, 0.549972310,0.2653387,0.2256056),gamma=0.0887321871)
}

inits2<- function(){
  list(beta1=c(0.19,0.21,0.23,0.24),beta2=c(0.15,0.25,0.15,0.1),beta3=c(0.01,0.24,0.04,-0.1),beta4=c(-0.23,-0.41,-0.1,0.13),beta6=c(-0.04,-0.13,-0.1,-0.3),
       beta5=c(0.05,0.05,-0.-4,-0.13),beta7=c(-0.12,-0.11,-0.1,-0.34),beta8=c(0.02,-0.04,-0.1,-0.3),beta10=c(2.12,2.1,1.93,1.88),beta9=c(0.1,0.2,0.24,0.13),beta11=c(1.41,1.35,1.38,1.42),beta12=c(0.43,0.41,0.43,0.42),beta13=c(0.25,0.49,0.515,0.534),beta14=c(0.61,0.62,0.61,0.62),beta15=c(1.2,1.23,1.19,1.18),k=c(0.3792589886, 0.0002829813,0.9000829897,0.7111138,0.5986663,0.09892697, 0.9641334,0.09788359, 0.09307924, 0.46171233,0.2935979,0.009434765, 0.549972310,0.2653387,0.2256056),gamma=0.00000887321871)
}

Modi1.sim <- bugs(data_5, inits, model.file = "C:/Users/Sayantan/Documents/Modified Group Horseshoe n=G=13,p=52(truncated1).bug",
                  parameters = c("beta1", "beta2", "beta3","beta4","beta5","beta6","beta7","beta8","beta9","beta10","beta11","beta12","beta13","beta14","beta15"),
                  n.chains = 1,  bugs.directory = "C:/Users/Sayantan/Downloads/winbugs143_unrestricted/winbugs14_full_patched/WinBUGS14/",DIC = FALSE)

print(Modi1.sim)


Modi2.sim <- bugs(data_5, inits2, model.file = "C:/Users/Sayantan/Documents/Modified Group Horseshoe n=G=13,p=52(truncated2).bug",
                  parameters = c("beta1", "beta2", "beta3","beta4","beta5","beta6","beta7","beta8","beta9","beta10","beta11","beta12","beta13","beta14","beta15"),
                  n.chains = 1,  bugs.directory = "C:/Users/Sayantan/Downloads/winbugs143_unrestricted/winbugs14_full_patched/WinBUGS14/",DIC = FALSE)

print(Modi2.sim)
ginv(t(X)%*%X)%*%t(X)%*%y


#Example-2
#n=20,p=G=100

n<-80
p<- 100
X <- matrix(rnorm(n*p) ,n)
dim(X)
beta<-c(2.1,2.1,2.1,2.1,rep(0,96))
length(beta)
y<-c()
for (i in 1:n) {y[i]<- rnorm(1,t(X[i,])%*%beta,1)

}
length(y)

ginv(t(X)%*%X)%*%t(X)%*%y

betahat<- solve(t(X)%*%X)%*%t(X)%*%y 
library(R2WinBUGS)

data_5 <- list (n=n,X=X,y=y)
inits<- function(){
  list(beta=c(2.12,2.142,2.13,2.14,rep(0.05,96)),k=c(0.5987519, 0.0002829813,0.9000829897,0.7111138,0.5986663,0.9892697, 0.9641334,0.09788359, 0.9307924, 0.46171233,0.2935979,0.009434765, 0.549972310,0.2653387,0.59390008, 0.78034238 ,0.73843961, 0.65788396, 0.73359079, 0.83471740,0.08777701, 0.98048961 ,0.74958334, 0.91142024 ,0.02394376, 0.91335045,0.05453937, 0.82809595 ,0.36696674, 0.08382429,0.9576359351, 0.9564620500, 0.1032528438, 0.0057725292, 0.2259635282,0.0093056944, 0.8722481987 ,0.3083402040, 0.0008688964 ,0.8120853566,0.9397325960, 0.9849700383, 0.0347615427, 0.0927684917, 0.9260217282,0.2252049860, 0.8327843202 ,0.5914962029, 0.0065637327 ,0.9409758818,0.8473815629, 0.1361672558, 0.6396646204, 0.5849685614 ,0.9327983150,0.0800248561, 0.0415767402,0.6329866 , 0.0021760426 ,0.7667448587,0.8582285, 0.1897616625, 0.4023003974, 0.0310342860 ,0.1645682351,0.9402776585, 0.6887569441 ,0.6699231144, 0.9957519183 ,0.0870268235,0.0250774037 ,0.3141780216, 0.0025314204, 0.5917758181, 0.0537560180,0.6089492764, 0.0701580407, 0.2779049502, 0.6090092416 ,0.0103744611,0.1828217265, 0.9993677227, 0.7749148475, 0.0033845329, 0.7631574978,0.0091184667, 0.6624656150, 0.0004131953, 0.4649960468, 0.2742158439,0.0543324707 ,0.1465160407 ,0.6352126327 ,0.4216404787, 0.6445653709,0.2476849932, 0.8653635104, 0.6150921590, 0.0536415602, 0.0734435902),gamma=0.887321871)
}


#Usual GH
t_1<-Sys.time()
Modi2.sim <- bugs(data_5, inits, model.file = "C:/Users/Sayantan/Documents/Usual Group Horseshoe G=p=30.bug",
                  parameters = c("beta"),
                  n.chains = 1,  bugs.directory = "C:/Users/Sayantan/Downloads/winbugs143_unrestricted/winbugs14_full_patched/WinBUGS14/",DIC = FALSE)

t_2<- Sys.time()
t_2-t_1
print(Modi2.sim)

print(Modi2.sim$mean$beta[97:100])

Start_time<-Sys.time()
library(basad)
#-----------------------------------------------------------
#Example 1: Run the default setting of the Gaussian priors
#-----------------------------------------------------------
obj <- basad(x = X, y = y,standardize = F)
dim(X)
length(y)
print(obj)

end_time<- Sys.time()
time<- end_time-Start_time
time


n<-80
p<-100
X<- matrix(rnorm(n*p),n,p)
beta<-c(rep(3,5),rep(0,p-5))
length(beta)
y<-c()
for (i in 1:n) {y[i]<- rnorm(1,t(X[i,])%*%beta,1)

}
length(y)
library(basad)
#-----------------------------------------------------------
#Example 1: Run the default setting of the Guassian priors
#-----------------------------------------------------------
obj <- basad(x = X, y = y,standardize = F)
dim(X)
length(y)
print(obj)
betahat<-t(X)%*%solve(X%*%t(X))%*%y
betahat[1:20]
rank(abs(betahat[1:20]))
X%*%betahat-y
betahat2<- solve(t(X)%*%X)%*%t(X)%*%y
betahat2
betahat-betahat2
(t(beta-betahat)%*%(beta-betahat))
(t(beta[6:1000]-betahat[6:1000])%*%(beta[6:1000]-betahat[6:1000]))



X <- matrix(rnorm(1e6 * 100), 1e6, 100)
 dim(X)

 b <- rnorm(100)
 y <- drop(X %*% b) + rnorm(1e6)
 system.time(b <- solve(crossprod(X), crossprod(X, y))) 
 library(parallel)
 detectCores(logical = FALSE)
 