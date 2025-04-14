#n=20,p=10,rho=0

set.seed(12345)
library(mvtnorm)
n<-50
mu<-rep(0,2)
rho<-0
Sigma <- matrix(c(1,rho,rho,1),2,2,byrow = T)

#dim(Sigma)
#Sigma


X1<-matrix(c(NA),n,2,byrow = T)
for (i in 1:n) { for(j in 1:2){
  X1[i,]<- rmvnorm(1,mu,Sigma)
}
  
}

#hist(X1)


X2<-matrix(c(NA),n,2,byrow = T)
for (i in 1:n) { for(j in 1:2){
  X2[i,]<- rmvnorm(1,mu,Sigma)
}
  
}




X3<-matrix(c(NA),n,2,byrow = T)
for (i in 1:n) { for(j in 1:2){
  X3[i,]<- rmvnorm(1,mu,Sigma)
}
  
}



X4<-matrix(c(NA),n,2,byrow = T)
for (i in 1:n) { for(j in 1:2){
  X4[i,]<- rmvnorm(1,mu,Sigma)
}
  
}



X5<-matrix(c(NA),n,2,byrow = T)
for (i in 1:n) { for(j in 1:2){
  X5[i,]<- rmvnorm(1,mu,Sigma)
}
  
}



X<- cbind(X1,X2,X3,X4,X5)
dim(X)
beta<-c(rep(0,2),-1.5,0,rep(2,4),rep(0,2))
beta1<-c(beta[1],beta[2]) 
beta2<-c(beta[3],beta[4]) 
beta3<- c(beta[5],beta[6])
beta3
beta4<- c(beta[7],beta[8])
y<-c()
for (i in 1:n) {y[i]<- rnorm(1,t(X[i,])%*%beta,1)

}

library(R2WinBUGS)
data <- list (n=n,X1=X1,X2=X2,X3=X3,X4=X4,X5=X5,y=y)
inits<- function(){
  list(beta1=c(0.1,0.2),beta2=c(-1.45,0.5),beta3=c(2.01,2.24),beta4=c(2.23,2.11),beta5=c(0.05,0.04),k=c(0.3792589886,0.9000829897,0.3894594,0.5986663, 0.9641334))
}

ifelse(t(beta2)%*%t(X2)%*%X2%*%beta2>log(5),1,0)
ifelse(t(beta4)%*%t(X4)%*%X4%*%beta4>log(5),1,0)

Modi1.sim <- bugs(data, inits, model.file = "C:/Users/Sayantan/Documents/Modified Group Horseshoe(tun) n=20,p=10.bug",
                  parameters = c("beta1", "beta2", "beta3","beta4","beta5"),
                  n.chains = 1, n.iter = 10000,n.sims = 3000,  bugs.directory = "C:/Users/Sayantan/Downloads/winbugs143_unrestricted/winbugs14_full_patched/WinBUGS14/",DIC = FALSE)

Modi2.sim <- bugs(data, inits, model.file = "C:/Users/Sayantan/Documents/Usual group Horseshoe(tun) n=20,p=10.bug",
                  parameters = c("beta1", "beta2", "beta3","beta4","beta5"),
                  n.chains = 1, n.iter = 10000,n.sims = 3000, bugs.directory = "C:/Users/Sayantan/Downloads/winbugs143_unrestricted/winbugs14_full_patched/WinBUGS14/",DIC = FALSE)




print(Modi1.sim)
print(Modi2.sim)

betahat<- solve(t(X)%*%X)%*%t(X)%*%y
betahat

betamodi1hat<- t(Modi1.sim$mean$beta1)
length(betamodi1hat)
betamodiest1<-rbind(betamodi1hat[,1],betamodi1hat[,2])


betamodi2hat<- t(Modi1.sim$mean$beta2)

betamodiest2<-rbind(betamodi2hat[,1],betamodi2hat[,2])


betamodi3hat<- t(Modi1.sim$mean$beta3)

betamodiest3<-rbind(betamodi3hat[,1],betamodi3hat[,2])


betamodi4hat<- t(Modi1.sim$mean$beta4)

betamodiest4<-rbind(betamodi4hat[,1],betamodi4hat[,2])


betamodi5hat<- t(Modi1.sim$mean$beta5)

betamodiest5<-rbind(betamodi5hat[,1],betamodi5hat[,2])


betamodiest<- rbind(betamodiest1,betamodiest2,betamodiest3,betamodiest4,betamodiest5)

yhatmodi1<-X%*%betamodiest

MSEmodi1<- (t(y-yhatmodi1)%*%(y-yhatmodi1))/length(y)
MSEmodi1

BSEmodi<-sqrt(t(beta-betamodiest)%*%(beta-betamodiest)) 
BSEmodi
betausu1hat<- t(Modi2.sim$mean$beta1)
betausuest1<-rbind(betausu1hat[,1],betausu1hat[,2])


betausu2hat<- t(Modi2.sim$mean$beta2)
betausuest2<-rbind(betausu2hat[,1],betausu2hat[,2])


betausu3hat<- t(Modi2.sim$mean$beta3)
betausuest3<-rbind(betausu3hat[,1],betausu3hat[,2])


betausu4hat<- t(Modi2.sim$mean$beta4)
betausuest4<-rbind(betausu4hat[,1],betausu4hat[,2])


betausu5hat<- t(Modi2.sim$mean$beta5)
betausuest5<-rbind(betausu5hat[,1],betausu5hat[,2])


betausuest<-rbind(betausuest1,betausuest2,betausuest3,betausuest4,betausuest5)

yhatusus<-X%*%betausuest
MSEusu1<- (t(y-yhatusus)%*%(y-yhatusus))/length(y)
MSEusu1

BSEusu<-sqrt(t(beta-betausuest)%*%(beta-betausuest))
BSEusu

#n=20,p=10,rho=0

set.seed(12345)
library(mvtnorm)
n<-20
mu<-rep(0,2)
Sigma <- matrix(c(NA),2,2,byrow = T)
for (i in 1:2) { for(j in 1:2){Sigma[i,j]<- 0^(abs(i-j))}
  
}
dim(Sigma)
Sigma


X1<-matrix(c(NA),n,2,byrow = T)
for (i in 1:n) { for(j in 1:2){
  X1[i,]<- rmvnorm(1,mu,Sigma)
}
  
}
X1



X2<-matrix(c(NA),n,2,byrow = T)
for (i in 1:n) { for(j in 1:2){
  X2[i,]<- rmvnorm(1,mu,Sigma)
}
  
}




X3<-matrix(c(NA),n,2,byrow = T)
for (i in 1:n) { for(j in 1:2){
  X3[i,]<- rmvnorm(1,mu,Sigma)
}
  
}



X4<-matrix(c(NA),n,2,byrow = T)
for (i in 1:n) { for(j in 1:2){
  X4[i,]<- rmvnorm(1,mu,Sigma)
}
  
}



X5<-matrix(c(NA),n,2,byrow = T)
for (i in 1:n) { for(j in 1:2){
  X5[i,]<- rmvnorm(1,mu,Sigma)
}
  
}



X<- cbind(X1,X2,X3,X4,X5)
dim(X)
beta<-c(rep(0,4),rep(2,4),rep(0,2))


y<-c()
for (i in 1:n) {y[i]<- rnorm(1,t(X[i,])%*%beta,1)

}
y
betahat<- solve(t(X)%*%X)%*%t(X)%*%y
betahat

library(R2WinBUGS)
data <- list (n=n,X1=X1,X2=X2,X3=X3,X4=X4,X5=X5,y=y)
inits<- function(){
  list(beta1=c(0.1,0.2),beta2=c(0.15,0.25),beta3=c(2.01,2.24),beta4=c(2.23,2.11),beta5=c(0.05,0.04),k=c(0.3792589886,0.9000829897,0.3894594,0.5986663, 0.9641334),gamma=0.90417050)
}
Modi1.sim <- bugs(data, inits, model.file = "C:/Users/Sayantan/Documents/Modified Group Horseshoe n=20,p=10.bug",
                  parameters = c("beta1", "beta2", "beta3","beta4","beta5"),
                  n.chains = 1, n.iter = 10000,n.sims = 3000,  bugs.directory = "C:/Users/Sayantan/Downloads/winbugs143_unrestricted/winbugs14_full_patched/WinBUGS14/",DIC = FALSE)

Modi2.sim <- bugs(data, inits, model.file = "C:/Users/Sayantan/Documents/Usual group Horseshoe n=20,p=10.bug",
                  parameters = c("beta1", "beta2", "beta3","beta4","beta5"),
                  n.chains = 1, n.iter = 10000,n.sims = 3000, bugs.directory = "C:/Users/Sayantan/Downloads/winbugs143_unrestricted/winbugs14_full_patched/WinBUGS14/",DIC = FALSE)




print(Modi1.sim)
print(Modi2.sim)

betahat<- solve(t(X)%*%X)%*%t(X)%*%y
betahat


#n=20,p=10,rho=0.5

set.seed(12345)
library(mvtnorm)
n<-100
rho<-0.5
mu<-rep(0,2)
Sigma <- matrix(c(1,rho,rho,1),2,2,byrow = T)

dim(Sigma)
Sigma


X1<-matrix(c(NA),n,2,byrow = T)
for (i in 1:n) { for(j in 1:2){
  X1[i,]<- rmvnorm(1,mu,Sigma)
}
  
}




X2<-matrix(c(NA),n,2,byrow = T)
for (i in 1:n) { for(j in 1:2){
  X2[i,]<- rmvnorm(1,mu,Sigma)
}
  
}




X3<-matrix(c(NA),n,2,byrow = T)
for (i in 1:n) { for(j in 1:2){
  X3[i,]<- rmvnorm(1,mu,Sigma)
}
  
}



X4<-matrix(c(NA),n,2,byrow = T)
for (i in 1:n) { for(j in 1:2){
  X4[i,]<- rmvnorm(1,mu,Sigma)
}
  
}



X5<-matrix(c(NA),n,2,byrow = T)
for (i in 1:n) { for(j in 1:2){
  X5[i,]<- rmvnorm(1,mu,Sigma)
}
  
}



X<- cbind(X1,X2,X3,X4,X5)
dim(X)
beta<-c(rep(0,4),rep(2,4),rep(0,2))


y<-c()
for (i in 1:n) {y[i]<- rnorm(1,t(X[i,])%*%beta,1)

}



library(R2WinBUGS)
data_2 <- list (n=n,X1=X1,X2=X2,X3=X3,X4=X4,X5=X5,y=y)
inits<- function(){
  list(beta1=c(0.1,0.2),beta2=c(0.15,0.25),beta3=c(2.01,2.24),beta4=c(2.23,2.11),beta5=c(0.05,0.04),k=c(0.3792589886,0.9000829897,0.3894594,0.5986663, 0.9641334),gamma=0.90417050)
}

#modified GH
Modi1.sim <- bugs(data_2, inits, model.file = "C:/Users/Sayantan/Documents/Modified Group Horseshoe n=20,p=10.bug",
                  parameters = c("beta1", "beta2", "beta3","beta4","beta5"),
                  n.chains = 1, n.iter = 10000,n.sims = 3000, bugs.directory = "C:/Users/Sayantan/Downloads/winbugs143_unrestricted/winbugs14_full_patched/WinBUGS14/",DIC = FALSE)

#Usual GH
Modi2.sim <- bugs(data_2, inits, model.file = "C:/Users/Sayantan/Documents/Usual group Horseshoe n=20,p=10.bug",
                  parameters = c("beta1", "beta2", "beta3","beta4","beta5"),
                  n.chains = 1, n.iter = 10000, n.sims = 3000,bugs.directory = "C:/Users/Sayantan/Downloads/winbugs143_unrestricted/winbugs14_full_patched/WinBUGS14/",DIC = FALSE)




print(Modi1.sim)
print(Modi2.sim)

betahat<- solve(t(X)%*%X)%*%t(X)%*%y
betahat

betamodi1hat<- t(Modi1.sim$mean$beta1)
length(betamodi1hat)
betamodiest1<-rbind(betamodi1hat[,1],betamodi1hat[,2])


betamodi2hat<- t(Modi1.sim$mean$beta2)

betamodiest2<-rbind(betamodi2hat[,1],betamodi2hat[,2])


betamodi3hat<- t(Modi1.sim$mean$beta3)

betamodiest3<-rbind(betamodi3hat[,1],betamodi3hat[,2])


betamodi4hat<- t(Modi1.sim$mean$beta4)

betamodiest4<-rbind(betamodi4hat[,1],betamodi4hat[,2])


betamodi5hat<- t(Modi1.sim$mean$beta5)

betamodiest5<-rbind(betamodi5hat[,1],betamodi5hat[,2])


betamodiest<- rbind(betamodiest1,betamodiest2,betamodiest3,betamodiest4,betamodiest5)
betamodiest
length(betamodiest)
u1<-c()
for (i in 1:length(beta)) { u1[i]<- betamodiest[i]/betahat[i]

}
u1
betagrmodiest<-c(rep(0,4),2.033,1.721,1.8437,2.175,0,0)
yhatmodi1<-X%*%betagrmodiest

MSEmodi1<- (t(y-yhatmodi1)%*%(y-yhatmodi1))/length(y)
MSEmodi1

BSEmodi<-sqrt(t(beta-betagrmodiest)%*%(beta-betagrmodiest)) 
BSEmodi

betausu1hat<- t(Modi2.sim$mean$beta1)
betausuest1<-rbind(betausu1hat[,1],betausu1hat[,2])


betausu2hat<- t(Modi2.sim$mean$beta2)
betausuest2<-rbind(betausu2hat[,1],betausu2hat[,2])


betausu3hat<- t(Modi2.sim$mean$beta3)
betausuest3<-rbind(betausu3hat[,1],betausu3hat[,2])


betausu4hat<- t(Modi2.sim$mean$beta4)
betausuest4<-rbind(betausu4hat[,1],betausu4hat[,2])


betausu5hat<- t(Modi2.sim$mean$beta5)
betausuest5<-rbind(betausu5hat[,1],betausu5hat[,2])


betausuest<-rbind(betausuest1,betausuest2,betausuest3,betausuest4,betausuest5)
betausuest
u2<-c()
for (i in 1:length(beta)) { u2[i]<- betausuest[i]/betahat[i]

}
u2
betagrusuest<-c(rep(0,2),-0.06275,-0.01336,2.005,1.758,1.829,2.1829,0,0)

yhatusus<-X%*%betagrusuest
MSEusu1<- (t(y-yhatusus)%*%(y-yhatusus))/length(y)
MSEusu1

BSEusu<-sqrt(t(beta-betagrusuest)%*%(beta-betagrusuest))
BSEusu



#n=20,p=20,rho=0

set.seed(12345)
library(mvtnorm)
n<-20
mu<-rep(0,2)
Sigma <- matrix(c(NA),2,2,byrow = T)
for (i in 1:2) { for(j in 1:2){Sigma[i,j]<- 0^(abs(i-j))}
  
}
dim(Sigma)
Sigma

#set.seed(12345)
X1<-matrix(c(NA),n,2,byrow = T)
for (i in 1:n) { for(j in 1:2){
  X1[i,]<- rmvnorm(1,mu,Sigma)
}
  
}
#X1


#set.seed(1234)
X2<-matrix(c(NA),n,2,byrow = T)
for (i in 1:n) { for(j in 1:2){
  X2[i,]<- rmvnorm(1,mu,Sigma)
}
  
}
#X2
#dim(X2)


#set.seed(123)
X3<-matrix(c(NA),n,2,byrow = T)
for (i in 1:n) { for(j in 1:2){
  X3[i,]<- rmvnorm(1,mu,Sigma)
}
  
}
#X3

#set.seed(123456)
X4<-matrix(c(NA),n,2,byrow = T)
for (i in 1:n) { for(j in 1:2){
  X4[i,]<- rmvnorm(1,mu,Sigma)
}
  
}
#X4

#set.seed(1234567)
X5<-matrix(c(NA),n,2,byrow = T)
for (i in 1:n) { for(j in 1:2){
  X5[i,]<- rmvnorm(1,mu,Sigma)
}
  
}
#X5


X<- cbind(X1,X2,X3,X4,X5)
dim(X)
beta<-c(0.1,0.2,0.3,0.4,rep(0,6))

set.seed(12345)
y<-c()
for (i in 1:n) {y[i]<- rnorm(1,t(X[i,])%*%beta,1)

}
y
betahat<- solve(t(X)%*%X)%*%t(X)%*%y
betahat

library(R2WinBUGS)
data <- list (n=n,X1=X1,X2=X2,X3=X3,X4=X4,X5=X5,y=y)
inits<- function(){
  list(beta1=c(0.1,0.2),beta2=c(0.15,0.25),beta3=c(2.01,2.24),beta4=c(2.23,2.11),beta5=c(0.05,0.04),k=c(0.3792589886,0.9000829897,0.3894594,0.5986663, 0.9641334))
}

#modified GH
Modi1.sim <- bugs(data, inits, model.file = "C:/Users/Sayantan/Documents/Modified Group Horseshoe n=20,p=10.bug",
                  parameters = c("beta1", "beta2", "beta3","beta4","beta5"),
                  n.chains = 1, n.iter = 10000, bugs.directory = "C:/Users/Sayantan/Downloads/winbugs143_unrestricted/winbugs14_full_patched/WinBUGS14/",DIC = FALSE)

#Usual GH
Modi2.sim <- bugs(data, inits, model.file = "C:/Users/Sayantan/Documents/Usual group Horseshoe n=20,p=10.bug",
                  parameters = c("beta1", "beta2", "beta3","beta4","beta5"),
                  n.chains = 1, n.iter = 10000,n.sims = 3000, bugs.directory = "C:/Users/Sayantan/Downloads/winbugs143_unrestricted/winbugs14_full_patched/WinBUGS14/",DIC = FALSE)




print(Modi1.sim)
print(Modi2.sim)

betahat<- solve(t(X)%*%X)%*%t(X)%*%y
betahat


#n=50,p=40,rho=0
library(mvtnorm)
n<-50
rho<-0
mu<-rep(0,4)
Sigma <- matrix(c(1,rep(rho,4),1,rep(rho,4),1,rep(rho,4),1),4,4,byrow = T)

dim(Sigma)
Sigma

set.seed(123)
x1<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x1[i,]<- rmvnorm(1,mu,Sigma)
}
  
}
x1


set.seed(12345)
x2<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x2[i,]<- rmvnorm(1,mu,Sigma)
}
  
}



set.seed(1234)
x3<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x3[i,]<- rmvnorm(1,mu,Sigma)
}
  
}


set.seed(123456)
x4<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x4[i,]<- rmvnorm(1,mu,Sigma)
}
  
}


set.seed(1234567)
x5<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x5[i,]<- rmvnorm(1,mu,Sigma)
}
  
}

set.seed(12)
x6<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x6[i,]<- rmvnorm(1,mu,Sigma)
}
  
}

set.seed(123408)
x7<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x7[i,]<- rmvnorm(1,mu,Sigma)
}
  
}

set.seed(11111)
x8<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x8[i,]<- rmvnorm(1,mu,Sigma)
}
  
}

set.seed(12132)
x9<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x9[i,]<- rmvnorm(1,mu,Sigma)
}
  
}

set.seed(123232)
x10<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x10[i,]<- rmvnorm(1,mu,Sigma)
}
  
}



X<- cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)
dim(X)
beta<-c(1,2,3,4,rep(0,36))

set.seed(12345)
y<-c()
for (i in 1:n) {y[i]<- rnorm(1,t(X[i,])%*%beta,1)

}
y


library(R2WinBUGS)
data_3 <- list (n=n,x1=x1,x2=x2,x3=x3,x4=x4,x5=x5,x6=x6,x7=x7,x8=x8,x9=x9,x10=x10,y=y)
inits<- function(){
  list(beta1=c(1.1,2.2,3.3,4.4),beta2=c(0.05,0.05,0.1,-0.15),beta3=c(0.01,0.24,0.04,0.21),beta4=c(-0.23,-0.41,-0.04,-0.13),beta5=c(0.05,0.54,0.03,0.17),beta6=c(0.12,0.03,0.01,0.02),
       beta7=c(-0.11,-0.13,0.15,0.21),beta8=c(0.02,0.04,0.03,0.01),beta9=c(-0.05,0.05,0.11,0.13),beta10=c(0.1,0.1,0.1,0.03),k=c(0.3792589886,0.9000829897,0.3894594,0.5986663, 0.9641334,0.03740830, 0.02546928, 0.96748209, 0.62176113, 0.04424659),gamma=0.90417050)
}


Modi1.sim<- bugs(data_3, inits, model.file = "C:/Users/Sayantan/Documents/Modified Group Horseshoe p=40.bug",
                 parameters = c("beta1", "beta2", "beta3","beta4","beta5","beta6","beta7","beta8","beta9","beta10"),
                 n.chains = 1, n.iter = 10000,n.sims = 3000, bugs.directory = "C:/Users/Sayantan/Downloads/winbugs143_unrestricted/winbugs14_full_patched/WinBUGS14/",DIC = FALSE,'debug=TRUE')

Modi1.sim$n.iter
class(Modi1.sim$n.iter)
class(Modi1.sim$n.burnin)
Modi2.sim <- bugs(data_3, inits, model.file = "C:/Users/Sayantan/Documents/Usual Group Horseshoe p=40.bug",
                  parameters = c("beta1", "beta2", "beta3","beta4","beta5","beta6","beta7","beta8","beta9","beta10"),
                  n.chains = 1, n.iter = 10000,n.sims = 3000, bugs.directory = "C:/Users/Sayantan/Downloads/winbugs143_unrestricted/winbugs14_full_patched/WinBUGS14/",DIC = FALSE,'debug=TRUE')


#Modi1.sim <- bugs(data_4, inits, model.file = "C:/Users/Sayantan/Documents/Modified Group Horseshoe p=40.bug",
parameters = c("beta1", "beta2", "beta3","beta4","beta5","beta6","beta7","beta8","beta9","beta10"),
n.chains = 1,   bugs.directory = "C:/Users/Sayantan/Downloads/winbugs143_unrestricted/winbugs14_full_patched/WinBUGS14/",DIC = FALSE,'debug=TRUE')

#Modi2.sim <- bugs(data_4, inits, model.file = "C:/Users/Sayantan/Documents/Usual Group Horseshoe p=40.bug",
parameters = c("beta1", "beta2", "beta3","beta4","beta5"),
n.chains = 1, n.iter = 10000,n.sims = 3000, bugs.directory = "C:/Users/Sayantan/Downloads/winbugs143_unrestricted/winbugs14_full_patched/WinBUGS14/",DIC = FALSE)




print(Modi1.sim)
print(Modi2.sim)

betahat<- solve(t(X)%*%X)%*%t(X)%*%y
betahat


#Example-2
#n=50,p=20,rho=0
library(mvtnorm)
n<-20
rho<-0.5
mu<-rep(0,4)
Sigma <- matrix(c(1,rep(rho,4),1,rep(rho,4),1,rep(rho,4),1),4,4,byrow = T)

dim(Sigma)
Sigma


x1<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x1[i,]<- rmvnorm(1,mu,Sigma)
}
  
}




x2<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x2[i,]<- rmvnorm(1,mu,Sigma)
}
  
}




x3<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x3[i,]<- rmvnorm(1,mu,Sigma)
}
  
}



x4<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x4[i,]<- rmvnorm(1,mu,Sigma)
}
  
}



x5<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x5[i,]<- rmvnorm(1,mu,Sigma)
}
  
}




X<- cbind(x1,x2,x3,x4,x5)
dim(X)
beta<-c(1,2,3,4,0.1,0.2,0.3,0.4,rep(0,12))


y<-c()
for (i in 1:n) {y[i]<- rnorm(1,t(X[i,])%*%beta,1)

}

betahat<- solve(t(X)%*%X)%*%t(X)%*%y


library(R2WinBUGS)
data_4 <- list (n=n,x1=x1,x2=x2,x3=x3,x4=x4,x5=x5,y=y)
inits<- function(){
  list(beta1=c(1.1,2.2,3.3,4.4),beta2=c(0.05,0.15,0.21,0.35),beta3=c(0.01,0.24,0.04,0.21),beta4=c(-0.23,-0.41,-0.04,-0.13),beta5=c(0.05,0.54,0.03,0.17),
       k=c(0.3792589886,0.9000829897,0.3894594,0.5986663, 0.9641334))
}

start_time<- Sys.time()
Modi1.sim<- bugs(data_4,inits,  model.file = "C:/Users/Sayantan/Documents/Modified Group Horseshoe(EB) p=20.bug",
                 parameters = c("beta1", "beta2", "beta3","beta4","beta5"),
                 n.chains = 1, n.iter = 10000,n.sims = 3000, bugs.directory = "C:/Users/Sayantan/Downloads/winbugs143_unrestricted/winbugs14_full_patched/WinBUGS14/",DIC = FALSE)


end_time<- Sys.time()
time<-end_time-start_time
time

start_time<- Sys.time()
Modi2.sim <- bugs(data_4, inits, model.file = "C:/Users/Sayantan/Documents/Usual Group Horseshoe(EB) p=20.bug",
                  parameters = c("beta1", "beta2", "beta3","beta4","beta5"),
                  n.chains = 1, n.iter = 10000,n.sims = 3000, bugs.directory = "C:/Users/Sayantan/Downloads/winbugs143_unrestricted/winbugs14_full_patched/WinBUGS14/",DIC = FALSE)

end_time<- Sys.time()
time<-end_time-start_time
time


print(Modi1.sim)
print(Modi2.sim)


betahat

hist(beta1)
density(Modi1.sim$mean$beta1)
print(Modi1.sim$mean$beta1[1])
length(Modi1.sim$mean$beta1)
betamodi1hat<- t(Modi1.sim$mean$beta1)
betamodiest1<-rbind(betamodi1hat[,1],betamodi1hat[,2],betamodi1hat[,3],betamodi1hat[,4])


betamodi2hat<- t(Modi1.sim$mean$beta2)

betamodiest2<-rbind(betamodi2hat[,1],betamodi2hat[,2],betamodi2hat[,3],betamodi2hat[,4])


betamodi3hat<- t(Modi1.sim$mean$beta3)

betamodiest3<-rbind(betamodi3hat[,1],betamodi3hat[,2],betamodi3hat[,3],betamodi3hat[,4])


betamodi4hat<- t(Modi1.sim$mean$beta4)

betamodiest4<-rbind(betamodi4hat[,1],betamodi4hat[,2],betamodi4hat[,3],betamodi4hat[,4])


betamodi5hat<- t(Modi1.sim$mean$beta5)

betamodiest5<-rbind(betamodi5hat[,1],betamodi5hat[,2],betamodi5hat[,3],betamodi5hat[,4])


betamodiest<- rbind(betamodiest1,betamodiest2,betamodiest3,betamodiest4,betamodiest5)

yhatmodi1<-X%*%betamodiest

MSEmodi1<- (t(y-yhatmodi1)%*%(y-yhatmodi1))/length(y)
MSEmodi1

BSEmodi<- t(beta-betamodiest)%*%(beta-betamodiest)
BSEmodi
betausu1hat<- t(Modi2.sim$mean$beta1)
betausuest1<-rbind(betausu1hat[,1],betausu1hat[,2],betausu1hat[,3],betausu1hat[,4])


betausu2hat<- t(Modi2.sim$mean$beta2)
betausuest2<-rbind(betausu2hat[,1],betausu2hat[,2],betausu2hat[,3],betausu2hat[,4])


betausu3hat<- t(Modi2.sim$mean$beta3)
betausuest3<-rbind(betausu3hat[,1],betausu3hat[,2],betausu3hat[,3],betausu3hat[,4])


betausu4hat<- t(Modi2.sim$mean$beta4)
betausuest4<-rbind(betausu4hat[,1],betausu4hat[,2],betausu4hat[,3],betausu4hat[,4])


betausu5hat<- t(Modi2.sim$mean$beta5)
betausuest5<-rbind(betausu5hat[,1],betausu5hat[,2],betausu5hat[,3],betausu5hat[,4])


betausuest<-rbind(betausuest1,betausuest2,betausuest3,betausuest4,betausuest5)

yhatusus<-X%*%betausuest
MSEusu1<- (t(y-yhatusus)%*%(y-yhatusus))/length(y)
MSEusu1

BSEusu<-t(beta-betausuest)%*%(beta-betausuest)
BSEusu


#n=40,p=20,rh0=0,different group size

library(mvtnorm)
mu1<-rep(0,4)
mu2<-rep(0,3)
mu3<-rep(0,2)
n<-10



rho<-0
Sigma1 <- matrix(c(1,rep(rho,4),1,rep(rho,4),1,rep(rho,4),1),4,4,byrow = T)

Sigma2 <- matrix(c(1,rep(rho,3),1,rep(rho,3),1),3,3,byrow = T)


Sigma3 <- matrix(c(1,rho,rho,1),2,2,byrow = T)


dim(Sigma1)
dim(Sigma2)
dim(Sigma3)
Sigma1
Sigma2
Sigma3


x1<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x1[i,]<- rmvnorm(1,mu1,Sigma1)
}
  
}



x2<-matrix(c(NA),n,3,byrow = T)
for (i in 1:n) { for(j in 1:3){
  x2[i,]<- rmvnorm(1,mu2,Sigma2)
}
  
}



x3<-matrix(c(NA),n,3,byrow = T)
for (i in 1:n) { for(j in 1:3){
  x3[i,]<- rmvnorm(1,mu2,Sigma2)
}
  
}


x4<-matrix(c(NA),n,2,byrow = T)
for (i in 1:n) { for(j in 1:2){
  x4[i,]<- rmvnorm(1,mu3,Sigma3)
}
  
}


x5<-matrix(c(NA),n,2,byrow = T)
for (i in 1:n) { for(j in 1:2){
  x5[i,]<- rmvnorm(1,mu3,Sigma3)
}
  
}


x6<-matrix(c(NA),n,2,byrow = T)
for (i in 1:n) { for(j in 1:2){
  x6[i,]<- rmvnorm(1,mu3,Sigma3)
}
  
}


x7<-matrix(c(NA),n,2,byrow = T)
for (i in 1:n) { for(j in 1:2){
  x7[i,]<- rmvnorm(1,mu3,Sigma3)
}
  
}


x8<-matrix(c(NA),n,2,byrow = T)
for (i in 1:n) { for(j in 1:2){
  x8[i,]<- rmvnorm(1,mu3,Sigma3)
}
  
}

x9<-matrix(c(NA),n,2,byrow = T)
for (i in 1:n) { for(j in 1:2){
  x9[i,]<- rmvnorm(1,mu3,Sigma3)
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

x14<-matrix(c(NA),n,2,byrow = T)
for (i in 1:n) { for(j in 1:2){
  x14[i,]<- rmvnorm(1,mu3,Sigma3)
}
  
}



X<- cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14)
dim(X)
beta<-c(2,2,2,2,rep(0,18),rep(2,4),rep(0.4,4),rep(0,4),rep(1.5,4),rep(0,2))


y<-c()
for (i in 1:n) {y[i]<- rnorm(1,t(X[i,])%*%beta,1)

}
class(y)
beta_11<-c(rep(0.4,4))
ifelse(t(beta_11)%*%t(x11)%*%x11%*%beta_11>2*log(14),1,0)

betahat<- solve(t(X)%*%X)%*%t(X)%*%y
library(R2WinBUGS)

data_5 <- list (n=n,x1=x1,x2=x2,x3=x3,x4=x4,x5=x5,x6=x6,x7=x7,x8=x8,x9=x9,x10=x10,x11=x11,x12=x12,x13=x13,x14=x14,y=y)
inits<- function(){
  list(beta1=c(1.1,2.2,3.3,4.4),beta2=c(0.15,0.25,0.15),beta3=c(0.01,0.24,0.04),beta4=c(-0.23,-0.41),beta6=c(-0.04,-0.13),
       beta5=c(0.05,0.05),beta7=c(-0.12,-0.11),beta8=c(0.02,-0.04),beta10=c(2.12,2.1,1.93,1.88),beta9=c(0.1,0.2),beta11=c(0.41,0.35,0.38,0.42),beta12=c(0.03,0.1,0.3,0.2),beta13=c(1.25,1.49,1.515,1.534),beta14=c(0.03,0.2),k=c(0.3792589886, 0.0002829813,0.9000829897,0.7111138,0.5986663,0.9892697, 0.9641334,0.09788359, 0.9307924, 0.46171233,0.2935979,0.009434765, 0.549972310, 0.887321871))
}
#Modi1.sim <- bugs(data_4, inits, model.file = "C:/Users/Sayantan/Documents/Modified Group Horseshoe p=20.bug",
parameters = c("beta1", "beta2", "beta3","beta4","beta5"),n.iter = 10000,n.thin = 5,
n.chains = 1,as.numeric(n.iter/2), as.numeric(n.thin) , bugs.directory = "C:/Users/Sayantan/Downloads/winbugs143_unrestricted/winbugs14_full_patched/WinBUGS14/",DIC = FALSE,'debug=TRUE')

#Modi GH
Modi1.sim<- bugs(data_5, inits, model.file = "C:/Users/Sayantan/Documents/Modified Group Horseshoe(EB) p=20 different group size.bug",
                 parameters = c("beta1", "beta2", "beta3","beta4","beta5","beta6","beta7","beta8","beta9","beta10","beta11","beta12","beta13","beta14"),
                 n.chains = 1, bugs.directory = "C:/Users/Sayantan/Downloads/winbugs143_unrestricted/winbugs14_full_patched/WinBUGS14/",DIC = FALSE)

Modi1.sim$n.iter
class(Modi1.sim$n.iter)
class(Modi1.sim$n.burnin)
#Usual GH
t_1<-Sys.time()
Modi2.sim <- bugs(data_5, inits, model.file = "C:/Users/Sayantan/Documents/Usual Group Horseshoe(EB) p=20 different group size.bug",
                  parameters = c("beta1", "beta2", "beta3","beta4","beta5","beta6","beta7","beta8","beta9","beta10","beta11","beta12","beta13","beta14"),
                  n.chains = 1,  bugs.directory = "C:/Users/Sayantan/Downloads/winbugs143_unrestricted/winbugs14_full_patched/WinBUGS14/",DIC = FALSE)

t_2<- Sys.time()
t_2-t_1

Modi2.sim$n.iter
Modi2.sim$n.sims

Modi1.sim$n.sims

print(Modi1.sim)
print(Modi2.sim)
betahat
hist(Modi1.sim)


betamodi1hat<- t(Modi1.sim$mean$beta1)
length(betamodi1hat)
betamodiest1<-rbind(betamodi1hat[,1],betamodi1hat[,2],betamodi1hat[,3],betamodi1hat[,4])


betamodi2hat<- t(Modi1.sim$mean$beta2)
length(betamodi2hat)
betamodiest2<-rbind(betamodi2hat[,1],betamodi2hat[,2],betamodi2hat[,3])


betamodi3hat<- t(Modi1.sim$mean$beta3)
length(betamodi3hat)
betamodiest3<-rbind(betamodi3hat[,1],betamodi3hat[,2],betamodi3hat[,3])


betamodi4hat<- t(Modi1.sim$mean$beta4)
length(betamodi4hat)
betamodiest4<-rbind(betamodi4hat[,1],betamodi4hat[,2])


betamodi5hat<- t(Modi1.sim$mean$beta5)

betamodiest5<-rbind(betamodi5hat[,1],betamodi5hat[,2])

betamodi6hat<- t(Modi1.sim$mean$beta6)

betamodiest6<-rbind(betamodi6hat[,1],betamodi6hat[,2])

betamodi7hat<- t(Modi1.sim$mean$beta7)

betamodiest7<-rbind(betamodi7hat[,1],betamodi7hat[,2])

betamodi8hat<- t(Modi1.sim$mean$beta8)

betamodiest8<-rbind(betamodi8hat[,1],betamodi8hat[,2])


betamodiest<- rbind(betamodiest1,betamodiest2,betamodiest3,betamodiest4,betamodiest5,betamodiest6,betamodiest7,betamodiest8)

yhatmodi1<-X%*%betamodiest

MSEmodi1<- (t(y-yhatmodi1)%*%(y-yhatmodi1))/length(y)
MSEmodi1

BSEmodi<- t(beta-betamodiest)%*%(beta-betamodiest)
BSEmodi
betausu1hat<- t(Modi2.sim$mean$beta1)
betausuest1<-rbind(betausu1hat[,1],betausu1hat[,2],betausu1hat[,3],betausu1hat[,4])


betausu2hat<- t(Modi2.sim$mean$beta2)
betausuest2<-rbind(betausu2hat[,1],betausu2hat[,2],betausu2hat[,3])


betausu3hat<- t(Modi2.sim$mean$beta3)
betausuest3<-rbind(betausu3hat[,1],betausu3hat[,2],betausu3hat[,3])


betausu4hat<- t(Modi2.sim$mean$beta4)
betausuest4<-rbind(betausu4hat[,1],betausu4hat[,2])


betausu5hat<- t(Modi2.sim$mean$beta5)
betausuest5<-rbind(betausu5hat[,1],betausu5hat[,2])

betausu6hat<- t(Modi2.sim$mean$beta6)
betausuest6<-rbind(betausu6hat[,1],betausu6hat[,2])

betausu7hat<- t(Modi2.sim$mean$beta7)
betausuest7<-rbind(betausu7hat[,1],betausu7hat[,2])

betausu8hat<- t(Modi2.sim$mean$beta8)
betausuest8<-rbind(betausu8hat[,1],betausu8hat[,2])


betausuest<-rbind(betausuest1,betausuest2,betausuest3,betausuest4,betausuest5,betausuest6,betausuest7,betausuest8)

yhatusus<-X%*%betausuest
MSEusu1<- (t(y-yhatusus)%*%(y-yhatusus))/length(y)
MSEusu1

BSEusu<-t(beta-betausuest)%*%(beta-betausuest)
BSEusu
sqrt(BSEusu)
sqrt(BSEmodi)

#n=40,p=20,rh0=0.5,different group size

library(mvtnorm)
mu1<-rep(0,4)
mu2<-rep(0,3)
mu3<-rep(0,2)
rho<-0.5
n<-40 
Sigma1 <- matrix(c(1,rep(rho,4),1,rep(rho,4),1,rep(rho,4),1),4,4,byrow = T)

Sigma2 <- matrix(c(1,rep(rho,3),1,rep(rho,3),1),3,3,byrow = T)


Sigma3 <- matrix(c(1,rho,rho,1),2,2,byrow = T)



dim(Sigma1)
dim(Sigma2)
dim(Sigma3)

set.seed(12345)
x1<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x1[i,]<- rmvnorm(1,mu1,Sigma1)
}
  
}
x1

set.seed(123456)
x2<-matrix(c(NA),n,3,byrow = T)
for (i in 1:n) { for(j in 1:3){
  x2[i,]<- rmvnorm(1,mu2,Sigma2)
}
  
}
dim(x2)
x2

set.seed(12345)
x3<-matrix(c(NA),n,3,byrow = T)
for (i in 1:n) { for(j in 1:3){
  x3[i,]<- rmvnorm(1,mu2,Sigma2)
}
  
}
x3
set.seed(12345)
x4<-matrix(c(NA),n,2,byrow = T)
for (i in 1:n) { for(j in 1:2){
  x4[i,]<- rmvnorm(1,mu3,Sigma3)
}
  
}
x4
set.seed(123)
x5<-matrix(c(NA),n,2,byrow = T)
for (i in 1:n) { for(j in 1:2){
  x5[i,]<- rmvnorm(1,mu3,Sigma3)
}
  
}
x5
set.seed(1234)
x6<-matrix(c(NA),n,2,byrow = T)
for (i in 1:n) { for(j in 1:2){
  x6[i,]<- rmvnorm(1,mu3,Sigma3)
}
  
}
x6
set.seed(123456)
x7<-matrix(c(NA),n,2,byrow = T)
for (i in 1:n) { for(j in 1:2){
  x7[i,]<- rmvnorm(1,mu3,Sigma3)
}
  
}
x7
set.seed(1234567)
x8<-matrix(c(NA),n,2,byrow = T)
for (i in 1:n) { for(j in 1:2){
  x8[i,]<- rmvnorm(1,mu3,Sigma3)
}
  
}
x8


X<- cbind(x1,x2,x3,x4,x5,x6,x7,x8)
dim(X)
beta<-c(1,2,3,4,rep(0,16))

set.seed(12345)
y<-c()
for (i in 1:n) {y[i]<- rnorm(1,t(X[i,])%*%beta,1)

}
y
betahat<- solve(t(X)%*%X)%*%t(X)%*%y
betahat
library(R2WinBUGS)

data_5 <- list (n=n,x1=x1,x2=x2,x3=x3,x4=x4,x5=x5,x6=x6,x7=x7,x8=x8,y=y)
inits<- function(){
  list(beta1=c(1.1,2.2,3.3,4.4),beta2=c(0.15,0.25,0.15),beta3=c(0.01,0.24,0.04),beta4=c(-0.23,-0.41),beta6=c(-0.04,-0.13),
       beta5=c(0.05,0.05),beta7=c(-0.12,-0.11),beta8=c(0.02,-0.04),k=c(0.3792589886,0.9000829897,0.3894594,0.5986663, 0.9641334,0.09788359, 0.22438915, 0.46171233),gamma=0.90417050)
}
#Modi1.sim <- bugs(data_4, inits, model.file = "C:/Users/Sayantan/Documents/Modified Group Horseshoe p=20.bug",
parameters = c("beta1", "beta2", "beta3","beta4","beta5"),n.iter = 10000,n.thin = 5,
n.chains = 1,as.numeric(n.iter/2), as.numeric(n.thin) , bugs.directory = "C:/Users/Sayantan/Downloads/winbugs143_unrestricted/winbugs14_full_patched/WinBUGS14/",DIC = FALSE,'debug=TRUE')

#Modi GH
Modi1.sim<- bugs(data_5, inits, model.file = "C:/Users/Sayantan/Documents/Modified Group Horseshoe p=20 different group size.bug",
                 parameters = c("beta1", "beta2", "beta3","beta4","beta5","beta6","beta7","beta8"),
                 n.chains = 1, n.iter = 10000,n.sims = 3000, bugs.directory = "C:/Users/Sayantan/Downloads/winbugs143_unrestricted/winbugs14_full_patched/WinBUGS14/",DIC = FALSE)

Modi1.sim$n.iter
class(Modi1.sim$n.iter)
class(Modi1.sim$n.burnin)
#Usual GH
Modi2.sim <- bugs(data_5, inits, model.file = "C:/Users/Sayantan/Documents/Usual Group Horseshoe p=20 different group size.bug",
                  parameters = c("beta1", "beta2", "beta3","beta4","beta5","beta6","beta7","beta8"),
                  n.chains = 1, n.iter = 10000,n.sims = 3000, bugs.directory = "C:/Users/Sayantan/Downloads/winbugs143_unrestricted/winbugs14_full_patched/WinBUGS14/",DIC = FALSE)


Modi2.sim$n.iter

print(Modi1.sim)
print(Modi2.sim)



#BSE Values
#Simulation-1
#rho<-0
mean(sqrt(0.1687263)^2,sqrt(0.5541751)^2,sqrt(0.04594759)^2,sqrt(0.2638369)^2,sqrt(0.4774271)^2)
mean(sqrt(0.2614755),sqrt(0.07606671),sqrt(0.09851366),sqrt(0.0142901),sqrt(0.1362817))
mean(0.1423075^2,0.2671364^2,0.2360682^2,0.251822^2)
mean(0.1449673,0.1055136,0.1082324)

mean(c(0.2026128,0.5957666,0.05829127,0.2562652,0.4687474))

x<-c(sqrt(0.1687263)^2,sqrt(0.5541751)^2,sqrt(0.04594759)^2,sqrt(0.2638369)^2,sqrt(0.4774271)^2)
mean(x)
#simulation-2
mean(c(0.2474249,0.4711816,0.6176071,0.1773851,0.4180477))
mean(c(0.231503,0.4504751,0.592166,0.1878733,0.4143675))

#simulation-4
mean(c(0.5632763^2,0.2197243^2,0.2415181^2,0.4342524^2,0.3391352^2))
mean(c(0.4194286^2,0.3649923^2,0.2110828^2,0.2543125^2,0.2333767^2))


#n=40,p=20,rh0=0,different group size(EB)
system.time(Modi1.sim)
library(mvtnorm)
mu1<-rep(0,4)
mu2<-rep(0,3)
mu3<-rep(0,2)
n<-100 



rho<-0.5
Sigma1 <- matrix(c(1,rep(rho,4),1,rep(rho,4),1,rep(rho,4),1),4,4,byrow = T)

Sigma2 <- matrix(c(1,rep(rho,3),1,rep(rho,3),1),3,3,byrow = T)


Sigma3 <- matrix(c(1,rho,rho,1),2,2,byrow = T)


dim(Sigma1)
dim(Sigma2)
dim(Sigma3)
Sigma1
Sigma2
Sigma3


x1<-matrix(c(NA),n,4,byrow = T)
for (i in 1:n) { for(j in 1:4){
  x1[i,]<- rmvnorm(1,mu1,Sigma1)
}
  
}



x2<-matrix(c(NA),n,3,byrow = T)
for (i in 1:n) { for(j in 1:3){
  x2[i,]<- rmvnorm(1,mu2,Sigma2)
}
  
}



x3<-matrix(c(NA),n,3,byrow = T)
for (i in 1:n) { for(j in 1:3){
  x3[i,]<- rmvnorm(1,mu2,Sigma2)
}
  
}


x4<-matrix(c(NA),n,2,byrow = T)
for (i in 1:n) { for(j in 1:2){
  x4[i,]<- rmvnorm(1,mu3,Sigma3)
}
  
}


x5<-matrix(c(NA),n,2,byrow = T)
for (i in 1:n) { for(j in 1:2){
  x5[i,]<- rmvnorm(1,mu3,Sigma3)
}
  
}


x6<-matrix(c(NA),n,2,byrow = T)
for (i in 1:n) { for(j in 1:2){
  x6[i,]<- rmvnorm(1,mu3,Sigma3)
}
  
}


x7<-matrix(c(NA),n,2,byrow = T)
for (i in 1:n) { for(j in 1:2){
  x7[i,]<- rmvnorm(1,mu3,Sigma3)
}
  
}


x8<-matrix(c(NA),n,2,byrow = T)
for (i in 1:n) { for(j in 1:2){
  x8[i,]<- rmvnorm(1,mu3,Sigma3)
}
  
}

x9<-matrix(c(NA),n,2,byrow = T)
for (i in 1:n) { for(j in 1:2){
  x9[i,]<- rmvnorm(1,mu3,Sigma3)
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

x14<-matrix(c(NA),n,2,byrow = T)
for (i in 1:n) { for(j in 1:2){
  x14[i,]<- rmvnorm(1,mu3,Sigma3)
}
  
}



X<- cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14)
dim(X)
beta<-c(2,2,2,2,rep(0,18),rep(2,4),rep(0.4,4),rep(0,4),rep(1.5,4),rep(0,2))


y<-c()
for (i in 1:n) {y[i]<- rnorm(1,t(X[i,])%*%beta,1)

}

beta_11<-c(rep(0.4,4))
ifelse(t(beta_11)%*%t(x11)%*%x11%*%beta_11>2*log(14),1,0)

betahat<- solve(t(X)%*%X)%*%t(X)%*%y


Start_time<- Sys.time()

library(R2WinBUGS)

data_5 <- list (n=n,x1=x1,x2=x2,x3=x3,x4=x4,x5=x5,x6=x6,x7=x7,x8=x8,x9=x9,x10=x10,x11=x11,x12=x12,x13=x13,x14=x14,y=y)
inits<- function(){
  list(beta1=c(1.1,2.2,3.3,4.4),beta2=c(0.15,0.25,0.15),beta3=c(0.01,0.24,0.04),beta4=c(-0.23,-0.41),beta6=c(-0.04,-0.13),
       beta5=c(0.05,0.05),beta7=c(-0.12,-0.11),beta8=c(0.02,-0.04),beta10=c(2.12,2.1,1.93,1.88),beta9=c(0.1,0.2),beta11=c(0.41,0.35,0.38,0.42),beta12=c(0.03,0.1,0.3,0.2),beta13=c(1.25,1.49,1.515,1.534),beta14=c(0.03,0.2),k=c(0.3792589886, 0.0002829813,0.9000829897,0.3894594,0.5986663,0.9892697, 0.9641334,0.09788359, 0.22438915, 0.46171233,0.2935979,0.009434765, 0.549972310, 0.887321871))
}
#Modi1.sim <- bugs(data_4, inits, model.file = "C:/Users/Sayantan/Documents/Modified Group Horseshoe p=20.bug",
parameters = c("beta1", "beta2", "beta3","beta4","beta5"),n.iter = 10000,n.thin = 5,
n.chains = 1,as.numeric(n.iter/2), as.numeric(n.thin) , bugs.directory = "C:/Users/Sayantan/Downloads/winbugs143_unrestricted/winbugs14_full_patched/WinBUGS14/",DIC = FALSE,'debug=TRUE')

#Modi GH
Modi1.sim<- bugs(data_5, inits, model.file = "C:/Users/Sayantan/Documents/Modified Group Horseshoe(EB) p=20 different group size.bug",
                 parameters = c("beta1", "beta2", "beta3","beta4","beta5","beta6","beta7","beta8","beta9","beta10","beta11","beta12","beta13","beta14"),
                 n.chains = 1, bugs.directory = "C:/Users/Sayantan/Downloads/winbugs143_unrestricted/winbugs14_full_patched/WinBUGS14/",DIC = FALSE)

Modi1.sim$n.iter
class(Modi1.sim$n.iter)
class(Modi1.sim$n.burnin)
#Usual GH
Modi2.sim <- bugs(data_5, inits, model.file = "C:/Users/Sayantan/Documents/Usual Group Horseshoe(EB) p=20 different group size.bug",
                  parameters = c("beta1", "beta2", "beta3","beta4","beta5","beta6","beta7","beta8","beta9","beta10","beta11","beta12","beta13","beta14"),
                  n.chains = 1,  bugs.directory = "C:/Users/Sayantan/Downloads/winbugs143_unrestricted/winbugs14_full_patched/WinBUGS14/",DIC = FALSE)


Modi2.sim$n.iter
Modi2.sim$n.sims

Modi1.sim$n.sims


end_time<- Sys.time()
time<- end_time-Start_time
time

print(Modi1.sim)
print(Modi2.sim)
betahat
hist(Modi1.sim)
system.time(Modi1.sim)


#Example-2



atan(1)
x<-c()
for (i in 1:1000) {x[i]<- 0.5-(atan(i)/3.14)

}
x
plot(x)
