
library(MBSGS)
set.seed(1)
data1 = gen_data_uni(nsample = 120,cor.var=0.5, ntrain = 80)
data1 = normalize(data1)
true_model <- data1$true_model
X <- data1$X
Y<- data1$Y
train_idx <- data1$train_idx
gsize <- data1$gsize
## We recommend to set niter=50000, burnin=10000
## num_update = 100 and niter.update = 100
## to reach convergence
model <- BGLSS(Y[,1],X,niter=50000,burnin=10000,group_size=gsize,
               num_update = 20,niter.update = 20)
model$pos_median!=0
true_model
Y
gsize


set.seed(1)
data1 = gen_data_uni(nsample = 120,cor.var=0.5, ntrain = 80)
data1 = normalize(data1)
true_model <- data1$true_model
X <- data1$X
Y<- data1$Y
train_idx <- data1$train_idx
gsize <- data1$gsize
## We recommend to set niter=50000, burnin=10000
## num_update = 100 and niter.update = 100
## to reach convergence
model <- BGLSS(Y[,1],X,niter=500,burnin=100,group_size=gsize,
               num_update = 20,niter.update = 20)
model$pos_median!=0
true_model



#Code starts here
library(mvtnorm)
n<-50
p<-100
rho<-0.0
mu<-rep(0,p)


onevec <- c(rep(1,p))
Sigma <- (1-rho)*diag(p)+rho*onevec%*%t(onevec)
#Sigma

X<-matrix(c(NA),n,p,byrow = T)
for (i in 1:n) {
  X[i,] <- rmvnorm(1,mu,Sigma)
}
#X
#dim(X)
beta<-c(0.4,0.4,0.4,0.4,0,0.4,0,0,rep(0,42))


y<-c()
for (i in 1:n) {y[i]<- rnorm(1,t(X[i,])%*%beta,1)

}

library(MBSGS)
model <- BGLSS(y,X,niter=50000,burnin=10000,group_size=c(4,3,3,2,2,2,2,2,2,4,4,4,4,2,5,5),
               num_update = 20,niter.update = 20)
model$pos_median!=0




#Example-2


n<-200
p<-50
rho<-0.5
mu<-rep(0,p)


onevec <- c(rep(1,p))
Sigma <- (1-rho)*diag(p)+rho*onevec%*%t(onevec)
#Sigma

X<-matrix(c(NA),n,p,byrow = T)
for (i in 1:n) {
  X[i,] <- rmvnorm(1,mu,Sigma)
}
#X
#dim(X)
beta<-c(0.1,0.2,0.3,0.4,0.4,rep(0,45))


y<-c()
for (i in 1:n) {y[i]<- rnorm(1,t(X[i,])%*%beta,1)

}




library(basad)
#-----------------------------------------------------------
#Example 1: Run the default setting of the Guassian priors
#-----------------------------------------------------------
obj <- basad(x = X, y = y)
print(obj)




#Example 4

n<-50
p<-100
rho<-0.5
mu<-rep(0,p)


onevec <- c(rep(1,p))
Sigma <- (1-rho)*diag(p)+rho*onevec%*%t(onevec)
#Sigma

X<-matrix(c(NA),n,p,byrow = T)
for (i in 1:n) {
  X[i,] <- rmvnorm(1,mu,Sigma)
}
#X
dim(X)
beta<-c(rep(0.4,4),rep(0,96))


y<-c()
for (i in 1:n) {y[i]<- rnorm(1,t(X[i,])%*%beta,1)

}

model <- BGLSS(y,X,niter=50000,burnin=10000,group_size=c(rep(04,05)),
               num_update = 20,niter.update = 20)
model$pos_median!=0

library(basad)
#-----------------------------------------------------------
#Example 1: Run the default setting of the Guassian priors
#-----------------------------------------------------------
obj <- basad(x = X, y = y)
print(obj)


v.group <-c(rep(1,4),rep(2,4),rep(3,4),rep(4,4),rep(5,4),rep(6,4),rep(7,4),rep(8,4),rep(9,4),rep(10,4),
            rep(11,4),rep(12,4),rep(13,4),rep(14,4),rep(15,4),rep(16,4),rep(17,4),rep(18,4),rep(19,4),rep(20,4),
            rep(21,4),rep(22,4),rep(23,4),rep(24,4),rep(25,4))

gr_cv2 <- cv.grpreg(X, y, group= v.group, penalty = "grSCAD",
                    loss="ls", pred.loss="L2", 
                    intercept = F, nfolds=5)


grscad = grpreg(X, y, lambda = gr_cv2$lambda.min,group = v.group, penalty = "grSCAD", loss="ls", intercept = F)
#grscad
print(grscad$beta[,1])

gr_cv3 <- cv.grpreg(X, y, group=v.group, penalty = "grMCP",
                    loss="ls", pred.loss="L2", 
                    intercept = F, nfolds=5)
grMCP = grpreg(X, y, lambda = gr_cv3$lambda.min,group = v.group, penalty = "grMCP", loss="ls", intercept = F)
#grscad
print(grMCP$beta[,1])



#Example 11

n<-50
p<-100
G <- 5
rho<-0.5
mu<-rep(0,p)


U1 <-c()
for (i in 1:n) {
  U1[i] <- rnorm(1)
}

U2 <-c()
for (i in 1:n) {
  U2[i] <- rnorm(1)
}


U3 <-c()
for (i in 1:n) {
  U3[i] <- rnorm(1)
}

U4 <-c()
for (i in 1:n) {
  U4[i] <- rnorm(1)
}

U5 <-c()
for (i in 1:n) {
  U5[i] <- rnorm(1)
}

U6 <-c()
for (i in 1:n) {
  U6[i] <- rnorm(1)
}

U7 <-c()
for (i in 1:n) {
  U7[i] <- rnorm(1)
}


U8 <-c()
for (i in 1:n) {
  U8[i] <- rnorm(1)
}

U9 <-c()
for (i in 1:n) {
  U9[i] <- rnorm(1)
}

U10 <-c()
for (i in 1:n) {
  U10[i] <- rnorm(1)
}

U11 <-c()
for (i in 1:n) {
  U11[i] <- rnorm(1)
}

U12 <-c()
for (i in 1:n) {
  U12[i] <- rnorm(1)
}


U13 <-c()
for (i in 1:n) {
  U13[i] <- rnorm(1)
}

U14 <-c()
for (i in 1:n) {
  U14[i] <- rnorm(1)
}

U15 <-c()
for (i in 1:n) {
  U15[i] <- rnorm(1)
}

U16 <-c()
for (i in 1:n) {
  U16[i] <- rnorm(1)
}

U17 <-c()
for (i in 1:n) {
  U17[i] <- rnorm(1)
}


U18 <-c()
for (i in 1:n) {
  U18[i] <- rnorm(1)
}

U19 <-c()
for (i in 1:n) {
  U19[i] <- rnorm(1)
}

U20 <-c()
for (i in 1:n) {
  U20[i] <- rnorm(1)
}

Z1<- matrix(c(NA),n,5,byrow = T)
Z2<- matrix(c(NA),n,5,byrow = T)
Z3<- matrix(c(NA),n,5,byrow = T)
Z4<- matrix(c(NA),n,5,byrow = T)
Z5<- matrix(c(NA),n,5,byrow = T)

Z6<- matrix(c(NA),n,5,byrow = T)
Z7<- matrix(c(NA),n,5,byrow = T)
Z8<- matrix(c(NA),n,5,byrow = T)
Z9<- matrix(c(NA),n,5,byrow = T)
Z10<- matrix(c(NA),n,5,byrow = T)

Z11<- matrix(c(NA),n,5,byrow = T)
Z12<- matrix(c(NA),n,5,byrow = T)
Z13<- matrix(c(NA),n,5,byrow = T)
Z14<- matrix(c(NA),n,5,byrow = T)
Z15<- matrix(c(NA),n,5,byrow = T)



Z16<- matrix(c(NA),n,5,byrow = T)
Z17<- matrix(c(NA),n,5,byrow = T)
Z18<- matrix(c(NA),n,5,byrow = T)
Z19<- matrix(c(NA),n,5,byrow = T)
Z20<- matrix(c(NA),n,5,byrow = T)
for (i in 1:n) {
  for(j in 1:5)
  {
    Z1[i,j]<- rnorm(1)
  }
}

for (i in 1:n) {
  for(j in 1:5)
  {
    Z2[i,j]<- rnorm(1)
  }
}

for (i in 1:n) {
  for(j in 1:5)
  {
    Z3[i,j]<- rnorm(1)
  }
}

for (i in 1:n) {
  for(j in 1:5)
  {
    Z4[i,j]<- rnorm(1)
  }
}

for (i in 1:n) {
  for(j in 1:5)
  {
    Z5[i,j]<- rnorm(1)
  }
}

for (i in 1:n) {
  for(j in 1:5)
  {
    Z6[i,j]<- rnorm(1)
  }
}

for (i in 1:n) {
  for(j in 1:5)
  {
    Z7[i,j]<- rnorm(1)
  }
}

for (i in 1:n) {
  for(j in 1:5)
  {
    Z8[i,j]<- rnorm(1)
  }
}

for (i in 1:n) {
  for(j in 1:5)
  {
    Z9[i,j]<- rnorm(1)
  }
}

for (i in 1:n) {
  for(j in 1:5)
  {
    Z10[i,j]<- rnorm(1)
  }
}

for (i in 1:n) {
  for(j in 1:5)
  {
    Z11[i,j]<- rnorm(1)
  }
}

for (i in 1:n) {
  for(j in 1:5)
  {
    Z12[i,j]<- rnorm(1)
  }
}

for (i in 1:n) {
  for(j in 1:5)
  {
    Z13[i,j]<- rnorm(1)
  }
}

for (i in 1:n) {
  for(j in 1:5)
  {
    Z14[i,j]<- rnorm(1)
  }
}

for (i in 1:n) {
  for(j in 1:5)
  {
    Z15[i,j]<- rnorm(1)
  }
}

for (i in 1:n) {
  for(j in 1:5)
  {
    Z16[i,j]<- rnorm(1)
  }
}

for (i in 1:n) {
  for(j in 1:5)
  {
    Z17[i,j]<- rnorm(1)
  }
}

for (i in 1:n) {
  for(j in 1:5)
  {
    Z18[i,j]<- rnorm(1)
  }
}

for (i in 1:n) {
  for(j in 1:5)
  {
    Z19[i,j]<- rnorm(1)
  }
}

for (i in 1:n) {
  for(j in 1:5)
  {
    Z20[i,j]<- rnorm(1)
  }
}

X1<- matrix(c(NA),n,5,byrow = T)
X2<- matrix(c(NA),n,5,byrow = T)
X3<- matrix(c(NA),n,5,byrow = T)
X4<- matrix(c(NA),n,5,byrow = T)
X5<- matrix(c(NA),n,5,byrow = T)

X6<- matrix(c(NA),n,5,byrow = T)
X7<- matrix(c(NA),n,5,byrow = T)
X8<- matrix(c(NA),n,5,byrow = T)
X9<- matrix(c(NA),n,5,byrow = T)
X10<- matrix(c(NA),n,5,byrow = T)

X11<- matrix(c(NA),n,5,byrow = T)
X12<- matrix(c(NA),n,5,byrow = T)
X13<- matrix(c(NA),n,5,byrow = T)
X14<- matrix(c(NA),n,5,byrow = T)
X15<- matrix(c(NA),n,5,byrow = T)

X16<- matrix(c(NA),n,5,byrow = T)
X17<- matrix(c(NA),n,5,byrow = T)
X18<- matrix(c(NA),n,5,byrow = T)
X19<- matrix(c(NA),n,5,byrow = T)
X20<- matrix(c(NA),n,5,byrow = T)
for (i in 1:n) {
  for(j in 1:5)
  {
    X1[i,j]<- U1[i]+Z1[i,j]
  }
}

for (i in 1:n) {
  for(j in 1:5)
  {
    X2[i,j]<- U2[i]+Z2[i,j]
  }
}

for (i in 1:n) {
  for(j in 1:5)
  {
    X3[i,j]<- U3[i]+Z3[i,j]
  }
}

for (i in 1:n) {
  for(j in 1:5)
  {
    X4[i,j]<- U4[i]+Z4[i,j]
  }
}

for (i in 1:n) {
  for(j in 1:5)
  {
    X5[i,j]<- U5[i]+Z5[i,j]
  }
}

for (i in 1:n) {
  for(j in 1:5)
  {
    X6[i,j]<- U6[i]+Z6[i,j]
  }
}

for (i in 1:n) {
  for(j in 1:5)
  {
    X7[i,j]<- U7[i]+Z7[i,j]
  }
}

for (i in 1:n) {
  for(j in 1:5)
  {
    X8[i,j]<- U8[i]+Z8[i,j]
  }
}

for (i in 1:n) {
  for(j in 1:5)
  {
    X9[i,j]<- U9[i]+Z9[i,j]
  }
}

for (i in 1:n) {
  for(j in 1:5)
  {
    X10[i,j]<- U10[i]+Z10[i,j]
  }
}

for (i in 1:n) {
  for(j in 1:5)
  {
    X11[i,j]<- U11[i]+Z11[i,j]
  }
}

for (i in 1:n) {
  for(j in 1:5)
  {
    X12[i,j]<- U12[i]+Z12[i,j]
  }
}

for (i in 1:n) {
  for(j in 1:5)
  {
    X13[i,j]<- U13[i]+Z13[i,j]
  }
}

for (i in 1:n) {
  for(j in 1:5)
  {
    X14[i,j]<- U14[i]+Z14[i,j]
  }
}

for (i in 1:n) {
  for(j in 1:5)
  {
    X15[i,j]<- U15[i]+Z15[i,j]
  }
}

for (i in 1:n) {
  for(j in 1:5)
  {
    X16[i,j]<- U16[i]+Z16[i,j]
  }
}

for (i in 1:n) {
  for(j in 1:5)
  {
    X17[i,j]<- U17[i]+Z17[i,j]
  }
}

for (i in 1:n) {
  for(j in 1:5)
  {
    X18[i,j]<- U18[i]+Z18[i,j]
  }
}

for (i in 1:n) {
  for(j in 1:5)
  {
    X19[i,j]<- U19[i]+Z19[i,j]
  }
}

for (i in 1:n) {
  for(j in 1:5)
  {
    X20[i,j]<- U20[i]+Z20[i,j]
  }
}

X<-cbind(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20)
dim(X)


#X
#dim(X)
beta<-c(rep(0.4,5),rep(0.5,5),rep(0.6,5),rep(0,85))


y<-c()
for (i in 1:n) {y[i]<- rnorm(1,t(X[i,])%*%beta,1)

}

model <- BGLSS(y,X,niter=50000,burnin=10000,group_size=c(rep(05,20)),
               num_update = 20,niter.update = 20)
model$pos_median!=0








Sigma <- matrix(c(NA),2,2,byrow = T)
for (i in 1:2) { for(j in 1:2){Sigma[i,j]<- 0^(abs(i-j))}
  
}

X1<-matrix(c(NA),n,2,byrow = T)
for (i in 1:n) { for(j in 1:2){
  X1[i,]<- rmvnorm(1,mu,Sigma)
}
  
}
#X1



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
beta<-c(rep(0,4),rep(0.2,4),rep(0,2))


y<-c()
for (i in 1:n) {y[i]<- rnorm(1,t(X[i,])%*%beta,1)

}
#y
#length(y)



model <- BGLSS(y,X,niter=50000,burnin=10000,group_size=c(rep(2,5)),
               num_update = 20,niter.update = 20)
model$pos_median!=0
true_model
model$pos_mean 
train_idx
gsize
data1
Y
dim(X)

#Example 15
library(mvtnorm)
n<-50
mu<-rep(0,2)
rho<-0.0
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



Z1 <- X1
P_Z1 <- Z1%*%solve(t(Z1)%*%Z1)%*%t(Z1)
Z2 <- (diag(n)-P_Z1)%*%X2 
P_Z2 <- Z2%*%solve(t(Z2)%*%Z2)%*%t(Z2)
Z3 <- (diag(n)-P_Z1-P_Z2)%*%X3
P_Z3 <- Z3%*%solve(t(Z3)%*%Z3)%*%t(Z3)
Z4 <- (diag(n)-P_Z1-P_Z2-P_Z3)%*%X4
P_Z4 <- Z4%*%solve(t(Z4)%*%Z4)%*%t(Z4)
Z5 <- (diag(n)-P_Z1-P_Z2-P_Z3-P_Z4)%*%X5
Z<- cbind(Z1,Z2,Z3,Z4,Z5)




beta<-c(rep(0,4),rep(0,2),rep(1,2),rep(0,2))


y<-c()
for (i in 1:n) {y[i]<- rnorm(1,t(Z[i,])%*%beta,1)

}
model <- BGLSS(y,Z,niter=50000,burnin=10000,group_size=c(rep(02,05)),
               num_update = 20,niter.update = 20)
model$pos_median!=0
