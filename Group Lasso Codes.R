#Example-1
library(mvtnorm)
n<-20
mu<-rep(0,2)
Sigma <- matrix(c(NA),2,2,byrow = T)
for (i in 1:2) { for(j in 1:2){Sigma[i,j]<- 0.0^(abs(i-j))}
  
}

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

#beta<-c(rep(sqrt(log (n)/n),4),rep(2,4),rep(0,2))
beta1<-c(rep(4,4),rep(0,2),rep(0.2,2),rep(0,2))
#set.seed(12345)
y<-c()
for (i in 1:n) {y[i]<- rnorm(1,t(X[i,])%*%beta1,1)

}

library(glmnet)
library(gglasso)
#la<- glmnet(X, y, lambda = 0.1, family='gaussian', alpha=1,intercept = F)
#print(la$beta[,1])

#summary(la$beta[,2])
#length(la$beta)

v.group <- c(1,1,2,2,
             3,3,4,4,5,5)
gr_cv <- cv.gglasso(x=X, y=y, group=v.group, 
                    loss="ls", pred.loss="L2", 
                    intercept = F, nfolds=5)
#x11(); plot(gr_cv)
paste(gr_cv$lambda.min, gr_cv$lambda.1se)


# group lasso
gr = gglasso(X, y, lambda = gr_cv$lambda.min,group = v.group, loss="ls", intercept = F)
print(gr$beta[,1])

gr1 = gglasso(X, y, lambda = gr_cv$lambda.min,group = v.group, loss="wls", intercept = F)
print(gr1$beta[,1])



gr_cv2 <- cv.grpreg(X, y, group=v.group, penalty = "grSCAD",
                    loss="ls", pred.loss="L2", 
                    intercept = F, nfolds=5)
#gr_cv2
grlasso <- grpreg(X,y,group = v.group, penalty = "grLasso", family = "gaussian",lambda = gr_cv$lambda.1se, intercept = F)
print(grlasso$beta[-1,])

grscad = grpreg(X, y, lambda = gr_cv2$lambda.min,group = v.group, penalty = "grSCAD", loss="ls", intercept = F)
grscad
print(grscad$beta[,1])

gr_cv3 <- cv.grpreg(X, y, group=v.group, penalty = "grMCP",
                    loss="ls", pred.loss="L2", 
                    intercept = F, nfolds=5)
grMCP = grpreg(X, y, lambda = gr_cv3$lambda.min,group = v.group, penalty = "grMCP", loss="ls", intercept = F)
grscad
print(grMCP$beta[,1])



cbind(beta1,betalasso,betascad,betamcp)











library(MASS)
data(Birthwt)
X1 <- Birthwt$X
y1 <- Birthwt$bwt
group <- Birthwt$group

cvfit <- cv.grpreg(X1, y1, group)
plot(cvfit)
summary(cvfit)
coef(cvfit) ## Beta at minimum CVE

grlasso <- grpreg(X1,y1,group = group, penalty = "grLasso", family = "gaussian",lambda = gr_cv$lambda.min, intercept = F)
#print(grlasso$beta)
grscad <- grpreg(X1,y1,group = group, penalty = "grSCAD", family = "gaussian",lambda = gr_cv$lambda.lse, intercept = F)
#print(grscad$beta)
grmcp <- grpreg(X1,y1,group = group, penalty = "grMCP", family = "gaussian",lambda = gr_cv$lambda.lse, intercept = F)
betalasso <- print(grlasso$beta[-1,])
betascad <- print(grscad$beta[-1,])
betamcp <- print(grmcp$beta[-1,])
cbind(beta,betalasso,betascad,betamcp)



#Example-2
#set.seed(12345)
library(mvtnorm)
mu1<-rep(0,4)
mu2<-rep(0,3)
mu3<-rep(0,2)
Sigma1 <- matrix(c(NA),4,4,byrow = T)
for (i in 1:4) { for(j in 1:4){Sigma1[i,j]<- 0.5^(abs(i-j))
}
  
}
Sigma2 <- matrix(c(NA),3,3,byrow = T)
for (i in 1:3) { for(j in 1:3){Sigma2[i,j]<- 0.5^(abs(i-j))
}
  
}

Sigma3 <- matrix(c(NA),2,2,byrow = T)
for (i in 1:2) { for(j in 1:2){Sigma3[i,j]<- 0.5^(abs(i-j))
}
  
}


dim(Sigma1)
dim(Sigma2)
dim(Sigma3)

#set.seed(12345)
X1<-matrix(c(NA),40,4,byrow = T)
for (i in 1:40) { for(j in 1:4){
  X1[i,]<- rmvnorm(1,mu1,Sigma1)
}
  
}
#X1

#set.seed(12345)
X2<-matrix(c(NA),40,3,byrow = T)
for (i in 1:40) { for(j in 1:3){
  X2[i,]<- rmvnorm(1,mu2,Sigma2)
}
  
}
#dim(X2)
#X2

#set.seed(123)
X3<-matrix(c(NA),40,3,byrow = T)
for (i in 1:40) { for(j in 1:3){
  X3[i,]<- rmvnorm(1,mu2,Sigma2)
}
  
}
#X3
#set.seed(12345)
X4<-matrix(c(NA),40,2,byrow = T)
for (i in 1:40) { for(j in 1:2){
  X4[i,]<- rmvnorm(1,mu3,Sigma3)
}
  
}
#X4
#set.seed(123)
X5<-matrix(c(NA),40,2,byrow = T)
for (i in 1:40) { for(j in 1:2){
  X5[i,]<- rmvnorm(1,mu3,Sigma3)
}
  
}
#X5
#set.seed(1234)
X6<-matrix(c(NA),40,2,byrow = T)
for (i in 1:40) { for(j in 1:2){
  X6[i,]<- rmvnorm(1,mu3,Sigma3)
}
  
}
#X6
#set.seed(123456)
X7<-matrix(c(NA),40,2,byrow = T)
for (i in 1:40) { for(j in 1:2){
  X7[i,]<- rmvnorm(1,mu3,Sigma3)
}
  
}
#X7
#set.seed(1234567)
X8<-matrix(c(NA),40,2,byrow = T)
for (i in 1:40) { for(j in 1:2){
  X8[i,]<- rmvnorm(1,mu3,Sigma3)
}
  
}
#X8


X<- cbind(X1,X2,X3,X4,X5,X6,X7,X8)
dim(X)
beta<-c(1,2,3,4,rep(0,16))

#set.seed(12345)
y<-c()
for (i in 1:40) {y[i]<- rnorm(1,t(X[i,])%*%beta,1)

}
#y
#betahat<- solve(t(X)%*%X)%*%t(X)%*%y
#betahat

library(glmnet)
library(gglasso)
#la<- glmnet(X, y, lambda = 0.1, family='gaussian', alpha=1,intercept = F)
#print(la$beta[,1])

#summary(la$beta[,2])
#length(la$beta)

v.group <- c(1,1,1,1,2,2,2,3,3,3,4,4,5,5,6,6,7,7,8,8)
gr_cv <- cv.gglasso(x=X, y=y, group=v.group, 
                    loss="ls", pred.loss="L2", 
                    intercept = F, nfolds=5)
#x11(); plot(gr_cv)
#paste(gr_cv$lambda.min, gr_cv$lambda.1se)
library(grpreg)




# group lasso
gr = gglasso(X, y, lambda = gr_cv$lambda.min,group = v.group, loss="ls", intercept = F)
print(gr$beta[,1])
grlasso <- grpreg(X,y,group = v.group, penalty = "grLasso", family = "gaussian",lambda = gr_cv$lambda.lse, intercept = F)
#print(grlasso$beta)
grscad <- grpreg(X,y,group = v.group, penalty = "grSCAD", family = "gaussian",lambda = gr_cv$lambda.min, intercept = F)
#print(grscad$beta)
grmcp <- grpreg(X,y,group = v.group, penalty = "grMCP", family = "gaussian",lambda = gr_cv$lambda.min, intercept = F)
betalasso <- print(grlasso$beta[-1,])
betascad <- print(grscad$beta[-1,])
betamcp <- print(grmcp$beta[-1,])
cbind(beta,betalasso,betascad,betamcp)

#Example-3

library(mvtnorm)
mu1<-rep(0,4)
mu2<-rep(0,3)
mu3<-rep(0,2)

rho<-0.5

Sigma1 <- matrix(c(1,rep(rho,4),1,rep(rho,4),1,rep(rho,4),1),4,4,byrow = T)

Sigma2 <- matrix(c(1,rep(rho,3),1,rep(rho,3),1),3,3,byrow = T)

Sigma3 <- matrix(c(1,rep(rho,2),1),2,2,byrow = T)

dim(Sigma1)
dim(Sigma2)
dim(Sigma3)

Sigma1
#set.seed(12345)
X1<-matrix(c(NA),40,4,byrow = T)
for (i in 1:40) { for(j in 1:4){
  X1[i,]<- rmvnorm(1,mu1,Sigma1)
}
  
}
#X1

#set.seed(12345)
X2<-matrix(c(NA),40,3,byrow = T)
for (i in 1:40) { for(j in 1:3){
  X2[i,]<- rmvnorm(1,mu2,Sigma2)
}
  
}
#dim(X2)
#X2

#set.seed(123)
X3<-matrix(c(NA),40,3,byrow = T)
for (i in 1:40) { for(j in 1:3){
  X3[i,]<- rmvnorm(1,mu2,Sigma2)
}
  
}
#X3
#set.seed(12345)
X4<-matrix(c(NA),40,2,byrow = T)
for (i in 1:40) { for(j in 1:2){
  X4[i,]<- rmvnorm(1,mu3,Sigma3)
}
  
}
#X4
#set.seed(123)
X5<-matrix(c(NA),40,2,byrow = T)
for (i in 1:40) { for(j in 1:2){
  X5[i,]<- rmvnorm(1,mu3,Sigma3)
}
  
}
#X5
#set.seed(1234)
X6<-matrix(c(NA),40,2,byrow = T)
for (i in 1:40) { for(j in 1:2){
  X6[i,]<- rmvnorm(1,mu3,Sigma3)
}
  
}
#X6
#set.seed(123456)
X7<-matrix(c(NA),40,2,byrow = T)
for (i in 1:40) { for(j in 1:2){
  X7[i,]<- rmvnorm(1,mu3,Sigma3)
}
  
}
#X7
#set.seed(1234567)
X8<-matrix(c(NA),40,2,byrow = T)
for (i in 1:40) { for(j in 1:2){
  X8[i,]<- rmvnorm(1,mu3,Sigma3)
}
  
}
#X8


X<- cbind(X1,X2,X3,X4,X5,X6,X7,X8)
dim(X)
beta<-c(1,2,3,4,rep(0,16))

#set.seed(12345)
y<-c()
for (i in 1:40) {y[i]<- rnorm(1,t(X[i,])%*%beta,1)

}
#y
#betahat<- solve(t(X)%*%X)%*%t(X)%*%y
#betahat

library(glmnet)
library(gglasso)
#la<- glmnet(X, y, lambda = 0.1, family='gaussian', alpha=1,intercept = F)
#print(la$beta[,1])

#summary(la$beta[,2])
#length(la$beta)

v.group <- c(1,1,1,1,2,2,2,3,3,3,4,4,5,5,6,6,7,7,8,8)
gr_cv <- cv.gglasso(x=X, y=y, group=v.group, 
                    loss="ls", pred.loss="L2", 
                    intercept = F, nfolds=5)
#x11(); plot(gr_cv)
#paste(gr_cv$lambda.min, gr_cv$lambda.1se)


# group lasso
gr = gglasso(X, y, lambda = gr_cv$lambda.min,group = v.group, loss="ls", intercept = F)
print(gr$beta[,1])




# p>n but G<n case
#Example-4
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
beta<-c(2,2,2,2,rep(0,32),rep(0,4),rep(0.4,4),rep(0,4),rep(0,4),rep(0,4),rep(0.2,4))
length(beta)
y<-c()
for (i in 1:n) {y[i]<- rnorm(1,t(X[i,])%*%beta,1)

}


library(glmnet)
library(gglasso)
#la<- glmnet(X, y, lambda = 0.1, family='gaussian', alpha=1,intercept = F)
#print(la$beta[,1])

#summary(la$beta[,2])
#length(la$beta)

v.group <- c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,6,6,6,6,7,7,7,7,8,8,8,8,9,9,9,9,10,10,10,10,11,11,11,11,12,12,12,12,13,13,13,13,14,14,14,14,15,15,15,15)
gr_cv <- cv.gglasso(x=X, y=y, group=v.group, 
                    loss="ls", pred.loss="L2", 
                    intercept = F, nfolds=5)
#x11(); plot(gr_cv)
#paste(gr_cv$lambda.min, gr_cv$lambda.1se)
# group lasso
gr = gglasso(X, y, lambda = gr_cv$lambda.min,group = v.group, loss="ls", intercept = F)
print(gr$beta[,1])
