library(devtools)
install_github("ian-frankenburg/treeLM")
library(treeLM)
library(mombf)
library(mlbench)
library(rpart)
data(BostonHousing)
y <- BostonHousing$medv
X <- BostonHousing[,-which(colnames(BostonHousing)=="medv")]
X$chas <- as.numeric(X$chas)
dat <- cbind(y,X)
fit <- rpart(y~.,data = dat,control = c(minbucket=1, minsplit=1, cp=-1))
X=treelm(fit)
library(mombf)
priorCoef <- imomprior(tau=.133)
priorDelta <- modelbbprior(alpha.p=1,beta.p=1)
priorVar <- igprior(.01,.01)
priorCoef <- zellnerprior(tau=nrow(X))
mombf_fit <- modelSelection(y=y, x=X, 
                         enumerate = F,
                         center=FALSE, 
                         scale=FALSE, 
                         niter=10^2,
                         priorCoef=priorCoef, 
                         priorDelta=priorDelta, 
                         priorVar=priorVar,
                         method='Laplace')
pp=postProb(mombf_fit)
as.numeric(pp$modelid[1])
coef(mombf_fit)
