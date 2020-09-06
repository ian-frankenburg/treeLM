# treeLM
R package to parse CART objects to linear models

## Installation
```
library(devtools)
install_github("ian-frankenburg/treeLM")
```

## Examples

### Classification tree parsing
```
library(treeLM)
library(rpart)
library(partykit)

rpart_fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
X_rpart <- treelm(fit)

ctree_fit <- ctree(Kyphosis ~ Age + Number + Start, data = kyphosis)
X_ctree <- treelm(fit)
```
### Regression tree predictions
```
library(mlbench)
library(rpart)
data("BostonHousing")
fit_rpart <- rpart(medv ~ ., data = BostonHousing,  cp = 0.001)
X <- as.data.frame(treelm(fit_rpart))
y <- BostonHousing$medv
fit_lm = lm(y~.,cbind(y,X))
all.equal(predict(fit_rpart,newdata = BostonHousing),predict(fit_lm,newdata = X))
```

