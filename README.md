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

# fit classifcation tree using rpart
rpart_fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
# design matrix
X_rpart <- treelm(rpart_fit)

# fit classifcation tree using conditional inference trees
ctree_fit <- ctree(Kyphosis ~ Age + Number + Start, data = kyphosis)
# design matrix
X_ctree <- treelm(ctree_fit)
```
### Regression tree predictions
```
library(mlbench)
library(rpart)
data("BostonHousing")

# fit rpart model to Boston Housing data
fit_rpart <- rpart(medv ~ ., data = BostonHousing,  cp = 0.001)

# get design matrix
X <- as.data.frame(treelm(fit_rpart))
y <- BostonHousing$medv

# fit linear model using design matrix from treelm function
fit_lm = lm(y~.,cbind(y,X))

# compare predictions from rpart and linear model
all.equal(predict(fit_rpart,newdata = BostonHousing),predict(fit_lm,newdata = X))
```

