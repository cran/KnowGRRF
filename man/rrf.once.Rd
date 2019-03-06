\name{rrf.once}
\alias{rrf.once}
\title{
Feature selection by regularized random forest and compare against full model
}
\description{
Select features using regularized random forest model. Build random forest model either using or not using feature selection. Compare model performance on an independent test set.
}
\usage{
rrf.once(X.train, Y.train, X.test, Y.test, coefReg)
}

\arguments{
  \item{X.train}{
a data frame or matrix (like x) containing predictors for the training set.
}
  \item{Y.train}{
response for the training set. If a factor, classification is assumed, otherwise regression is assumed. If omitted, will run in unsupervised mode.
}
  \item{X.test}{
a data frame or matrix (like x) containing predictors for the test set.
}
  \item{Y.test}{
response for the test set. 
}
  \item{coefReg}{
regularization coefficient chosen for RRF, ranges between 0 and 1. 
}
}

\value{
return a list, including
  \item{perf}{number of feature selected by RRF, performance (AUC or MSE depending on classification or regression) of RF model using all features, performance (AUC or MSE depending on classification or regression) of RF model using selected features}
  \item{FullModel}{RF model built with all features}
  \item{ReducedModel}{RF model built with only selected features}
  \item{featureIndex}{feature index selected by RRF}

}
\references{
Guan, X., & Liu, L. (2018). Know-GRRF: Domain-Knowledge Informed Biomarker Discovery with Random Forests. 
}
\author{
Li Liu, Xin Guan
}

\examples{
##---- Example: regression ----
library(randomForest)

set.seed(1)
X<-data.frame(matrix(rnorm(100*100), nrow=100))
b=seq(0.1, 2.2, 0.2) 
##y has a linear relationship with first 10 variables
y=b[4]*X$X3+b[5]*X$X4+b[6]*X$X5+b[7]*X$X6+b[8]*X$X7+b[9]*X$X8+b[10]*X$X9+b[11]*X$X10 


##split training and test set
X.train=X[1:70,]
X.test=X[71:100,]
y.train=y[1:70]
y.test=y[71:100]

##use RRF to impute regularized coefficients
imp<-randomForest(X.train, y.train)$importance 
coefReg=imp/max(imp) 

rrf.once(X.train, y.train, X.test, y.test, coefReg)

##---- Example: classification ----
y=as.factor(ifelse(y>0, 1, 0)) ##classification
y.train=y[1:70]
y.test=y[71:100]

##use RRF to impute regularized coefficients
imp<-randomForest(X.train, y.train)$importance 
coefReg=imp/max(imp) 

rrf.once(X.train, y.train, X.test, y.test, coefReg)


}

