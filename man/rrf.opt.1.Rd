\name{rrf.opt.1}
\alias{rrf.opt.1}
\title{
KnowGRRF with weights from one knowledge domain
}
\description{
Regularize on the weights to guide RRF feature selection. Weights can from either one knowledge domain, or use statistics-based weights, e.g., p/q value, variable importance, etc. Feature set selected is also based on stability, that is the frequency of selection from multiple runs. Features that are consistently selected from multiple runs will be used in a random forest model, from which AIC and AUC will be calculated to evaluate the model performance. Only AIC will be calculated for regression.
}
\usage{
rrf.opt.1(X.train, Y.train, X.test=NULL, Y.test=NULL, pwr, weight, 
iter=1,total=10, cutoff=0.5)
}
\arguments{
  \item{X.train}{
a data frame or matrix (like x) containing predictors for the training set.
}
  \item{Y.train}{
response for the training set. If a factor, classification is assumed, otherwise regression is assumed. If omitted, will run in unsupervised mode.
}
  \item{X.test}{
an optional data frame or matrix (like x) containing predictors for the test set.
}
  \item{Y.test}{
optional response for the test set. 
}
  \item{pwr}{
Regularization term to adjust the scale of weights. When one domain knowledge is used, this is a single regularization value. Larger regularization will differentiate the importance of variables more significantly. Fewer variables tend to be selected with large pwr. This parameter can be tuned using optimization methods or grid searching with on.aic function.
}
  \item{weight}{
A vector of weights corresponding to each of predictors. Weights are between 0 and 1.
}
  \item{iter}{
The number of RF model built to evaluate AIC and AUC. AIC is calculated using out-of-bag prediction from random forest using feature selected. AUC is calculated for classification problem only.
}
  \item{total}{
the number of times to repeat the selection for stability test in select.stable function.
}
  \item{cutoff}{
The minimum percentage of times that the feature is selected in multiple runs for stability test, ranges between 0 and 1.
}
}

\value{
return a list, including
  \item{AIC}{AIC calculated from random forest model out-of-bag predicted probability for classification, or out-of-bag prediction for classification}
  \item{AUC}{AUC calculated from out-of-bag prediction from random forest classification model}
  \item{Test.AUC}{AUC calculated from test prediction from random forest classification model}
  \item{AUC}{AUC calculated from out-of-bag prediction from random forest classification model}
  \item{feaSet}{feature set selected}

}
\references{
Guan, X., & Liu, L. (2018). Know-GRRF: Domain-Knowledge Informed Biomarker Discovery with Random Forests. 
}
\author{
Li Liu, Xin Guan
}
\note{
This function can be used after weights and regularization term are determined. Weights are from knowledege domain and regularization term can be determined by optimization. See example.
}


\examples{
##---- Example: classification ----
library(randomForest)

set.seed(1)
X.train<-data.frame(matrix(rnorm(10*100), nrow=100))
b=seq(1, 6, 0.5) 
##y has a linear relationship with 5 variables
y.train=b[7]*X.train$X6+b[8]*X.train$X7+b[9]*X.train$X8+b[10]*X.train$X9+b[11]*X.train$X10 
y.train=as.factor(ifelse(y.train>0, 1, 0)) ##classification

##use weights from domain knowledge. If not available, 
##can use statistic-based weights, e.g., variable importance, p/q value, etc
imp<-randomForest(X.train, y.train)$importance 
coefReg=0.5+0.5*imp/max(imp) 


#'\donttest{
#'use optimization function to find the appropriate regularization term 
#'to scale weights and then apply the weights to guide the RRF

#'opt<-optim(par=5, fn=on.aic, X.train=X.train, Y.train=y.train, 
#'weight=coefReg, iter=5,total=10, cutoff=0.5, num = 1, method='L-BFGS-B', 
#'lower=1, upper=10, control=list(fnscale=1,factr=100, trace = TRUE)) 
#'gives an error because under the initial value and searching space, 
#'no feature is selected, could try smaller number for initialization


#'opt<-optim(par=1, fn=on.aic, X.train=X.train, Y.train=y.train, 
#'weight=coefReg, iter=5,total=10, cutoff=0.5, num = 1, method='L-BFGS-B', 
#'lower=0.01, upper=0.5, control=list(fnscale=1,factr=100, trace = TRUE)) 
#'converged, could take long to run, opt$par returns pwr that could be used in rrf.opt.1
#'}

rrf.opt.1(X.train, y.train, pwr=1, weight=coefReg, total=5)  

}





