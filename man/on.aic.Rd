\name{on.aic}
\alias{on.aic}
\title{
AIC from model built with KnowGRRF, functions used in optimization to find scaling parameter for rrf.opt.1 or rrf.opt.m
}
\description{
This function can be used to search scaling parameter used in KnowGRRF while minimizing AIC.
}
\usage{
on.aic(X.train, Y.train, pwr, weight, iter=1,total=10, cutoff=0.5, num = 1)
}

\arguments{
  \item{X.train}{
a data frame or matrix (like x) containing predictors for the training set.
}
  \item{Y.train}{
response for the training set. If a factor, classification is assumed, otherwise regression is assumed. If omitted, will run in unsupervised mode.
}
  \item{pwr}{
Regularization term (a single number for single domain, a vector for multiple domain) to adjust the scale of weights. Larger regularization will differentiate the importance of variables more significantly. Fewer variables tend to be selected with large pwr. This parameter can be found by optimization.
}
  \item{weight}{
A vector of weights for single domain, or a matrix of weights for multiple domains, corresponding to each of predictors. Weights are between 0 and 1. For multiple domains, each column in weight matrix corresponds to weights from one domain. 
}
  \item{iter}{
The number of RF model built to evaluate AIC. AIC is calculated using out-of-bag prediction from random forest using feature selected. 
}
  \item{total}{
the number of times to repeat the selection for stability test in select.stable function.
}
  \item{cutoff}{
The minimum percentage of times that the feature is selected in multiple runs for stability test, ranges between 0 and 1.
}
  \item{num}{
The number of domain knowledge that weights come from.
}
}

\value{
mean of AIC from a number of RF model using feature selected by KnowGRRF
}
\references{
Guan, X., & Liu, L. (2018). Know-GRRF: Domain-Knowledge Informed Biomarker Discovery with Random Forests. 
}
\author{
Xin Guan, Li Liu
}

\examples{
##used in optim function. See examples in rrf.opt.1 and rrf.opt.m

}

