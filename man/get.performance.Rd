\name{get.performance}
\alias{get.performance}
\title{
Get performance of feature selection
}
\description{
Comparing feature selected by algorithms and the ground truth in simulation
}
\usage{
get.performance(set.truth, set.sel, set.all)
}
\arguments{
  \item{set.truth}{
A vector of important features set in simulation (ground truth)
}
  \item{set.sel}{
A vector of important features selected by algorithms
}
  \item{set.all}{
A vector of all candidate features for feature selection
}
}

\value{
returned a vector of feature selection performance including JI (ratio of intersect of two sets and union of two sets), TPR (percentage of correctly selected features in all true important features) and FPR (percentage of wrongly selected features in true non-important features)
}
\references{
Guan, X., & Liu, L. (2018). Know-GRRF: Domain-Knowledge Informed Biomarker Discovery with Random Forests. 
}
\author{
Li Liu, Xin Guan
}


\examples{
set.truth=1:10  ##true important feature from ground truth
set.sel=c(8:10, 95)  ##selected feature by an algorithm
set.all=1:100  ##all candidate features

get.performance(set.truth, set.sel, set.all)


}
