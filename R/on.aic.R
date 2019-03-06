on.aic <-
  function (X.train, Y.train, pwr, weight, iter=1,total=10, cutoff=0.5, num = 1) 
  {
    if (num == 1) {
      return(mean(rrf.opt.1(X.train, Y.train, pwr=pwr, weight=weight, iter=iter,total=total, cutoff=cutoff)$AIC))
    }
    else {
      return(mean(rrf.opt.m(X.train, Y.train, pwr=pwr, weight=weight, iter=iter,total=total, cutoff=cutoff)$AIC))
    }
  }
