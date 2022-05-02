step.slow.p <- function(object, data, p.value=0.05, max.iter=50, verbose=FALSE){
  ## object: A marima object
  ## data:   The same data as given as argument when fitting 'object'
  ## p.value: The significance level
  ## max.iter: Number of iterations before evaluating the penalty
  ## verbose: Extra printing so that one can see which terms ar left out in each iteration
  
  # Init
  
  obj <- object # So that the original object is returned if no reduction is needed.
  "[" <- function(x, ...) .Primitive("[")(x, ..., drop = FALSE)
  
  if (any(obj$ar.fvalues[,,-1] >0)){
    ar.pv <- obj$ar.pvalues[,,-1]
    ar.p <- obj$out.ar.pattern[,,-1]
    ar.pv.max <- max(ar.pv[ar.p==1])
  } else{
    ar.pv.max <- -1
    ar.p <- obj$out.ar.pattern
  }
  if (any(obj$ma.fvalues[,,-1] >0)){
    ma.pv <- obj$ma.pvalues[,,-1]
    ma.p <- obj$out.ma.pattern[,,-1]
    ma.pv.max <- max(ma.pv[ma.p==1])
  } else {
    ma.pv.max <- -1
    ma.p <- obj$out.ma.pattern
  }
  print(c(ar.pv.max, ma.pv.max))
  # Now starting the actual model reduction
  while (max(ar.pv.max, ma.pv.max) > p.value){
    if (ar.pv.max > ma.pv.max){
      if (verbose) print(sprintf("AR out is: %i",which(ar.f ==ar.min)))
      ar.p[ar.pv ==ar.pv.max] <- FALSE
      if (verbose) print(ar.p)
    } else{
      if (verbose) print(sprintf("MA out is: %i",which(ma.f ==ma.min)))
      ma.p[ma.pv ==ma.pv.max] <- FALSE
      if (verbose)  print(ma.p)
    } # else
    if (ar.pv.max != -1){
      ar.p <- lead.one(ar.p, add = 1)
    }
    if (ma.pv.max != -1){
      ma.p <- lead.one(ma.p, add = 1)
    }
    ## Now restimate
    obj <- marima(data, ar.pattern =  ar.p,
                  ma.pattern =  ma.p, max.iter = max.iter)
    if (any(obj$ar.fvalues[,,-1] >0)){
      ar.pv <- obj$ar.pvalues[,,-1]
      ar.p <- obj$out.ar.pattern[,,-1]
      ar.pv.max <- max(ar.pv[ar.p==1])
    } else{
      ar.pv.max <- -1
      ar.p <- obj$out.ar.pattern
    }
    if (any(obj$ma.fvalues[,,-1] >0)){
      ma.pv <- obj$ma.pvalues[,,-1]
      ma.p <- obj$out.ma.pattern[,,-1]
      ma.pv.max <- max(ma.pv[ma.p==1])
    } else {
      ma.pv.max <- -1
      ma.p <- obj$out.ma.pattern
    }
    print(c(ar.pv.max, ma.pv.max))
    

  } # while
  obj$call.ar.pattern <- object$call.ar.pattern
  obj$call.ma.pattern <- object$call.ma.pattern
  return(obj)
}