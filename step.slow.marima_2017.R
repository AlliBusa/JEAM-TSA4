step.slow <- function(object, data, penalty=2, max.iter=50, verbose=FALSE){
  ## object: A marima object
  ## data:   The same data as given as argument when fitting 'object'
  ## penalty: The penalty to be used
  ## max.iter: Number of iterations before evaluating the penalty
  ## verbose: Extra printing so that one can see which terms ar left out in each iteration
  
  # Init
  
  obj <- object # So that the original object is returned if no reduction is needed.
  "[" <- function(x, ...) .Primitive("[")(x, ..., drop = FALSE)
  
  if (any(obj$ar.fvalues[,,-1] >0)){
    ar.f <- obj$ar.fvalues[,,-1]
    ar.p <- obj$out.ar.pattern[,,-1]
    ar.min <- min(ar.f[ar.f>0])
  } else{
    ar.min <- Inf
    ar.p <- obj$out.ar.pattern
  }
  if (any(obj$ma.fvalues[,,-1] >0)){
    ma.f <- obj$ma.fvalues[,,-1]
    ma.p <- obj$out.ma.pattern[,,-1]
    ma.min <- min(ma.f[ma.f>0])
  } else {
    ma.min <- Inf
    ma.p <- obj$out.ma.pattern
  }
  print(c(ar.min, ma.min))
  # Now starting the actual model reduction
  while (min(ar.min, ma.min) < penalty){
    if (ar.min < ma.min){
      if (verbose) print(sprintf("AR out is: %i",which(ar.f ==ar.min)))
      ar.p[ar.f ==ar.min] <- FALSE
      if (verbose) print(ar.p)
    } else{
      if (verbose) print(sprintf("MA out is: %i",which(ma.f ==ma.min)))
      ma.p[ma.f ==ma.min] <- FALSE
      if (verbose)  print(ma.p)
    } # else
    if (ar.min != Inf){
      ar.p <- lead.one(ar.p, add = 1)
    }
    if (ma.min != Inf){
      ma.p <- lead.one(ma.p, add = 1)
    }
    ## Now restimate
    obj <- marima(data, ar.pattern =  ar.p,
                  ma.pattern =  ma.p, max.iter = max.iter)
    
    if (any(obj$ar.fvalues[,,-1] >0)){
      ar.f <- obj$ar.fvalues[,,-1]
      ar.p <- obj$out.ar.pattern[,,-1]
      ar.min <- min(ar.f[ar.f>0])
    } else{
      ar.p <- obj$out.ar.pattern
      ar.min <- Inf
    }
    if (any(obj$ma.fvalues[,,-1] >0)){
      ma.f <- obj$ma.fvalues[,,-1]
      ma.p <- obj$out.ma.pattern[,,-1]
      ma.min <- min(ma.f[ma.f>0])
    } else {
      ma.p <- obj$out.ma.pattern
      ma.min <- Inf
    }
    
  } # while
  return(obj)
}