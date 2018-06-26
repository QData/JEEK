.EEGM <- function(covMatrix, lambda){
  result = sign(covMatrix) * pmax(abs(covMatrix) - lambda, 0)
  result
}

.backwardMap <- function(covMatrix){
  niuList = 0.001 * (1:1000)
  bestDet = det(.EEGM(covMatrix, 0.001))
  bestniu = 0.001
  for (i in 1:1000) {
    if (bestDet < det(.EEGM(covMatrix, niuList[i]))) {
      bestDet = det(.EEGM(covMatrix, niuList[i]))
      bestniu = niuList[i]
    }
  }
  return(solve(.EEGM(covMatrix, bestniu)))
}

#A simplex solver for linear programming problem in jeek
.linprogS <- function(w, b, lambda){
  # K
  # Get parameters
  K = length(b)
  con = cbind(diag(1, K, K),rep(1,K))
  # linear programming solution
  f.obj = c(w, w)
  con1 = cbind(-con, +con)
  lambda = lambda * pmin(w[1:K], w[K+1])
  b1 = lambda - b
  b2 =  lambda + b
  f.con = rbind(-diag(2 * (K + 1)), con1, -con1)
  f.dir = rep("<=", 4 * K + 2)
  f.rhs = c(rep(0, 2 * (K + 1)), b1, b2)
  lp.out = lp("min", f.obj, f.con, f.dir, f.rhs)
  beta = lp.out$solution[1:(K + 1)] - lp.out$solution[(K + 2):(2 * (K + 1))]
  if (lp.out$status == 2) warning("No feasible solution!  Try a larger tuning parameter!")
  return(beta)
}

#The parallel version for jeek
.linprogSPar <- function(i, W, B, lambda){
  #get j,k
  p = dim(B)[1]
  K = dim(B)[3]
  #(1,2) (1,3) (1,4) (1,5)
  #      (2,3) (2,4) (2,5)
  #            (3,4) (3,5)
  #                  (4,5)
  #(1,2) -> (1,3) -> (2,3) -> (1,4) -> (2,4) -> (3,4)
  k = ceiling(sqrt(2 * i + 1/4) + 1/2)
  j = i - (k - 1) * (k - 2) / 2
  w = W[j,k,]
  b = B[j,k,]
  return(.linprogS(w, b, lambda))
}

jeek <- function(X, lambda, W = NA, covType = "cov", parallel = FALSE) {
  #decide if they dataframe or not
  if (is.data.frame(X[[1]])){
    for (i in 1:(length(X))){
      X[[i]] = as.matrix(X[[i]])
    }
  }
  K = length(X)
  if (!isSymmetric(X[[1]])){
    try(if (covType %in% c("cov","kendall") == FALSE) stop("The cov/cor type you specifies is not include in this package. Please use your own function to obtain the list of cov/cor and use them as the input of simule()"))
    if (covType == "cov")
    {
      for (i in 1:K){
        X[[i]] = cov(X[[i]])
      }
    }
    if (covType == "kendall"){
      for(i in 1:K){
        X[[i]] = cor.fk(X[[i]])
      }
    }
  }

  # get key parameters
  p = dim(X[[1]])[2]
  B = array(0, dim = c(p, p, K))
  weight = array(1, dim = c(p, p, (K + 1)))
  xt = array(0, dim = c(p, p, (K + 1)))
  graphs = list()
  #transfer to 3D tensor for parallelization
  for (i in 1:length(X)) {
    B[,,i] = X[[i]]
  }
  if (!is.na(W)){
    for (i in 1:length(W)) {
      weight[,,i] = W[[i]]
    }
  }
  #Decide if X is the data matrices or cov matrices

  B = array(apply(B, 3, .backwardMap), dim = c(p, p, K))

  f = function(x) .linprogSPar(x, weight, B, lambda)

  if (parallel == TRUE) {
    no_cores = detectCores() - 1
    cl = makeCluster(no_cores)
    # declare variable and function names to the cluster
    clusterExport(cl, list("f", "weight", "B", "lambda", ".linprogSPar", "lp", ".linprogS"), envir = environment())
    numOfVariable = (p - 1) * p / 2
    result = parLapply(cl, 1:numOfVariable, f)
    #print('Done!')
    for (i in 1:numOfVariable) {
      k = ceiling(sqrt(2 * i + 1/4) + 1/2)
      j = i - (k - 1) * (k - 2) / 2
      xt[j, k, ] = result[[i]]
      xt[k, j, ] = result[[i]]
    }
    stopCluster(cl)
  }
  else{
    for(j in 1:(p-1)){
      for(k in (j+1):p){
        xt[j,k,] = .linprogS(weight[j,k,], B[j,k,], lambda)
        xt[k,j,] = xt[j,k,]
      }
    }
  }
  for (i in 1:K) {
    graphs[[i]] = xt[, , i] + xt[, , (K + 1)] + diag(1,p,p)
  }
  out = list(Graphs = graphs)
  class(out) = "jeek"
  return(out)
}




