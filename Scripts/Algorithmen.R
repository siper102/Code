durbin.levinson <- function(data = c(), gam = c(), n.lags = 2){
  if(length(data) > 0){
    gam <- itsmr::acvf(data, n.lags +1)
  }
  d <- matrix(data = rep(0, n.lags ** 2), nrow = n.lags)
  v <- numeric(n.lags+1)
  d[1, 1] <- gam[2] / gam[1]
  v[1] <- gam[1]
  v[2] <- v[1] * (1 - d[1, 1]**2) 
  
  if(n.lags == 1){ 
    return(list(d = d, v = v)) 
  }
  for(n in 2:n.lags){
    d[n, n] <- 1 / v[n] * (gam[n+1] - sum(d[n-1, 1:(n-1)] * gam[n:2]))
    d[n, 1:(n-1)] <- d[(n-1), 1:(n-1)] - d[n, n] * d[(n-1), (n-1):1]
    v[n+1] <- v[n] * (1 - d[n, n]**2)
  }
  return(list(d = d, v = v))
}


innovations <- function(data = c(), gam = c(), n.lags = 2){
  if(length(data) > 0){
    gam <- acvf(data, n.lags +1)
  }
  c <- matrix(data = rep(0, n.lags * n.lags), nrow = n.lags)
  v <- numeric(n.lags+1)
  v[1] <- gam[1]
  
  if(n.lags == 1){
    c[1, 1] <- gam[2] / v[1]
    v[2] <- gam[1] - c[1, 1]**2 * v[1]
    return(list(c=c, v=v))
  }
  for(n in 1:n.lags){
    c[n, n] <- gam[n+1] / v[1]
    for(k in 1:(n-1)){
      c[n, n-k] <- ( gam[n-k+1] - sum(c[k, k:1] * c[n, n:(n-k+1)] * v[1:k]) ) / v[k+1]
    }
    v[n+1] <- gam[1] - sum(c[n, n:1]**2 * v[1:n]) 
  }
  return(list(c=c, v=v))
}

x.hat <- function(data, params.ia, n){
  x.hats <- numeric(n)
  for(i in 1:n){
    if(i==1){
      x.hats[i] <- 0
    }
    else{
      x.hats[i] <- sum( params.ia[(i-1), 1:(i-1)] * (data[(i-1):1] - x.hats[(i-1):1]))
    }
  }
  return(x.hats[n])
}


predict.ia <- function(data, params, n.lags, pl = FALSE, index = 1:length(data)){
  n <- length(data)
  pred <- numeric(n - n.lags)
  params.ia <- params[n.lags, 1:n.lags]
  for(i in (n.lags+1):n){
    x.predictor <-  data[(i-n.lags):(i-1)]
    pred[i-n.lags] <- x.hat(x.predictor, params, n = (n.lags+1))
  }
  pred <- c(rep(NaN, n.lags), pred)
  res <- data - pred
  mse <- sd(res, na.rm = T)
  if(pl){
    plot(index,data, type = "l", main = paste("Innovations Algorithmus für n = ", n.lags, "MSE = ", mse))
    points(index, pred, type = "l", col = "red", add = T)
  }
  return(list(pred=pred, res = res, mse = mse))
}

forecast.dl <- function(data, params, n.lags){
  n <- length(data)
  x.predictor <- data[(n-n.lags-1):n]
  return(sum(x.predictor * rev(params[n.lags, 1:n.lags])))
}


predict.dl <- function(data, params, iterations = n.lags, n.lags, pl = F, index = 1:length(data)){
  params.algorithm <- params[iterations, 1:n.lags]
  n <- length(data)
  pred <- numeric(n - n.lags)
  for(i in 1:n.lags){
    pred <- pred + params.algorithm[n.lags+1-i] * data[i:(n - n.lags + i - 1)]
  }
  pred <- c(rep(NaN, n.lags), pred)
  res <- data - pred
  mse <- sd(res, na.rm = T)
  if(pl){
    plot(index,data, type = "l", main = paste("Durbin Levinson Algorithmus für n = ", n.lags, "MSE = ", mse))
    points(index, pred, type = "l", col = "red", add = T)
  }
  return(list(pred = pred, res = res, mse = mse))
}


