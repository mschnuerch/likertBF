##==============================##
##    Log Posterior Functions   ##
## =============================##


# Null Model --------------------------------------------------------------

logPost0 <- function(alpha, data, b){
  crit <- c(-Inf, alpha, Inf)
  prob <- diff(pnorm(crit))
  valid <- FALSE
  try({
    out <- dmultinom(data$Y1, prob = prob, log = TRUE) +
      dmultinom(data$Y2, prob = prob, log = TRUE) +
      sum(dnorm(alpha, 0, b[1], log = TRUE))
    valid = TRUE
  }, silent = TRUE)
  
  if (valid) 
    return(out)
  else
    return(-Inf)
  
}


# Shift Model -------------------------------------------------------------

logPost1 <- function(par, data, b){
  I <- length(data$Y1)
  alpha <- par[1:(I - 1)]
  theta <- par[I]
  c1 <- c(-Inf, alpha - .5 * theta, Inf)
  c2 <- c(-Inf, alpha + .5 * theta, Inf)
  prob1 <- diff(pnorm(c1))
  prob2 <- diff(pnorm(c2))
  valid <- FALSE
  try({
    out <- dmultinom(data$Y1, prob = prob1, log = TRUE) +
      dmultinom(data$Y2, prob = prob2, log = TRUE) +
      sum(dnorm(alpha, 0, b[1], log = TRUE)) +
      sum(dnorm(theta, 0, b[2], log = TRUE))
    valid = TRUE}, silent = TRUE)
  
  if (valid) 
    return(out)
  else
    return(-Inf)
  
}



# Unconstrained Model -----------------------------------------------------

logPostU <- function(par, data, b){
  I <- length(data$Y1)
  alpha <- par[1:(I - 1)]
  theta <- par[I:(2 * I - 2)]
  c1 <- c(-Inf, alpha - .5 * theta, Inf)
  c2 <- c(-Inf, alpha + .5 * theta, Inf)
  prob1 <- diff(pnorm(c1))
  prob2 <- diff(pnorm(c2))
  # ans <- sum(data$Y1 * log(prob1)) + 
  #   sum(data$Y2 * log(prob2)) +
  #   sum(alpha^2) / (2 * b[1]^2) +
  #   sum(theta^2) / (2 * b[2]^2)
  valid <- FALSE
  try({
    out <- dmultinom(data$Y1, prob = prob1, log = TRUE) +
      dmultinom(data$Y2, prob = prob2, log = TRUE) +
      sum(dnorm(alpha, 0, b[1], log = TRUE)) +
      sum(dnorm(theta, 0, b[2], log = TRUE))
    valid = TRUE}, silent = TRUE)
  
  if (valid) 
    return(out)
  else
    return(-Inf)
  
}


##==============================##
## Posterior Sampling Functions ##
## =============================##


# Null Model --------------------------------------------------------------

# sampler for given sd of proposal distribution
sample0_fixed <- function(dat, b, M, tune, start = NULL, progress = FALSE){
  I <- length(dat$Y1) # how many pars
  counter <- 0
  alpha <- matrix(nrow = M, ncol = I - 1) # matrix for posterior samples
  if (is.null(start)){ # were any starting values provided
    alpha[1,] <- sort(rnorm(I - 1)) # draw random starting values
  }else{
    alpha[1,] <- start$alpha
  }
  if(progress)
    progress_bar <- txtProgressBar(min = 0, max = M, style = 3, char = "=")
  for (m in 2:M){
    alpha[m, ] <- alpha[m - 1,] # use current values
    cand <- rnorm(I - 1, alpha[m,], tune[1]) # draw candidate values
    while (mean(diff(cand) > 0) != 1) { # do the crits order
      cand <- rnorm(I - 1, alpha[m,], tune[1]) # draw candidate values
    }
    lf_cur <- logPost0(alpha[m,], dat, b) # log posterior density of current value
    lf_cand <- logPost0(cand, dat, b) #  log posterior density of candidate
    prob <- min(exp(lf_cand - lf_cur), 1) 
    if (rbinom(1, 1, prob)){
      alpha[m,] <- cand
      counter <- counter + 1}
    if(progress)
      setTxtProgressBar(progress_bar, value = m)
  }
  if(progress)
    close(progress_bar)
  
  return(list(alpha = alpha, accpt = counter / M, tune = tune))
}

# sampler with automatic tuning mechanism
sample0 <- function(dat, b, M, tune, small, progress){
  
  if (!is.null(tune)) {
    out <- sample0_fixed(dat, b, M, tune, progress = progress)
  }else{
    #small <- .0005
    if(progress)
      cat("  Tuning proposal distributions...\n")
    M0 <- 2000
    tmp <- sample0_fixed(dat, b, M = M0, tune = .04)
    start <- list(
      alpha = apply(tmp$alpha, 2, mean)
    )
    # adjust alpha
    tune <- small[1]
    repeat{
      tmp <- sample0_fixed(dat, b, M = M0, tune = tune, start = start)
      if(tmp$accpt < .35){
        if(tmp$accpt < .3){
          tune <- tune - tune / 10
          next
        }else{
          break
        }
      }
      tune <- tune * 2
    }
    start <- list(
      alpha = apply(tmp$alpha, 2, mean)
    )
    if(progress)
      cat("  Sampling from posterior...\n")
    out <- sample0_fixed(dat, b, M = M, tune = tune, start = start, 
                         progress = progress)
  }
  return(out)
}


# Shift Model -------------------------------------------------------------

# sampler for given sd of proposal distribution
sample1_fixed <- function(dat, b, M, tune, start = NULL, progress = FALSE){
  I <- length(dat$Y1)
  counter <- 0
  alpha <- matrix(nrow = M, ncol = I - 1)
  theta <- 1:M
  if (is.null(start)){
    alpha[1,] <- sort(rnorm(I - 1))
    theta[1] <- 0
  }else{
    alpha[1,] <- start$alpha
    theta[1] <- start$theta
  }
  if(progress)
    progress_bar <- txtProgressBar(min = 0, max = M, style = 3, char = "=")
  for (m in 2:M){
    alpha[m,] <- alpha[m - 1,]
    theta[m] <- theta[m - 1]
    candAlpha <- rnorm(I - 1, alpha[m,], sqrt(tune[1]))
    candTheta <- rnorm(1, theta[m], sqrt(tune[2]))
    c1 <- candAlpha - .5 * candTheta
    c2 <- candAlpha + .5 * candTheta
    while (!(mean(diff(c1) > 0) == 1 & 
             mean(diff(c2) > 0) == 1 &
             mean(diff(candAlpha) > 0) == 1)) {
      candAlpha <- rnorm(I - 1, alpha[m,], sqrt(tune[1]))
      candTheta <- rnorm(1, theta[m], sqrt(tune[2]))
      c1 <- candAlpha - .5 * candTheta
      c2 <- candAlpha + .5 * candTheta
    }
    lf_cur <- logPost1(c(alpha[m,], theta[m]), dat, b)
    lf_cand <- logPost1(c(candAlpha, candTheta), dat, b)
    prob <- min(exp(lf_cand - lf_cur), 1)
    if (rbinom(1, 1, prob)){
      alpha[m,] <- candAlpha
      theta[m] <- candTheta
      counter <- counter + 1}
    
    if(progress)
      setTxtProgressBar(progress_bar, value = m)
  }
  if(progress)
    close(progress_bar)
  
  return(list(alpha = alpha, theta = theta, accpt = counter / M, tune = tune))
}

# sampler with automatic tuning mechanism
sample1 <- function(dat, b, M, tune, small, progress){
  
  if (!is.null(tune)){
    out <- sample1_fixed(dat, b, M, tune, progress = progress)
  }else{
    #small <- .0005
    if(progress)
      cat("  Tuning proposal distributions...\n")
    M0 <- 2000
    tmp <- sample1_fixed(dat, b, M = M0, tune = c(.04,.02))
    start <- list(
      alpha = apply(tmp$alpha, 2, mean),
      theta = mean(tmp$theta)
    )
    # adjust alpha
    tune <- c(small[1], .02)
    repeat{
      tmp <- sample1_fixed(dat, b, M = M0, tune = tune, start = start)
      if(tmp$accpt < .5) 
        break
      tune[1] <- tune[1] * 2
    }
    
    #adjust theta
    tune <- c(tune[1], small[2])
    repeat{
      tmp <- sample1_fixed(dat, b, M = M0, tune = tune, start = start)
      if(tmp$accpt < .35){
        if(tmp$accpt < .3){
          tune[2] <- tune[2] - tune[2] / 10
          next
        }else{
          break
        }
      }
      tune[2] <- tune[2] * 2
    }
    start <- list(
      alpha = apply(tmp$alpha, 2, mean),
      theta = mean(tmp$theta)
    )
    if(progress)
      cat("  Sampling from posterior...\n")
    out <- sample1_fixed(dat, b, M = M, tune = tune, start = start, 
                         progress = progress)
  }
  return(out)
}


# Unconstrained Model -----------------------------------------------------

# sampler for given sd of proposal distribution
sampleU_fixed <- function(dat, b, M, tune, start = NULL, progress = F){
  I <- length(dat$Y1)
  counter <- 0
  alpha <- theta <- matrix(nrow = M, ncol = I - 1)
  if (is.null(start)){
    alpha[1,] <- sort(rnorm(I - 1))
    theta[1,] <- rep(0, I - 1)}
  else{
    alpha[1,] <- start$alpha
    theta[1,] <- start$theta}
  if(progress)
    progress_bar <- txtProgressBar(min = 0, max = M, style = 3, char = "=")
  for (m in 2:M){
    alpha[m,] <- alpha[m - 1,]
    theta[m,] <- theta[m - 1,]
    candAlpha <- rnorm(I - 1, alpha[m,], sqrt(tune[1]))
    candTheta <- rnorm(I - 1, theta[m,], sqrt(tune[2]))
    c1 <- candAlpha - .5 * candTheta
    c2 <- candAlpha + .5 * candTheta
    
    while (!(mean(diff(c1) > 0) == 1 & 
             mean(diff(c2) > 0) == 1 &
             mean(diff(candAlpha) > 0) == 1)) {
      candAlpha <- rnorm(I - 1, alpha[m,], sqrt(tune[1]))
      candTheta <- rnorm(I - 1, theta[m,], sqrt(tune[2]))
      c1 <- candAlpha - .5 * candTheta
      c2 <- candAlpha + .5 * candTheta
    }
    
    lf_cur <- logPostU(c(alpha[m,], theta[m,]), dat, b)
    lf_cand <- logPostU(c(candAlpha, candTheta), dat, b)
    prob <- min(exp(lf_cand - lf_cur), 1)
    if (rbinom(1, 1, prob)){
      alpha[m,] <- candAlpha
      theta[m,] <- candTheta
      counter <- counter + 1}
    if(progress)
      setTxtProgressBar(progress_bar, value = m)
  }
  if(progress)
    close(progress_bar)
  return(list(alpha = alpha, theta = theta, accpt = counter / M, tune = tune))
}

# sampler with automatic tuning mechanism
sampleU <- function(dat, b, M, tune, small, progress){
  
  if (!is.null(tune)){
    out <- sampleU_fixed(dat, b, M, tune, progress = progress)
  }else{
    #small <- .0005
    if(progress)
      cat("  Tuning proposal distributions...\n")
    M0 <- 2000
    tmp <- sampleU_fixed(dat, b, M = M0, tune = c(.04,.02))
    start <- list(
      alpha = apply(tmp$alpha, 2, mean),
      theta = apply(tmp$theta, 2, mean)
    )
    
    # adjust alpha
    tune <- c(small[1], .02)
    repeat{
      tmp <- sampleU_fixed(dat, b, M = M0, tune = tune, start = start)
      if(tmp$accpt < .5) 
        break
      tune[1] <- tune[1] * 2
    }
    
    #adjust theta
    tune <- c(tune[1], small[2])
    repeat{
      tmp <- sampleU_fixed(dat, b, M = M0, tune = tune, start = start)
      if(tmp$accpt < .35){
        if(tmp$accpt < .3){
          tune[2] <- tune[2] - tune[2] / 10
          next
        }else{
          break
        }
      }
      tune[2] <- tune[2] * 2
    }
    start <- list(
      alpha = apply(tmp$alpha, 2, mean),
      theta = apply(tmp$theta, 2, mean)
    )
    if(progress)
      cat("  Sampling from posterior...\n")
    out <- sampleU_fixed(dat, b, M = M, tune = tune, start = start, progress = progress)
  }
  return(out)
}
