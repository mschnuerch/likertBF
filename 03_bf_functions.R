##==============================##
##           likertBF           ##
## =============================##

likertBF <- function(dat, b, M, tune, small, max_iter, updateProgress){
  k <- length(dat$Y1) - 1
  pars0 <- paste0("a", 1:k) # parameters in null model
  pars1 <- c(pars0, "t") # parameters in shift model
  parsU <- c(pars0, paste0("t", 1:k)) # parameters in unconstrained model
  
  l0 <- rep(-Inf, k) # lower bound M0
  u0 <- rep(Inf, k) # upper bound M0
  names(l0) <- names(u0) <- pars0
  
  l1 <- rep(-Inf, k + 1)
  u1 <- rep(Inf, k + 1)
  names(l1) <- names(u1) <- pars1
  
  lu <- rep(-Inf, 2 * k)
  uu <- rep(Inf, 2 * k)
  names(lu) <- names(uu) <- parsU
  
  # NULL MODEL
  updateProgress(value = .05, detail = "Sampling from Null Model")
  mcmc0_full <- sample0(dat, b, M, tune, small, max_iter)
  mcmc0 <- mcmc0_full$alpha
  colnames(mcmc0) <- pars0
  
  # SHIFT MODEL
  updateProgress(value = .3, detail = "Sampling from Shift Model")
  mcmc1_full <- sample1(dat, b, M, tune, small, max_iter)
  mcmc1 <- cbind(mcmc1_full$alpha, mcmc1_full$theta)
  colnames(mcmc1) <- pars1
  
  # UNCONSTRAINED MODEL
  updateProgress(value = .55, detail = "Sampling from Unconstrained Model")
  mcmcU_full <- sampleU(dat, b, M, tune, small, max_iter)
  mcmcU <- cbind(mcmcU_full$alpha, mcmcU_full$theta)
  colnames(mcmcU) <- parsU
  
  
  updateProgress(value = .75, detail = "Calculating marginal likelihoods")
  modU <- bridge_sampler(samples = mcmcU
                         , log_posterior = logPostU
                         , b = b
                         , data = dat
                         , lb = lu
                         , ub = uu
                         , param_types = rep("real", 2 * k)
                         , silent = TRUE)
  
  updateProgress(value = .82, detail = "Calculating marginal likelihoods")
  mod0 <- bridge_sampler(samples = mcmc0
                         , log_posterior = logPost0
                         , b = b
                         , data = dat
                         , lb = l0
                         , ub = u0
                         , param_types = rep("real", k)
                         , silent = TRUE) 
  
  updateProgress(value = .89, detail = "Calculating marginal likelihoods")
  mod1 <- bridge_sampler(samples = mcmc1
                         , log_posterior = logPost1
                         , b = b
                         , data = dat
                         , lb = l1
                         , ub = u1
                         , param_types = rep("real", k + 1)
                         , silent = TRUE)
  
  
  updateProgress(value = .96, detail = "Calculating Bayes factors")
  
  # Stochastic Dominance Model
  I <-  k + 1
  c1 <- mcmcU_full$alpha - mcmcU_full$theta
  c2 <- mcmcU_full$alpha + mcmcU_full$theta
  diffMat <- (c2 - c1) > 0
  a <- apply(diffMat, 1 , mean)
  a1 <- mean(a == 1)
  a2 <- mean(a == 0)
  
  # Bayes factors
  b0 <- bf(mod0, modU)$bf
  b1 <- bf(mod1, modU)$bf
  bd_0 <- (a1 + a2) * I / 2
  bd_12 <- a1 * I
  bd_21 <- a2 * I
  bf <- c(1, bd_0, bd_12, bd_21, b1, b0)
  out <- list(
    samples = mcmcU_full,
    bf = bf)
  
  updateProgress(value = 1, detail = "Finished")
  return(out)
}