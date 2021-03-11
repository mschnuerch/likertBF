##==============================##
##    Calculate Bayes Factors   ##
## =============================##

# install.packages("bridgesampling")
library(bridgesampling)
source("01_sampling functions.R")

likertBF <- function(df, b, M = 2e5, tune = NULL, small = rep(.0005, 2), progress = F){
  
  dat <- list(
    Y1 = df[,1],
    Y2 = df[,2]
  )
  
  k <- length(dat$Y1) - 1
  pars0 <- paste0("a", 1:k)
  pars1 <- c(pars0, "t")
  parsU <- c(pars0, paste0("t", 1:k))
  
  # Null Model
  if(progress)
    cat("\nNull Model:\n")
  mcmc0_full <- sample0(dat, b, M, tune, small, progress)
  mcmc0 <- mcmc0_full$alpha
  colnames(mcmc0) <- pars0
  low_bound <- rep(-Inf, k)
  up_bound <- rep(Inf, k)
  names(low_bound) <- names(up_bound) <- pars0
  if(progress)
    cat("  Calculating Marginal Likelihood...")
  mod0 <- bridge_sampler(samples = mcmc0
                         , log_posterior = logPost0
                         , b = b
                         , data = dat
                         , lb = low_bound
                         , ub = up_bound
                         , param_types = rep("real", k)
                         , silent = TRUE) 
  if(progress)
    cat("[Done]\n")
  
  # Shift Model
  if(progress)
    cat("\nShift Model:\n")
  mcmc1_full <- sample1(dat, b, M, tune, small, progress)
  mcmc1 <- cbind(mcmc1_full$alpha, mcmc1_full$theta)
  colnames(mcmc1) <- pars1
  low_bound <- rep(-Inf, k + 1)
  up_bound <- rep(Inf, k + 1)
  names(low_bound) <- names(up_bound) <- pars1
  if(progress)
    cat("  Calculating Marginal Likelihood...")
  mod1 <- bridge_sampler(samples = mcmc1
                         , log_posterior = logPost1
                         , b = b
                         , data = dat
                         , lb = low_bound
                         , ub = up_bound
                         , param_types = rep("real", k + 1)
                         , silent = TRUE)
  if(progress)
    cat("[Done]\n")
  
  # Unconstrained Model
  if(progress)
    cat("\nUnconstrained Model:\n")
  mcmcU_full <- sampleU(dat, b, M, tune, small, progress)
  mcmcU <- cbind(mcmcU_full$alpha, mcmcU_full$theta)
  colnames(mcmcU) <- parsU
  low_bound <- rep(-Inf, 2 * k)
  up_bound <- rep(Inf, 2 * k)
  names(low_bound) <- names(up_bound) <- parsU
  if(progress)
    cat("  Calculating Marginal Likelihood...")
  modU <- bridge_sampler(samples = mcmcU
                         , log_posterior = logPostU
                         , b = b
                         , data = dat
                         , lb = low_bound
                         , ub = up_bound
                         , param_types = rep("real", 2 * k)
                         , silent = TRUE)
  if(progress)
    cat("[Done]\n")
  
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
  bd_2 <- (a1 + a2) * I / 2
  bd_12 <- a1 * I
  bd_21 <- a2 * I
  bf <- c(b0, b1, bd_2, bd_12, bd_21)
  names(bf) <- c("B_0u","B_1u","B_du (2sided)","B_du (1 > 2)","B_du (2 > 1)")
  out <- list(
    samples = list(mod0 = mcmc0_full,
                mod1 = mcmc1_full,
                modU = mcmcU_full),
    bf = bf
  )
  return(out)
}
