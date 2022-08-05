# Initial values for MCMC algorithm (unconstrained model)

inner_fun <- function(K) {
  
  exit <- FALSE
  while (!exit) {
    alpha <- sort(rnorm(K - 1))
    theta <- rnorm(K - 1, 0, .1)
    crit1 <- alpha - .5 * theta
    crit2 <- alpha + .5 * theta
    
    if(!is.unsorted(crit1) & !is.unsorted(crit2)) {
      p1 <- pnorm(c(crit1, Inf)) - pnorm(c(-Inf, crit1))
      p2 <- pnorm(c(crit2, Inf)) - pnorm(c(-Inf, crit2))
      exit <- TRUE
    }
  }
  
  out <- list(
    alpha = alpha, 
    theta = theta,
    gamma1 = crit1,
    gamma2 = crit2,
    lambda1 = p1,
    lambda2 = p2
  )
  
  return(out)
  
}

gen_init_values <- function (chains, K) {
  replicate(chains, list(inner_fun(K)))
}


# Calculate prior probability of dominance constraint

gen_sample <- function (K, b1, b2) {
  
  # we have to make sure that the sampled thresholds order
  exit <- FALSE
  while (!exit) {
    alpha <- sort(rnorm(K - 1, 0, b1))
    theta <- rnorm(K - 1, 0, b2)
    c1 <- alpha - .5*theta
    c2 <- alpha + .5*theta
    
    if(!is.unsorted(c1) & !is.unsorted(c2)) {
      # if thresholds order in both conditions, 
      # the sample is valid
      exit <- TRUE
    }
  }
  
  list(gamma1 = c1,
       gamma2 = c2)
  
}

check_sample <- function (x) {
  
  d_12 <- all(x$gamma1 > x$gamma2)
  d_21 <- all(x$gamma1 < x$gamma2)
  d <- d_12 + d_21
  
  return(
    c(
      greater = d_12,
      less = d_21,
      two_sided = d
    )
  )
  
}

sample_from_prior <- function (n_samples, K, b1, b2) {
  
  samples <- purrr::map(
    rep(K, n_samples),
    gen_sample,
    b1 = b1,
    b2 = b2)
  
  dominance <- purrr::map_df(
    samples,
    check_sample
  )
  
  colMeans(dominance)
  
}

