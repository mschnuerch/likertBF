##==============================##
##           likertBF           ##
## =============================##

likertBF <- function(Y1, Y2, b1, b2, chains, iter, warmup, updateProgress){

  dat <- list(K = length(Y1),
              y1 = Y1,
              y2 = Y2,
              b1 = b1,
              b2 = b2)

  # NULL MODEL
  updateProgress(value = .05, detail = "Sampling from Null Model")
  samples_null <- sampling(stanModels:::stanmodels[["model_null"]],
                           data = dat,
                           iter = (iter + warmup),
                           chains = chains,
                           warmup = warmup,
                           refresh = 0)


  # SHIFT MODEL
  updateProgress(value = .3, detail = "Sampling from Shift Model")
  samples_shift <- sampling(stanModels:::stanmodels[["model_shift"]],
                            data = dat,
                            iter = (iter + warmup),
                            chains = chains,
                            warmup = warmup,
                            refresh = 0)

  # UNCONSTRAINED MODEL
  updateProgress(value = .55, detail = "Sampling from Unconstrained Model")
  samples_unconstrained <- sampling(stanModels:::stanmodels[["model_unconstrained"]],
                                    data = dat,
                                    iter = (iter + warmup),
                                    chains = chains,
                                    warmup = warmup,
                                    init = gen_init_values(chains, dat$K),
                                    refresh = 0,
                                    control = list(adapt_delta = 0.99,
                                                   max_treedepth = 15))


  updateProgress(value = .75, detail = "Calculating marginal likelihoods")
  ml_null <- bridge_sampler(samples_null, silent = TRUE)

  updateProgress(value = .82, detail = "Calculating marginal likelihoods")
  ml_shift <- bridge_sampler(samples_shift, silent = TRUE)

  updateProgress(value = .89, detail = "Calculating marginal likelihoods")
  ml_unconstrained <- bridge_sampler(samples_unconstrained, silent = TRUE)


  updateProgress(value = .96, detail = "Calculating Bayes factors")

  # Stochastic Dominance Model
  prior <- sample_from_prior(iter * chains, dat$K, b1, b2)

  post_samples <- rstan::extract(samples_unconstrained)
  diffMat <- (post_samples$gamma1 - post_samples$gamma2) > 0
  tmp <- apply(diffMat, 1, mean)
  post_greater <- mean(tmp == 1)
  post_less <- mean(tmp == 0)

  # Bayes factors
  b0 <- bf(ml_null, ml_unconstrained)$bf
  b1 <- bf(ml_shift, ml_unconstrained)$bf
  bd_two_sided <- (post_greater + post_less) / prior["two_sided"]
  bd_greater <- post_greater / prior["greater"]
  bd_less <- post_less / prior["less"]
  bf <- c(1, bd_two_sided, bd_greater, bd_less, b1, b0)
  out <- list(
    samples = post_samples,
    bf = bf)

  updateProgress(value = 1, detail = "Finished")
  return(out)
}
