gamma_mcmc_extended <- R6Class(
 classname = 'normal_model_extended',
 inherit = normal_model,
 private = list(buffer = NULL, buffer_length=100),
 public = list(
 initialize = function(y = c(24, 43, 58, 71, 43, 49, 61, 44, 67, 49, 53, 56, 59, 52, 62, 54, 57, 33, 46, 43, 57),
                           mu_0 = 42, tau_0 = 200, s2_0 = 400, nu_0 = 1, initial_mu = 1, initial_s2 = 1, buffer_length = 1000){
 private$buffer_length <- buffer_length
 private$buffer <- numeric(buffer_length)
 private$buffer_mu[i] <- sampler$buffer_mu # or whatever name you've used
 private$buffer_s2[i] <- sampler$buffer_s2 # (idem)
 super$initialize(y = c(24, 43, 58, 71, 43, 49, 61, 44, 67, 49, 53, 56, 59, 52, 62, 54, 57, 33, 46, 43, 57),
                           mu_0 = 42, tau_0 = 200, s2_0 = 400, nu_0 = 1, initial_mu = 1, initial_s2 = 1, buffer_length = 1000)
 },
 update = function(iterations = 1){
 n <- self$iteration
 for (i in seq(from = n, length = iterations)){
 super$update()
 if (i <= private$buffer_length){
 private$buffer_mu[i] <- private$buffer_mu # or whatever name you've used
 private$buffer_s2[i] <- private$buffer_s2 # (idem)
 }
 }
 if (i > private$buffer_length) warning('Buffer full! extra samples discarded')
 }
 ),
 active = list(history = function() private$buffer)
 )






sampler <- normal_model_extended$new(...your arguments here...)
sampler$update_sampler(iterations = 1000)
hist(sampler$buffer_mu)
hist(sampler$buffer_s2)