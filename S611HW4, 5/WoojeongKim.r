library(R6)
 normal_model <- R6Class(
 classname = "normal_model",
 public = list(initialize = function(y = NULL, mu_0 = NULL, tau_0 = NULL, s2_0 = NULL, nu_0 = NULL,
    initial_mu = NULL, initial_s2 = NULL, buffer_length = NULL){
     self <- new.env(parent = parent.frame())
     self$y <- y
     n <- max(rank(self$y))
     self$y_bar <- mean(y)
     self$y_var <- var(y)
     self$initial_mu <- mu_0 #rnorm(1, mean = mu_0, sd = {s2_0}^{1/2})
     self$initial_s2 <- s2_0 #(rgamma(1, shape = nu_0/2, rate = (nu_0 * s2_0)/2))^{-1}
     self$mu <- mu_0
     self$tau <- tau_0
     self$s2 <- s2_0
     self$nu <- nu_0
     self$buffer_length <- buffer_length
     self$buffer_mu <- self$buffer_s2 <- numeric(buffer_length)
     self$buffer_mu <- initial_mu#########
     self$buffer_s2 <- initial_s2######
     self$iteration <- 0
     #return(e)
 },
 update = function(self = NULL){
         n <- max(rank(self$y))
         mu <- rnorm(1, mean = (((n * self$y_bar)/ self$s2) + (self$initial_mu/(self$tau)^2))/((n/self$s2) +
             (1/(self$tau)^2)), sd = 1/(n/(self$s2) + 1/(self$tau)^2))
         s2 <- (rgamma(1, shape = (n+ self$nu)/2, rate = ((n-1) * (self$y_var) + n * (self$y_bar - self$mu)^2 +
                                                                 self$nu * self$initial_s2)/2))^{-1}
         self$mu <- mu
         self$s2 <- s2
         self$buffer_mu <- self$mu
         self$buffer_s2 <- self$s2
         self$iteration <- self$iteration + 1
         invisible(self)
     }
 )
 )

 (y, mu_0, tau_0, s2_0, nu_0, initial_mu, initial_s2)



sampler <- normal_model$new(y = c(24, 43, 58, 71, 43, 49, 61, 44, 67, 49, 53, 56, 59, 52, 62, 54, 57, 33, 46, 43, 57),
                           mu_0 = 42, tau_0 = 200, s2_0 = 400, nu_0 = 1, initial_mu = 1, initial_s2 = 1, buffer_length = 1000)
buffer_mu <- buffer_s2 <- numeric(1000)
for (sampler_iteration in 1:1000){
sampler$update()
buffer_mu[i] <- sampler$buffer_mu # or whatever NULLme you've used
buffer_s2[i] <- sampler$buffer_s2 # (idem)
}

>sampler$iteration
[1] 1000

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






sampler <- normal_model_extended$new(y = c(24, 43, 58, 71, 43, 49, 61, 44, 67, 49, 53, 56, 59, 52, 62, 54, 57, 33, 46, 43, 57),
                           mu_0 = 42, tau_0 = 200, s2_0 = 400, nu_0 = 1, initial_mu = 1, initial_s2 = 1, buffer_length = 1000)
buffer_mu <- buffer_s2 <- numeric(1000)
for (sampler_iteration in 1:1000){
sampler$update()
buffer_mu[i] <- sampler$buffer_mu # or whatever NULLme you've used
buffer_s2[i] <- sampler$buffer_s2 # (idem)
}
sampler$update_sampler(iterations = 1000)
hist(sampler$buffer_mu)
hist(sampler$buffer_s2)