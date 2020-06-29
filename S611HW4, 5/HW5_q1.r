library(R6)
 gamma_mcmc <- R6Class(
 classname = "gamma_mcmc",
 public = list(current = 2, shape = 5, scale = 1, iter=0,
 initialize = function(initial = 1, shape = 5, scale = 1){
 self$current <- initial
 self$shape <- shape
 self$scale <- scale
 self$iter <- 0
 },
 update = function(){
 theta.star <- exp(rnorm(1, mean = log(self$current), sd = 1))
 lr <- self$shape*(log(theta.star) - log(self$current)) -
 self$scale * (theta.star - self$current)
 if (log(runif(1)) < lr){
 self$current <- theta.star
 }
 self$iter <- self$iter + 1
 invisible(self)
 }
 )
 )


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