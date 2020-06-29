 create_sampler <- function(y, mu_0, tau_0, s2_0, nu_0, initial_mu, initial_s2){
     e <- new.env(parent = parent.frame())
     e$y <- y
     e$y_bar <- mean(y)
     e$y_var <- var(y)
     e$initial_mu <- mu_0 #rnorm(1, mean = mu_0, sd = {s2_0}^{1/2})
     e$initial_s2 <- s2_0 #(rgamma(1, shape = nu_0/2, rate = (nu_0 * s2_0)/2))^{-1}
     e$mu <- mu_0
     e$tau <- tau_0
     e$s2 <- s2_0
     e$nu <- nu_0
     return(e)
 }
 
 update_sampler <- function(sampler) {
     n <- max(rank(sampler$y))
     mu <- rnorm(1, mean = (((n * sampler$y_bar)/ sampler$s2) + (sampler$initial_mu/(sampler$tau)^2))/((n/sampler$s2) + (1/(sampler$tau)^2)), 
                 sd = 1/(n/(sampler$s2) + 1/(sampler$tau)^2))
     s2 <- (rgamma(1, shape = (n+ sampler$nu)/2, rate = ((n-1) * (sampler$y_var) + n * (sampler$y_bar - sampler$mu)^2 +
                                                             sampler$nu * sampler$initial_s2)/2))^{-1}
     sampler$mu <- mu
     sampler$s2 <- s2
 }
 
 sampler <- create_sampler(y = c(24, 43, 58, 71, 43, 49, 61, 44, 67, 49, 53, 56, 59, 52, 62, 54, 57, 33, 46, 43, 57),
                           mu_0 = 42, tau_0 = 200, s2_0 = 400, nu_0 = 1, initial_mu = 1, initial_s2 = 1)
 buffer_mu <- buffer_s2 <- numeric(1000)
 for (i in 1:1000){
     update_sampler(sampler)
     buffer_mu[i] <- sampler$mu # or whatever name you've used
     buffer_s2[i] <- sampler$s2 # (idem)
     sampler$iteration[i] <- 0
     sampler$iteration[i+1] <- sampler$iteration[i]+1
 }
#The following results are number of iteration of variable "sampler" and mean, sd of mu and s2.
 sampler$iteration
   [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
  [42] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
  [83] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 [124] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 [165] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 [206] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 [247] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 [288] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 [329] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 [370] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 [411] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 [452] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 [493] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 [534] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 [575] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 [616] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 [657] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 [698] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 [739] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 [780] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 [821] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 [862] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 [903] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 [944] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 [985] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1
 mean(buffer_mu)
[1] -684.9042
 sd(buffer_mu)
[1] 37791.76
 hist(buffer_mu)
 mean(buffer_s2)
[1] 1485663803
 sd(buffer_s2)
[1] 2324694826
 hist(buffer_s2)
 