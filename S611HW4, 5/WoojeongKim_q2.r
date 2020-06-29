create_sampler <- function(y, mu_0, tau_0, s2_0, nu_0, initial_mu, initial_s2, buffer_length){
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
     e$buffer_length <- buffer_length
     e$buffer_mu <- e$buffer_s2 <- numeric(buffer_length)
     e$buffer_mu[1] <- initial_mu#########
     e$buffer_s2[1] <- initial_s2######
     return(e)
 }


 update_sampler <- function(sampler, iterations = 1) {
     i <- 2
     m <- iterations
     a <- c(1:m)
     for(i in a){
         n <- max(rank(sampler$y))
         mu <- rnorm(1, mean = (((n * sampler$y_bar)/ sampler$s2) + (sampler$initial_mu/(sampler$tau)^2))/((n/sampler$s2) +
             (1/(sampler$tau)^2)), sd = 1/(n/(sampler$s2) + 1/(sampler$tau)^2))
         s2 <- (rgamma(1, shape = (n+ sampler$nu)/2, rate = ((n-1) * (sampler$y_var) + n * (sampler$y_bar - sampler$mu)^2 +
                                                                 sampler$nu * sampler$initial_s2)/2))^{-1}
         sampler$mu <- mu
         sampler$s2 <- s2
         sampler$buffer_mu[i] <- sampler$mu
         sampler$buffer_s2[i] <- sampler$s2
         i <- i+1
         sampler$iteration[i] <- 0
         sampler$iteration[i+1] <- sampler$iteration[i]+1
     }
 }

 sampler <- create_sampler(y = c(24, 43, 58, 71, 43, 49, 61, 44, 67, 49, 53, 56, 59, 52, 62, 54, 57, 33, 46, 43, 57),
                           mu_0 = 42, tau_0 = 200, s2_0 = 400, nu_0 = 1, initial_mu = 1, initial_s2 = 1, buffer_length = 1000)

 update_sampler(sampler, iterations = 1000)
 mean(sampler$buffer_mu)
[1] 493.2865
 sd(sampler$buffer_mu)
[1] 34560.32
 mean(sampler$buffer_s2)
[1] 1259379447
 sd(sampler$buffer_s2)
[1] 2328320798
 hist(sampler$buffer_mu)
 hist(sampler$buffer_s2)