 Function produces coverage results for two sample confidence intervals
THe coverage is reported for the pooled and Welch procedures
 Arguments ###
 n1 the sample size to be simulated for group 1
 s1 the population standard deviation (sigma) for the normal distribution from which the samples comefrom
 n2 the sample size to be simulated for group 2
 s2 the population standard deviation (sigma) for the normal distribution from which the samples comefrom

 alpha The value for alpha used for the confidence intervals
 M the number of samples to be simulated. Recommended minimum is 10,000

ci.sim <- function(n1, s1, n2, s2, alpha, M){
Coverage <- data.frame(pooled = rep(NA, M), welch = rep(NA, M))
for(i in 1:M){
mu1 = 0; mu2 = 0
y1 <- rnorm(n1, mu1, s1)
y2 <- rnorm(n2, mu2, s2)
pooled.ci <- t.test(y1, y2, conf.level = 1-alpha, var.equal = TRUE)$conf.int
welch.ci <- t.test(y1, y2, conf.level = 1-alpha, var.equal = FALSE)$conf.int
pooled <- (((mu1 - mu2) > pooled.ci[1]) & ((mu1 - mu2) < pooled.ci[2]))
welch <- (((mu1 - mu2) > welch.ci[1]) & ((mu1 - mu2) < welch.ci[2]))
Coverage[i,] = c(pooled,welch)
}
return(data.frame(pooled.E = mean(Coverage$pooled),
welch.E = mean(Coverage$welch)))
}

> ci.sim <- function(n1, s1, n2, s2, alpha, M){
+     Coverage <- data.frame(pooled = rep(NA, M), welch = rep(NA, M))
+     for(i in 1:M){
+         mu1 = 0; mu2 = 0
+         y1 <- rnorm(n1, mu1, s1)
+         y2 <- rnorm(n2, mu2, s2)
+         pooled.ci <- t.test(y1, y2, conf.level = 1-alpha, var.equal = TRUE)$conf.int
+         welch.ci <- t.test(y1, y2, conf.level = 1-alpha, var.equal = FALSE)$conf.int
+         pooled <- (((mu1 - mu2) > pooled.ci[1]) & ((mu1 - mu2) < pooled.ci[2]))
+         welch <- (((mu1 - mu2) > welch.ci[1]) & ((mu1 - mu2) < welch.ci[2]))
+         Coverage[i,] = c(pooled,welch)
+     }
+     return(data.frame(pooled.E = mean(Coverage$pooled),
+                       welch.E = mean(Coverage$welch)))
+ }



> ci.sim(100, 5, 100, 5, 0.99, 10000)
  pooled.E welch.E
1   0.0091  0.0091
> ci.sim(500, 5, 500, 5, 0.99, 10000)
  pooled.E welch.E
1   0.0095  0.0095
> ci.sim(500, 10, 500, 10, 0.99, 10000)
  pooled.E welch.E
1   0.0126  0.0126
> ci.sim(500, 10, 100, 10, 0.99, 10000)
  pooled.E welch.E
1   0.0097  0.0094
> ci.sim(1000, 5, 1000, 5, 0.99, 10000)
  pooled.E welch.E
1   0.0118  0.0118
> ci.sim(1000, 5, 5000, 5, 0.99, 10000)
  pooled.E welch.E
1   0.0116  0.0116
> ci.sim(1000, 5, 4000, 10, 0.99, 10000)
  pooled.E welch.E
1   0.0148  0.0109
> ci.sim(1000, 5, 1000, 10, 0.99, 10000)
  pooled.E welch.E
1   0.0108  0.0108
> ci.sim(1000, 10, 1000, 5, 0.99, 10000)
  pooled.E welch.E
1   0.0097  0.0097