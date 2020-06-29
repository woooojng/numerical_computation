  HeartAbridged <- filter(read.csv('C:/Users/woojeongkim/Desktop/Spring 2020/M520/HeartAbridged.csv'), AHD != "Converted")
 mu1 <- mean(HeartAbridged$RestBP)
 sd1 <- sd(HeartAbridged$RestBP)
 mu2 <- mean(HeartAbridged$Chol)
 sd2 <- sd(HeartAbridged$Chol)
 mu3 <- mean(HeartAbridged$MaxHR)
 sd3 <- sd(HeartAbridged$MaxHR)
 n <- 300
 m <- 10000
 sample.mean.dist1 <- data.frame(mean = replicate(m, mean(sample(HeartAbridged$RestBP, n))), n)
 sample.mean.dist2 <- data.frame(mean = replicate(m, mean(sample(HeartAbridged$Chol, n))), n)
 sample.mean.dist3 <- data.frame(mean = replicate(m, mean(sample(HeartAbridged$MaxHR, n))), n)


 ggplot(sample.mean.dist1, aes(x = mean)) +
     geom_density(fill = "skyblue") +
     stat_function(fun = dnorm, args = list(mean = mu1, sd = sd1/sqrt(300)),
                   col = 'red') +
     geom_vline(xintercept = mu1)
 ggplot(sample.mean.dist2, aes(x = mean)) +
     geom_density(fill = "skyblue") +
     stat_function(fun = dnorm, args = list(mean = mu2, sd = sd1/sqrt(300)),
                   col = 'red') +
     geom_vline(xintercept = mu2)
 ggplot(sample.mean.dist2, aes(x = mean)) +
     geom_density(fill = "skyblue") +
     stat_function(fun = dnorm, args = list(mean = mu2, sd = sd2/sqrt(300)),
                   col = 'red') +
     geom_vline(xintercept = mu2)
 ggplot(sample.mean.dist3, aes(x = mean)) +
     geom_density(fill = "skyblue") +
     stat_function(fun = dnorm, args = list(mean = mu3, sd = sd3/sqrt(300)),
                   col = 'red') +
     geom_vline(xintercept = mu3)

> ggplot(sample.mean.dist1, aes(sample = mean)) +
+     geom_qq() + geom_qq_line()
> ggplot(sample.mean.dist2, aes(sample = mean)) +
+     geom_qq() + geom_qq_line()
> ggplot(sample.mean.dist3, aes(sample = mean)) +
+     geom_qq() + geom_qq_line()


