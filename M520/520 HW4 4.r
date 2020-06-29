set.seed(520)
y1 <- sample(HeartAbridged$RestBP, 300) # From our data set, get the sample
y1bar <- mean(y1) # calculating the sample mean
sd1 <- sd(y1)
z_aDiv2_1 <- qnorm(1-0.005) # To get the 99% confidence interval, getting the interval z-score
interval1 <- y1bar + c(-1,1)*z_aDiv2_1*sd1/sqrt(300) # Calulating the 99% confidence interval by using formula
interval1
[1] 129.0983 134.3551

set.seed(520)
y2 <- sample(HeartAbridged$Chol, 300) # From our data set, get the sample
y2bar <- mean(y2) # calculating the sample mean
sd2 <- sd(y2)
z_aDiv2_1 <- qnorm(1-0.005) # To get the 99% confidence interval, getting the interval z-score
interval2 <- y2bar + c(-1,1)*z_aDiv2_1*sd2/sqrt(300) # Calulating the 99% confidence interval by using formula
interval2
[1] 238.6855 254.0878

set.seed(520)
y3 <- sample(HeartAbridged$MaxHR, 300) # From our data set, get the sample
y3bar <- mean(y3) # calculating the sample mean
sd3 <- sd(y3)
z_aDiv2_1 <- qnorm(1-0.005) # To get the 99% confidence interval, getting the interval z-score
interval3 <- y3bar + c(-1,1)*z_aDiv2_1*sd3/sqrt(300) # Calulating the 99% confidence interval by using formula
interval3
[1] 146.1142 152.9191


• So our interval is (3491.89, 4597.68).
• Our population mean is μ = 3932.8.
• μ is inside the interval, so the probability that μ is inside the calculated interval interval is 1, not 0.95.


m <- 1000
fun_interval <- function(data, n, m, alpha){
# getting the interval z-score
z_aDiv2 <- qnorm(1-alpha/2)
# Getting population parameters
mu <- mean(data); sigma <- mean(data)
# In R, it is best to create a blank vector that will contain your output when
# using a for loop. R is slow with for loops if you don't do this.
containMu <- rep(NA, m)
for(i in 1:m){
# Getting the sample
y <- sample(data, n)
# calculating the sample mean
ybar <- mean(y)
#remember, we have sigma, the standard deviation of our "population" of diamonds
# Calulating the interval
interval <- ybar + c(-1,1)*z_aDiv2*sigma/sqrt(n)
# Check if mu is in the interval and store the resulting TRUE/FALSE in the i-th
# element of containMu
containMu[i] <- ((mu < interval[2]) && (mu > interval[1]))
}
return(mean(containMu))
}


set.seed(520)
> fun_interval(HeartAbridged$RestBP, 300, 1000, 0.99)
[1] 0.633
> fun_interval(HeartAbridged$Chol, 300, 1000, 0.99)
[1] 0.482
> fun_interval(HeartAbridged$MaxHR, 300, 1000, 0.99)
[1] 0.565

 HeartAbridged <- filter(read.csv('C:/Users/woojeongkim/Desktop/Spring 2020/M520/HeartAbridged.csv'), AHD != "Converted")
> mu1 <- mean(HeartAbridged$RestBP)
> t1 <- t.test(HeartAbridged$RestBP, conf.level = 0.99)
> t2 <- t.test(HeartAbridged$Chol, conf.level = 0.99)
> t3 <- t.test(HeartAbridged$MaxHR, conf.level = 0.99)
> kable(data.frame(Bounds = c("Lower", "Upper"), RestBP = t1$conf.int, Chol = t2$conf.int, MaxHR = t3$conf.int))

|Bounds |   RestBP|     Chol|    MaxHR|
|:------|--------:|--------:|--------:|
|Lower  | 129.0688| 238.9825| 146.2008|
|Upper  | 134.3107| 254.4036| 153.0138|

2.
