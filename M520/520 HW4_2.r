1-b

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

> p1 <- ggplot(HeartAbridged, aes(x = RestBP, color = AHD, fill = AHD)) +
+     geom_density(alpha = 0.6) +
+     xlim(c(70,180))

> p2 <- ggplot(HeartAbridged, aes(x = Chol, color = AHD, fill = AHD)) +
+     geom_density(alpha = 0.6) +
+     xlim(c(140, 340))

> p3 <- ggplot(HeartAbridged, aes(x = MaxHR, color = AHD, fill = AHD)) +
+     geom_density(alpha = 0.6) +
+     xlim(c(80, 220))

> grid.arrange(p1, p2, p3)

<GRAPHS1, 2, 3>

1.c
Well, nothing looks all that great in terms of clear separation, however let’s investigate the nWBV data of
each AHD for the heck of it.

#Yes/No AHD
Yes <- filter(HeartAbridged, AHD == "Yes")
No <- filter(HeartAbridged, AHD == "No")

t.test(Yes$Chol, conf.level = 0.99)$conf.int
[1] 240.5115 262.4382
attr(,"conf.level")
[1] 0.99

t.test(Yes$RestBP, conf.level = 0.99)$conf.int
[1] 130.4102 138.7264
attr(,"conf.level")
[1] 0.99

t.test(Yes$MaxHR, conf.level = 0.99)$conf.int
[1] 134.2537 144.2643
attr(,"conf.level")
[1] 0.99



#Looking at the confidence intervals, there does appear to be a bit of crossover from the two AHDs. According
to these confidence intervals, it is plausible that the two AHDs have similar population means.
From a statistical perspective, this means that "Chol" should not be used to classify people as having dementia
or not.
But both confidence intervals only use information from each AHD individually.
However, both AHDs contain information about "Chol", a single variable. We will go over how to aggregate
the information about "Chol" from both AHDs to discern more precisely if the AHD means differ.



y1 <- HeartAbridged$Chol[HeartAbridged$AHD == 'No']
ybar1 <- mean(y1); s1 <- sd(y1); n1 <- length(y1)
y2 <- HeartAbridged$Chol[HeartAbridged$AHD == 'Yes']
ybar2 <- mean(y2); s2 <- sd(y2); n2 <- length(y2)


Statistics Nondementia Dementia
Mean 0.746 0.724
SD 0.039 0.031
n 72.000 64.000
We will calculate both versions of the two-sample confidence intervals.
1 = 2
The 99% pooled confidence interval would be:
spsq <- ((n1 - 1)*s1^2 + (n2 - 1)*s2^2)/(n1 + n2 - 2)
sp <- sqrt(spsq)
pooled.ci <- (ybar1 - ybar2) + c(-1, 1)*qt(.995, df = n1 + n2 - 2)*sp*sqrt(1/n1 + 1/n2)
pooled.ci
[1] -24.278104   6.608951


#This indicates that the mean nWBV of a specific population of individuals without dementia is between
#0.0058 and 0.0376 larger than the population with dementia. This doesn’t seem like much but let’s consider
#the scale of things.

1-d

 #lower bound
> 0.0058/ybar2
[1] 2.306394e-05
> 0.0376/ybar2
[1] 0.000149518

#Those without dementia could have a nWBV that’s just 0.8% larger or up to 5% larger than those with
dementia, with 99% confidence.
Is this of practical importance? Unfortunately, I’m not that kind of doctor so I can’t be sure. But, I do think
that any characteristic that helps detect dementia or the onset of dementia is important.
The confidence interval indicates that there is something physical/concrete that detectably disnguishes
dementia from nondementia. It is important to note that this is not implying that smaller nWBV values
cause dementia. Just that there may be some connection.


1 6= 2
Next we will compare the pooled (not so good usually) standard deviation confidence interval to the non-pooled
(good) one.
First let’s get the degrees of for the Welch confidence interval
11
### Calculating degrees of freedom
a1 <- s1^2/n1; a2 <- s2^2/n2
df = (a1 + a2)^2/(a1^2/(n1-1) + a2^2/(n2-1))
df
[1] 298.64
## [1] 133.0172
#For the pooled procedure the degrees of freedom would be 134, but for this procedure the degrees of freedom
used would be 133.0171865.
Not much of difference here but when there are larger discrepancies in samples sizes or standard deviations
between the groups.
Then plug in that degrees of freedom to the confidence interval
welch.ci <- (ybar1 - ybar2) + c(-1, 1)*qt(.995, df = df)*sqrt(s1^2/n1 + s2^2/n2)
welch.ci
[1] -24.180737   6.511585
## [1] 0.005974586 0.037435716
#The important difference between these two confidence intervals is in their Margins of Error, i.e., how
much you add and substract by. The larger the margin of error, the wider the confidence interval. So smaller
is better!
#### Margins of error
pooled.E <- qt(.995, df = n1 + n2 - 2)*sp*sqrt(1/n1 + 1/n2)
welch.E <- qt(.995, df = df)*sqrt(s1^2/n1 + s2^2/n2)
pooled.E
[1] 15.44353
## [1] 0.01591886
welch.E
[1] 15.34616
## [1] 0.01573056
pooled.E/welch.E
[1] 1.006345
## [1] 1.01197
#It appears that the pooled procedure produces a confidence interval has a margin of error that is 1% larger.
This would mean that its true coverage/confidence level would be a bit higher than 99%.
