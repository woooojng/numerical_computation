1c


1d


HeartAbridged Example p.12 in Lec11pt2
We are comparing the mean AHD of a population of dementia patients μD to the mean of a population of
nondementia patients μD.
H0 : μN = μD
H1 : μN 6= μD
We will use  = 0.01 to make a decision.
The test statistic is
ts = s(yN−yD)
s2
N
n1
+
s2
D
n2
= (0.7460624 − 0.7243573 r
0.0386552
72 + 0.03147642
64
= 3.6058448.
with 133.0171865 degrees of freedom.
The p-value is
p = 2 · P(T  |3.6058448|)
= 4.3884987 × 10−4
There are now two scenarios, using the spectrum and the decision. For a decision based conclusion, note that
the p < , therefore our decision would be to reject the null hypothesis.
• Decision: There is statistically significant evidence that the mean AHD of patients without dementia
is higher than those with dementia (p = 0.0004).
• Spectrum: There is very strong evidence that the mean AHD of patients without dementia is higher
than those with dementia (p = 0.0004).


# filter() only keeps rows where the condition is true
# this gets rid of those that were diagnosed with dementia during the study
# I have arbitrarly decided that's not our objective
HeartAbridged <- filter(read.csv('C:/Users/woojeongkim/Desktop/Spring 2020/M520/HeartAbridged.csv'), AHD != "Converted")
t.test(Chol ~ AHD, HeartAbridged)$conf.int
[1] -20.484170   2.815018
attr(,"conf.level")
[1] 0.95
## [1] -0.033611348 -0.009798954
## attr(,"conf.level")
## [1] 0.95


yY <- HeartAbridged$Chol[HeartAbridged$AHD == "Yes"] # Get the vector of Yes-AHD in Chol
yYbar <- mean(yY) # Get the sample mean

yN <- HeartAbridged$Chol[HeartAbridged$AHD != "Yes"] # Get the vector of No-AHD in Chol
yNbar <- mean(yN)

ybar_diff <- yNbar - yYbar
 yYbar
[1] 251.4748
 yNbar
[1] 242.6402
 ybar_diff
[1] -8.834576

#The mean Chol for nondementia patients is 0.7461 and the mean Chol for dementia patients is 0.7244.
The difference then is yN − yD = 0.0217
As discussed before but is the difference big enough to say that the AHDs differ with a high level of certainty?
And how will we measure that certainty.
Let’s look at a basic method first.

...

### Getting AHD sizes

nY <- length(HeartAbridged$Chol[HeartAbridged$AHD == "Yes"]) # Yes in AHD size

nN <- length(HeartAbridged$Chol[HeartAbridged$AHD != "Yes"]) # No in AHD size

### For excruciating detail, run the command ?t.test
### Two ways



# The first method is for two individual vectors
t.test(yN, yN)

	Welch Two Sample t-test

data:  yY and yN
t = 1.4924, df = 298.64, p-value = 0.1366
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -2.815018 20.484170
sample estimates:
mean of x mean of y 
 251.4748  242.6402 


##
## Welch Two Sample t-test
##
## data: yN and yD
## t = 3.6058, df = 133.02, p-value = 0.0004388
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
## 0.009798954 0.033611348
## sample estimates:
## mean of x mean of y
## 0.7460624 0.7243573

t.test(yN, yD, conf.level = .99)

	Welch Two Sample t-test

data:  yN and yY
t = -1.4924, df = 298.64, p-value = 0.1366
alternative hypothesis: true difference in means is not equal to 0
99 percent confidence interval:
 -24.180737   6.511585
sample estimates:
mean of x mean of y 
 242.6402  251.4748 



##
## Welch Two Sample t-test
##
## data: yN and yD
## t = 3.6058, df = 133.02, p-value = 0.0004388
## alternative hypothesis: true difference in means is not equal to 0
## 99 percent confidence interval:
## 0.005974586 0.037435716
## sample estimates:
## mean of x mean of y
## 0.7460624 0.7243573
# 2, one vector of data and another with AHD label using a "formula"
# Formula are in the form y ~ x
# y is the response variable, x is the predictor/AHDing variable.


t.test(Chol ~ AHD, data = HeartAbridged)

	Welch Two Sample t-test

data:  Chol by AHD
t = -1.4924, df = 298.64, p-value = 0.1366
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -20.484170   2.815018
sample estimates:
 mean in group No mean in group Yes 
         242.6402          251.4748 
##
## Welch Two Sample t-test
##
## data: Chol by AHD
## t = -3.6058, df = 133.02, p-value = 0.0004388
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
## -0.033611348 -0.009798954
## sample estimates:
## mean in AHD Demented mean in AHD Nondemented
## 0.7243573 0.7460624
# the AHDing variable can only have two possible AHDs.
# without eliminating the Converted AHD from the data, we would get an error.

#Using 2nd method to test hypothesis by using t-test
t.test(Chol ~ AHD, HeartAbridged, conf.level = .99)$conf.int
t.test(Chol ~ AHD, HeartAbridged, conf.level = .99)$conf.int
t.test(Chol ~ AHD, HeartAbridged, conf.level = .99)$conf.int



yY2 <- HeartAbridged$RestBP[HeartAbridged$AHD == "Yes"] # Get the vector of Yes-AHD in RestBP
yYbar2 <- mean(yY2) # Get the sample mean

yN2 <- HeartAbridged$RestBP[HeartAbridged$AHD != "No"] # Get the vector of No-AHD in RestBP
yNbar2 <- mean(yN2)

ybar_diff2 <- yNbar2 - yYbar2
 yYbar2
[1] 134.5683
 yNbar2
[1] 242.6402
 ybar_diff2
[1] -8.834576


t.test(yN, yD, conf.level = .99)
