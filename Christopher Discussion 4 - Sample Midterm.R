##Question 1
y = c(1.2, 1.3, 1.4, 1.6, 1.65, 1.8, 1.9, 2, 2.2, 2.25, 2.5, 2.6, 2.7, 2.9, 3, 3.2, 4, 5, 8, 10) #The data
#Part A
mean(y); sd(y); median(y) #You've seen these before. Mean, Standard Deviation, and Median! 
#Note difference between mean and median. Skewed data.

#Part B
n = length(y); n #20
tTest = sqrt(n) * (mean(y) - 3) / sd(y); tTest
pt(tTest, n-1) #pval: P(t_{19} <= .119) = .547

#Part C
#95% CI based on t. Very similar to test in Part B. 
lower = mean(y) - qt(.975, n-1) * (sd(y) / sqrt(n)); lower
upper = mean(y) + qt(.975, n-1) * (sd(y) / sqrt(n)); upper

#Part D
#If the null is true, half the data is above the median
#The number of obs that are bigger than median is Binomial(n, 0.5).
sum(y > 3.0) #Only 5 out of 20 are bigger than 3.
binom.test(5, 20, .5, alternative='less')
pbinom(5, 20, .5) #p = P(#BiggerThanMed <= 5) = P(X=5) +...+ P(X=0)

#Part E
a = (.5*n) - (sqrt(.25*n) * qnorm(.975)); a #Normal Approximation of the Rank of the CI lower bound.
b = (1 + .5*n) + (sqrt(.25*n) * qnorm(.975)); b #Normal Approximation of the Rank of the CI upper bound.
y[c(floor(a), ceiling(b))] #CI. Remember, round down the lower bound, round up the upper bound!


###Question 2
S1 = c(5, 6, 8, 9, 10, 12, 14, 30)
S2 = c(7, 8, 9, 10)
n = length(S1); n
m = length(S2); m

#Part A
mean(S1); mean(S2) #Means of the two samples
median(S1); median(S2) #Medians of the two samples

#Part B
#To show the variances are unequal, look at their ratio. It's huge!
var(S1) / var(S2) # Ratio = 37.84. They are very different. 
#Need to approximate the degrees of freedom and use a different denominator than the pooled t-test.
numer = mean(S1) - mean(S2)
denom = sqrt( var(S1)/n + var(S2)/m  )
tTest = numer / denom; tTest
DF = { (var(S1)/n + var(S2)/m)^2 } / { (var(S1)/n)^2 / (n-1) + (var(S2)/m)^2 / (m-1) }; DF 
qt(.95, DF) #Critical value for this effective degrees of freedom.
#On the test, calculate the approximate DF, then report the critical values that sandwich those DF.
#For example, DF=7.7 here, so report these criticals
qt(.95, 7:8) #1.894579, 1.859548. In either case, we fail to reject H0.

#Part C
#CI procedure is similar to Question 1. Note: This is a CI for mu_x - mu_y! 
#To get mu_y - mu_x, take the interval and multiply each bound by -1. 
lower = numer - qt(.975, DF) * denom; lower
upper = numer + qt(.975, DF) * denom; upper
#muX - muY: (-3.4, 9.9), so muY-muX would be (-9.9, 3.4).

#Part D: Wilcoxon
cbind(Data = c(S1, S2), Ranks = rank(c(S1, S2))) #Calculate rank of combined data, add up the ranks of Sample 1.
W = 1 + 2 + 4.5 + 6.5 + 8.5 + 10 + 11 + 12; W
#Look up in Table A3, n=8, m=4. 5% upper: 63.
#W = 55.5 < 63 = CriticalValue, so fail to reject H0.

#Part E:
as.numeric(outer(S2,S1,"-")) #Computes All pairwise diffs
median(as.vector(outer(S2,S1,"-"))) #Hodges-Lehmann Estimate
#Look up values in A4. lower = 5, upper = 27. So we take the (5+1) = 6th-ranked diff and 27th-ranked diff
sort(as.vector(outer(S2,S1,"-")))[c(6,27)] #Sort the pairwise diffs and take the 6th and 27th to make 95% CI.