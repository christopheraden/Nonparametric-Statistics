#NEWS: "TukeyHSD.R" will prove useful on the second homework problem. 
# "JonckheereTerpstra.R" may also be useful, but I've copied it into this document.

##PROBLEM 1
source("~/Dropbox/School/STA 104/Computing/TukeyHSD.R")
##EXAMPLE DATA, taken from Page 95
loc1 = c(26.5, 15, 18.2, 19.5, 23.1, 17.3)
loc2 = c(16.5, 15.8, 14.1, 30.2, 25.1, 17.4)
loc3 = c(19.2, 21.4, 26, 21.6, 35, 28.9)
loc4 = c(26.7, 37.3, 28, 30.1, 33.5, 26.3)

x = c(loc1, loc2, loc3, loc4)
grp = rep(1:4, times=c(6, 6, 6, 6))
HSDs = HSD.Perm(x, grp, nsim = 1E3) #Takes awhile.
quantile(x = HSDs$Qdist, probs = c(.5, .75)) #Median and 75th percentile for Permutation HSD.


##PROBLEM 2
#Let's use the same data set as before.
library(clinfun) #If you don't have this, do install.packages("clinfun") to download it, then run this line.

#Jonckheere requires the groups be given as NUMERIC or an ordered factor. Give it numbered labels.
#Figure out the alternative based on what the problem asks.
jonckheere.test(x, grp, alternative = "increasing", nperm = 1E4)

##PROBLEM 4
X = c(35, 38, 32, 39)
Y = c(13, 15, 19, 25)
#H0: mu_X - mu_Y = 0
#HA: mu_X - mu_Y > 0
Paired.Permutation.Test = function(X,Y, nsim=1E4){
  #Takes two paired samples X and Y, then randomly flips the signs of 
  #the differences. Returns the mean differences of each flipped sample.
  n = length(X)
  if(length(Y) != n) stop("Length of X and Y must be equal.")
  diffs = X-Y
  signs = matrix(sample(c(-1,1), size = nsim*n, replace=TRUE), nrow=nsim, ncol=n)
  simMeans = sapply(1:nsim, function(r) mean(diffs * signs[r, ] ))
  hist(simMeans, main="Histogram of Paired Mean Differences, Observed in Red", xlab="Mean Difference")
  abline(v=mean(diffs), col='red')
  simMeans
}
sim = Paired.Permutation.Test(X, Y)

##PROBLEM 5
X = c(35, 38, 32, 39)
Y = c(13, 15, 19, 25)
SignedRank.Permutation.Test = function(X,Y, nsim=1E3){
  #Takes two paired samples X and Y, shuffles their signs nsim times,
  #then computes the signed rank test. Generates a histogram of the 
  #permutations and puts the observed statistic in red on the histogram.
  #Returns all permutation samples, necessary for calculating p-values.
  n = length(X)
  if(length(Y) != n) stop("Length of X and Y must be equal.")
  signs = matrix(sample(c(-1,1), size = nsim*n, replace=TRUE), nrow=nsim, ncol=n)
  simWilcox = sapply(1:nsim, function(r) ( wilcox.test(x = X * signs[r, ], y = Y * signs[r, ], paired=TRUE)$statistic ))
  
  hist(simWilcox, main="Histogram of Wilcoxon Signed Rank Tests, Observed in Red", xlab="Signed Rank Statistic")
  abline(v = wilcox.test(X, Y, paired=TRUE)$statistic, col='red')
  simWilcox
}
signedRankPermutations = SignedRank.Permutation.Test(X, Y)

##PROBLEM 6, Exercise 6
#Let's use data from before, but pretend this data was PAIRED. Let's do the sign test.
X = c(26.5, 15, 18.2, 19.5, 23.1, 17.3)
Y = c(16.5, 15.8, 14.1, 30.2, 25.1, 17.4)
n = length(X)
plot(1:n, X-Y)
abline(h=0, col='red')
numXBigger = sum(X>Y)
#Test H0: mu_D = 0 versus HA: mu_D = muX-muY > 0.
#If mu_X - mu_Y >> 0, we'd expect many more X's to be bigger than their Y's.
#Thus, #(X > Y) would be larger than we observed.
pval = pbinom(numXBigger-1, n, .5, lower.tail = FALSE); pval
#Why did I subtract 1 from #(X > Y)?
#Recall that the binomCDF will do Pr(X<=c). lower.tail = FALSE means I calculate Pr(X>c), 
#when I really want Pr(X>=c). Thus, Pr(X>c-1) = Pr(X>=c) = p-value.