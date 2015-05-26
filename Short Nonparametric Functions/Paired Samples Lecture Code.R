X = c(35, 38, 32, 39, 80, 5, 55)
Y = c(13, 15, 19, 25, 81, 14, 6)
plot(1:7, X-Y, main="X-Y Differences")
abline(h = 0)

#H0: mu_X - mu_Y = 0
#HA: mu_X - mu_Y > 0

#Homemade Monte Carlo permutation test.
Paired.Permutation.Test = function(X,Y, nsim=1E3){
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
sim = Paired.Permutation.Test(X, Y, nsim=1E4)
meanDiff = mean(X) - mean(Y)
mean(sim >= meanDiff)

#Another permutation test.
library(exactRankTests)
perm.test(X, Y, paired=TRUE, alternative = "greater")

#By Wilcoxon Signed Rank Test.
wilcox.test(X, Y, paired = TRUE, alternative = "greater")
