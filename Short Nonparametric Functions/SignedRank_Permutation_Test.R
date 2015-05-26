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

##EXAMPLE
# X = c(26.5, 15, 18.2, 19.5, 23.1, 17.3)
# Y = c(16.5, 15.8, 14.1, 30.2, 25.1, 17.4)
# SR.Perms = SignedRank.Permutation.Test(X,Y)