ExactPermutationTest = function(X, Y){
  #Function takes in two samples, determines their sample sizes,
  #then computes all possible COMBINATIONS, where order doesn't matter.
  #It then calculates the mean difference, plots the permutation distribution
  # and returns all possible mean differences.  
  #Outputs all possible combinations, stored as two matrices in a list, with
  #each row of each matrix corresponding to one sample from the respective population.
  n = length(X)
  m = length(Y)
  sampMeanDiff = mean(X) - mean(Y)
  
  combinsSamp1 = t(combn(1:(n+m), n)) #All combinations of the first sample  
  #Figures out Sample 2 by finding which elements are not in Sample 1.
  combinsSamp2 = t(sapply(1:nrow(combinsSamp1), function(r) which(! 1:(n+m) %in% combinsSamp1[r,])))
  
  combined = c(X,Y) #Merge samples
  combinIndeces = cbind(combinsSamp1, combinsSamp2) #Merge sample indeces
  combinData = matrix(combined[t(combinIndeces)], ncol=(n+m), byrow=TRUE) #match samples to indeces
  #Separate the X's and Y's
  allX = combinData[ , 1:n] 
  allY = combinData[ , (n+1):(n+m)]
  D = rowMeans(allX) - rowMeans(allY) #Compute difference in means
  
  hist(D, main="Permutation Distribution for Difference in Means",
       xlim = c(1.1*min(D, sampMeanDiff), 1.1*max(D, sampMeanDiff))) #Histogram of simulated diffs, d_i.
  abline(v= sampMeanDiff, col="red") #Sample mean diff, d_obs, in red.
  return(D) #Return the d_i's
}

# How to use: ExactPermutationTest(X, Y).
# Give it the two samples, each as a vector, like this:
X = c(1, 2, 3, 9, 12)
Y = c(10, 20, 30, 40, 50, 60)
di = ExactPermutationTest(X, Y) #Takes the two samples, gives you all possible differences and plots.
sampMeanDiff = mean(X)- mean(Y) #Compute the observed sample mean difference
mean(di <= sampMeanDiff) #Calculate the p-value. 
# NOTE: You may need to change the direction of the inequality for the p-value, 
# depending on the alternative hypothesis.

# Simple Test of a Normal against another Normal
#H0: F_X (x)  = F_Y (x) (or: mu_X = mu_Y)
#HA: F_X (x) >= F_Y (x) (or: mu_X < mu_Y)
set.seed(9001) #Setting seeds allows for repeatable results.
X = rnorm(3) #N(0,1) distr
Y = rnorm(3) + 1000 #N(1000, 1) distr
di = ExactPermutationTest(X, Y)
sampMeanDiff = mean(X) - mean(Y) #Sample Mean Difference
mean(di <= sampMeanDiff) #p-value. Fail to reject H0. 
# Note: Isn't it obvious that X and Y are different? The small samples and low power from nonparametric
# methods in this case means we don't have enough evidence to reject the null. An unfortunate
# drawback to using nonparametrics in this case.

# Slightly more complicated: Comparing an Exponential against a t-distribution. 
#H0: mu_X = mu_Y
#HA: mu_X < mu_Y
set.seed(9001) #Setting seeds allows for repeatable results.
X = rexp(8, 1/3) #Simulate from Exp(1/3) distr
Y = rt(9, 4) #Simulate from a heavy-tailed t-distr
di = ExactPermutationTest(X, Y)
sampMeanDiff = mean(X) - mean(Y) #Sample Mean Difference
mean(di <= sampMeanDiff) #p-value. Fail to reject H0.