#There is another function in this folder called "Exact_Permutation_Test.R". 
#It will enumerate every possible permutation of the data you provide it.
#For large enough data sets (even n=11, m=11 is pushing it), the exact approach
#is too slow. This method is called Monte Carlo. Instead of generating all possible
#permutations, it will only do a subset of the total number. The rest of the math is
#identical, though the denominator in the p-value is the number of d_i, which will not be "n+m choose n".
#I have written this function to generate identical output as the Exact Permutation Test.

ApproxPermutationTest = function(X, Y, nsim=1E5){
  # Function takes in two samples, determines their sample sizes,
  # then samples, WITH REPLACEMENT, from the combined data, 
  # nsim number of times (100,000 by default). This
  # means that it is possible to get identical datasets, and thus,
  # identical differences in means. We cannot get duplicate datasets
  # in the exact permutation test method.

  # It then calculates the mean differences, plots the permutation distribution
  # and returns the mean differences.  
  n = length(X) #Calculates the number of obs in sample 1
  m = length(Y) #Calculates the number of obs in sample 2
  combined = c(X,Y) #Merge samples
  
  #Sample the data, then put them into variable, with each row being one simulated dataset
  combinData = t(sapply(1:nsim, function(i) sample(combined, replace=FALSE))) 
  
  allX = combinData[ , 1:n] #Separate the X's from the big matrix
  allY = combinData[ , (n+1):(n+m)] #...and the Y's.
  D = rowMeans(allX) - rowMeans(allY) #Compute all differences in means
  sampMeanDiff = mean(X) - mean(Y) #Compute the observed difference in means, d_obs
  hist(D, main="Permutation Distribution for Difference in Means",
       xlim = c(1.1*min(D, sampMeanDiff), 1.1*max(D, sampMeanDiff))) #Histogram of simulated diffs, d_i.
  abline(v= sampMeanDiff, col="red") #Sample mean diff, d_obs, in red.
  return(D) #Return vector of differences, needed to calculate a p-value.
}

# How to use: ApproxPermutationTest(X, Y).
# nsim is an optional argument that tells the function how many times to resample
# the data. By default, it's 100,000. Assign the function call to a variable, since
# this is where it'll store the differences. You can then use it to calculate p-values.

#EXAMPLE: Testing for a difference in means between automatics and manuals.
#Alternative: Manuals have better MPG.
data(mtcars)
mtcars = mtcars[,c("mpg", "am")] #Subset only MPG and Transmission
Automatics = mtcars[mtcars$am == 0, "mpg"] #MPG for all Automatics
Manuals = mtcars[mtcars$am == 1, "mpg"] #MPG for all Manuals
sampleMeanDiff = mean(Automatics) - mean(Manuals)

di = ApproxPermutationTest(Automatics, Manuals) #Optional argument: nsim.
pval = mean(di <= sampleMeanDiff)
