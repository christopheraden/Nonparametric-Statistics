#Motor Trend Car Data.
data(mtcars)
mtcars = mtcars[,c("mpg", "am")] #Subset only MPG and Transmission
names(mtcars) = c("MPG", "Transmission") #Rename to something better.

#Question: Do automatics get worse fuel economy than manual transmissions?
mtcars$Transmission = factor(mtcars$Transmission, labels=c("Automatic", "Manual"))
by(mtcars, mtcars$Transmission, summary) #Get summary info for each Transmission grouping.
boxplot(MPG~Transmission, data=mtcars) #Boxplots help illuminate the difference in groups.

#Permutations Package in R
install.packages('perm')
library(perm)

#If we did EXACT permutation test on this data set, how many possible permutations would there be?
choose(19+13, 19) #347,373,600? Yikes.

#Instead, we can use a poor-man's version, where we do not enumerate every possibility.
#This is what's done in larger datasets.
#Permutation Two-Sample Test
pc = permControl(nmc=10000) #Change number of Monte Carlo samples to 10,000, for more accuracy.
permTS(MPG~Transmission, data=mtcars, exact = TRUE, control=pc) #This uses Monte Carlo, which isn't EXACTLY a permutation test.

#Plotting the Permutation Mean Differences
#The old-fashioned way: By hand, without packages.
#Pros: A better understanding of what the algorithm does, more control
#Cons: Way slower compute time, have to program it yourself.
resampleMeanDiff = function(X, n, m){
  #Takes in data set, size of first group, size of second group
  #Shuffle the data, then return a difference of means
  X = X[sample(n+m)]
  mean(X[1:n]) - mean(X[ (n+1) : (n+m) ])
}
resampleMeanDiff(mtcars$MPG, 19, 13)
resampMeanDiffs = sapply(1:1E4, function(i) resampleMeanDiff(mtcars$MPG, 19, 13))
sampleMeanDiff = mean(mtcars$MPG[mtcars$Transmission == "Automatic"]) - 
                 mean(mtcars$MPG[mtcars$Transmission == "Manual"])
hist(resampMeanDiffs)
abline(v = sampleMeanDiff, col='red') #This is where the sample mean difference is.
pval = mean(resampMeanDiffs <= sampleMeanDiff)
