savageScores = function(X){
  #Function calculates the Savage (shifted Exponential) Scores described
  #on p.50-51 of "Introduction to Modern Nonparametrics" by James Higgins.
  #Takes in the full vector of data (the combined X and Y samples) and
  #outputs the Savage score for all of them. The output should sum to 0.
  n = length(X)
  terms = sapply(0:(n-1), function(k) 1/(n-k) ) #{1/n, 1/(n-1), 1/(n-2), ...}
  expoRanks = cumsum(terms) #{1/n, 1/n + 1/(n-1), 1/n + 1/(n-1) + 1/(n-2), ...}
  expoRanks[rank(X)] - 1 #{Maps the exponential ranks to the original dataset and returns.}
}

Permutations = function(X, Y){
  #WARNING: This function assumes no ties in calculating the Savage Scores!!

  #Function will calculate all possible permutations of two vectors, X and Y.
  #It will then calculate the Wilcoxon Rank Sum Statistic W and the Savage Scores,
  #assuming no ties. It will replace the actual numerical values of X and Y with 
  #placeholders for X and Y in the final output. 
  #Function outputs a data frame with one row for each permutation, and columns for the 
  #order of X and Y, Wilcoxon W Statistic, and Savage Score Sum for the first sample's
  #observations.
  n = length(X) #Compute first sample size
  m = length(Y) #Compute second sample size
  
  combinsSamp1 = t(combn(1:(n+m), n)) #All combinations of the first sample  
  #Find complementary combinations for second sample
  combinsSamp2 = t(sapply(1:nrow(combinsSamp1), function(r) which(! 1:(n+m) %in% combinsSamp1[r,]))) 
  
  combined = c(X,Y) #Merge samples
  combinIndeces = cbind(combinsSamp1, combinsSamp2) #Merge sample indeces
  combinData = matrix(combined[t(combinIndeces)], ncol=(n+m), byrow=TRUE) #match samples to indeces

  W = sapply(1:nrow(combinData), function(r) sum(rank(combinData[r, ])[1:n])) #Wilcoxon
  savScoreSum = sapply(1:nrow(combinData), function(r) sum(savageScores(combinData[r, ])[1:n])) #Savage Score Sums
  
  #Goes through each row of the permutation matrix, combinData, and finds which positions
  #belong to X's and which to Y's. Replaces the X values with a character "X"
  #and Y values with the character "Y". Stores the resulting character matrix
  #in an XY Position matrix.
  xyPos = matrix(NA, ncol=(n+m), nrow=nrow(combinData))
  for (r in 1:nrow(xyPos)){
    xyPos[r, rank(combinData[r,])[1:n]] = "X"
    xyPos[r, rank(combinData[r,])[(n+1):(n+m)]] = "Y"
  }
  
  #Keep XY Positions, Wilcoxon Rank Sum (Integer'ed), and Savage Score Sum (rounded to 2 places)
  PermutOut = data.frame(xyPos, as.integer(W), round(savScoreSum, 2))  
  colnames(PermutOut) = c(1:(n+m), "Wilc", "Savage Sum") #Pretty names
  #Sort by ascending Wilcoxon rank sums, then sort by lowest Savage for tied Rank Sums.
  PermutOut = PermutOut[order(PermutOut$Wilc, PermutOut$`Savage Sum`),] 
  rownames(PermutOut) = 1:choose(length(c(X, Y)), length(X))
  PermutOut
}

#Test data set:
X = c(50, 55, 62, 451, 452)
Y = c(1, 2, 3, 4, 450)
Permutations(X, Y) #Displays all permutations, and their corresponding W and Savage Sums!

#Wilcoxon Rank Sum: Calculate ranks for combined sample (X,Y), then add the ranks
#for the samples that came from X.
rank(c(X,Y)) #Y, Y, Y, Y, X, X, X, Y, X, X
W = sum(rank(c(X,Y))[1:length(X)]) # Observed W = 5 + 6 + 7 + 9 + 10 = 37
mean(Permutations(X, Y)$Wilc >= W) #P-Value for H0: F_X(x) = F_Y(x) vs HA: F_X(x) < F_Y(x)
t.test(X, Y, alternative = 'greater', var.equal = FALSE) #Corresponding t-test.
