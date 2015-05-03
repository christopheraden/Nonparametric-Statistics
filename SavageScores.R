# The code in the exactRankTests package seems to be incorrect for Savage Scores.
# I have written my own version and confirmed that it gives the correct outputs.
# This function, however, assumes that there are no ties in the data.

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