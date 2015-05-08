#NEWS: "Permutation Samples Output.R" in the Computing folder will produce
# a table with every possible permutation of the data, the Wilcoxon Rank Sum statistic
# and the Savage score for that permutation. This is useful for developing p-values.


###QUESTION 2
#Continuing the sample midterm review, we'll move on past Hodges-Lehmann now.
#NOTE: The data has been changed since last week. This new data has no ties. 
#You may assume that there will be no ties on the exam.
S1 = c(4, 6, 7, 9, 11, 13, 14, 30)
S2 = c(5, 8, 10, 12)
n = length(S1); n
m = length(S2); m

#Part F: Savage Scores
#Load the SavageScores R file. This command assumes SavageScores.R is in the same folder as this file!
source("SavageScores.R")
dataWithSavageScores = data.frame(Label=rep(c("S1", "S2"), times=c(n,m)), #Repeat Labels as many times as there are samples in each group
  Data=c(S1, S2), SavageScore=round(savageScores(c(S1, S2)),3)) #Show data next to Savage Scores, too.
#Take Savage Scores that belong to Sample One and add them all together.
obsSavageSum = sum(dataWithSavageScores$SavageScore[dataWithSavageScores$Label=="S1"]); obsSavageSum

source("Permutation Samples Output.R") #Source the table-making function I wrote and run it on our data.
allPerms = Permutations(S1, S2)
allPerms[order(allPerms$`Savage Sum`),] #Display all permutations, sorted from lowest Savage Sum to highest.

#Calculate p-value: Number of permutations with a greater savage score than ours.
#If mu_y > mu_x, most of the Y's should be bigger than the X's, so we'd expect to see a pattern
#like XXXXXYYYY. This means we'd see very SMALL savage sums under the alternative. Thus, we reject if
#the savage sum is small.
pval = mean(allPerms$`Savage Sum` <=  obsSavageSum); pval

#Part G: Siegel-Tukey
sd(S1); sd(S2)
labelData = data.frame(Label=rep(c("S1", "S2"), times=c(n,m)), Data=c(S1,S2))
labelData = labelData[order(labelData$Data), ] #Sort according to Data value.

#Give value 1 to the smallest, 2 to the largest, 3 to second largest, 4 to second smallest, etc...
dataWithST = data.frame(labelData, STRank = c(1,4,5,8,9,12,11,10,7,6,3,2)); dataWithST
dataWithST[dataWithST$Label=="S1", ] #Subset: Take only the obs that are in sample 1. 
siegelTukeyS1Sums = sum(dataWithST$STRank[dataWithST$Label=="S1"]); siegelTukeyS1Sums #Sum their Siegel-Tukey Ranks.

#Part H: Kolmogorov-Smirnov
F1 = ecdf(S1) #Empirical CDF of sample 1.
F2 = ecdf(S2) #Empirical CDF of sample 2.
combinedData = c(S1, S2)[order(c(S1, S2))] #Sorted and merged samples.
F1(combinedData) #Empirical CDF of Sample 1, evaluated at all sample points.
F2(combinedData) #Ditto for Sample 2.
#All information in one table, with absolute difference included. KS Statistic is the max abs diff.
KSdata = data.frame(x = combinedData, F1x = F1(combinedData), F2x = F2(combinedData), 
           AbsDiff = abs(F1(combinedData) - F2(combinedData))) 
KS = max(KSdata$AbsDiff); KS
#Pvalues are calculated either through permutation methods, or through Binomial Distribution.
#For this class, don't worry about how to calculate p-value, unless permutation table given. 
#For critical value, there is a formula:
#Reject if KS > c_alpha * sqrt( {n+m} / {n*m} ), where c_alpha is given by a table.