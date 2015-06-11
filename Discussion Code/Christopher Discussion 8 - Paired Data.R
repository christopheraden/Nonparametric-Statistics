data(mtcars)
X = mtcars$disp[mtcars$am==1] #Only the manuals
Y = mtcars$mpg[mtcars$am==1]
#Add fake data to throw off the pattern a bit and reduce sample size
X = c(X[1:10], 10)
Y = c(Y[1:10], 100)
n = length(X)
plot(Y~X, ylab="Miles per Gallon", xlab='Engine Displacement (cubic inches)')

#Pearson's
corObs = cor(Y, X); corObs
simPearsonCors = c(corObs, replicate(1E4-1, cor(X, Y[sample.int(n)])))
hist(simPearsonCors)
abline(v = corObs, col='red')
mean(simPearsonCors <= corObs) #pval

#Spearman's
corObsSpearman = cor(Y, X, method="spearman"); corObsSpearman
simSpearmanCors = c(corObsSpearman, replicate(1E4-1, cor(X, Y[sample.int(n)], method="spearman")))
hist(simSpearmanCors)
abline(v = corObsSpearman, col='red')
mean(simSpearmanCors <= corObsSpearman) #pval

#Kendall's Tau
corObsKendall = cor(Y, X, method="kendall"); corObsKendall
simKendallCors = c(corObsKendall, replicate(1E4-1, cor(X, Y[sample.int(n)], method="kendall")))
hist(simKendallCors)
abline(v = corObsKendall, col='red')
mean(simKendallCors <= corObsKendall) #pval

#Test for Regression Slope
designMatrix = model.matrix((~X))
obsSlope = coef(lm.fit(x=designMatrix, y=Y))[2]
simSlopes = c(obsSlope, replicate(1E4-1, coef(lm.fit(x=designMatrix, y=Y[sample.int(n)]))[2] ))
hist(simSlopes)
abline(v = obsSlope, col='red')
mean(simSlopes <= obsSlope) #pval
