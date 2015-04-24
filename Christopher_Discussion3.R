# NOTES:
# Take a look in the Computing Folder. I've uploaded some functions to 
# do permutation tests and get permutation distributions from them.

#Motor Trend Car Data.
data(mtcars)
mtcars = mtcars[,c("mpg", "am", "hp")] #Subset only MPG, Transmission, and Horsepower.
names(mtcars) = c("MPG", "Transmission", "Horsepower") #Rename to something better.
mtcars$Transmission = factor(mtcars$Transmission, labels=c("Automatic", "Manual"))

#Last week, we saw that Automatics got worse MPG than Manuals, with a permutation test. But... 
boxplot(Horsepower~Transmission, data=mtcars, main='Horsepower vs. Transmission')
cor(mtcars$MPG, mtcars$Horsepower, method="spearman")
# It seems like automatics tend to have higher horsepower, and horsepower is
# very negatively correlated to MPG. This is usually the territory of regression, but
# we aren't there yet :). Let's try restricting the comparison to only the cars 
# that have less than 100hp to make the comparison more even.
punyMtCars = mtcars[mtcars$Horsepower < 100, ] #Take only the "go-karts".

by(punyMtCars, punyMtCars$Transmission, summary) #3 automatics, 6 manuals.

# New Question: Doing a rudimentary control for horsepower,
# do automatics get worse fuel economy than manual transmissions?

# We will investigate using permutation test, Mann-Whitney, van der Waerden, and Savage Scores.

#Permutation test with complete enumeration:
library(perm)
permTS(MPG ~ Transmission, data=punyMtCars, method = "exact.ce")

#Compute the Mann-Whitney:
wilcox.test(MPG ~ Transmission, data = punyMtCars)

# Compute the van der Waerden test
library(exactRankTests) #Remember to install.packages("exactRankTests") if you haven't.
vanDerWaerden = cscores(punyMtCars$MPG, type="NormalQuantile")
manualSumVDW = sum(vanDerWaerden[punyMtCars$Transmission=='Manual'])
pperm(manualSumVDW, vanDerWaerden, 6, alternative='two.sided', simulate=TRUE) #number of manuals: 6

# Compute the Savage test.
exponentialScores = cscores(punyMtCars$MPG, type = "Savage")
manualExpoSum = sum(exponentialScores[punyMtCars$Transmission=='Manual'])
pperm(manualExpoSum, exponentialScores, 6, alternative='two.sided', simulate=TRUE)


#What about the original data?
#Monte Carlo:
pc = permControl(nmc=1E5) #Change number of simulations to 100,000.
permTS(MPG ~ Transmission, data=mtcars, exact=TRUE, control = pc)
#Mann-Whitney:
wilcox.test(MPG ~ Transmission, data=mtcars, exact=TRUE)

# Compute the van der Waerden test
vanDerWaerden = cscores(mtcars$MPG, type="NormalQuantile")
manualSumVDW = sum(vanDerWaerden[mtcars$Transmission=='Manual'])
pperm(manualSumVDW, vanDerWaerden, 13, alternative='two.sided', simulate=TRUE)

# Compute the Savage test.
exponentialScores = cscores(mtcars$MPG, type = "Savage")
manualExpoSum = sum(exponentialScores[mtcars$Transmission=='Manual'])
pperm(manualExpoSum, exponentialScores, 13, alternative='two.sided', simulate=TRUE)