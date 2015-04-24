#Intro to R:
x = 10 #Assigning variables
x #Printing the value of the variable
x = c(1, 2, 11, 24) #A vector of values
x #Value changed. "x" is no longer 10.
x > 5 #Tests if EACH value is greater than 5.
as.numeric(x>5) #If true, makes value 1, else, false.
sum(x>5) #Number of values where x > 5.

mean(x) #Takes the average of all x values.
sd(x) #Standard deviation.
sqrt(x) #Square root each entry.
sum(x) #Sum of all elements.
n = length(x) #Tells us how many elements there are.
sort(x) #Sort the values of x, smallest to largest.
order(x) #Gives the rankings of the elements. 1 is to minimum of the vector.

M = matrix(x, nrow=2, ncol=2) #Makes the values into a matrix. Fills in column by column.
M #Print the matrix to the screen
as.data.frame(M) #Converts matrices into data frames.

#Some stats
alpha = .05 #Set type I error rate.
qnorm(1-alpha/2) #Gives the quantile of the normal distribution.
binom.test(2, 12, p=.5) #An exact binomial test--how odd is 2 successes out of 12 if H0: p=0.5?
?binom.test

###
#Some homework help
###

#Make a dataset. This is NOT the same as the homework.
rainfall = c(24.3, 20.6, 17.6, 23.0, 27.2, 28.5, 32.8, 
            28.2, 25.9, 19.5, 27.2, 31.1, 28.7, 24.8, 
            24.3, 27.1, 30.6, 26.8, 18.9, 36.3, 28.0)
n = length(rainfall) #How many observations are there?

#Asymptotic Confidence for Median, from the book, alpha = .05.
a = (.5*n) - (sqrt(.25*n) * qnorm(.975))
b = (1 + .5*n) + (sqrt(.25*n) * qnorm(.975))
#This is the ORDER of the confidence interval. To get the CI find
#the corresponding value from the sample. a and b are not always integers, so you need to round them.
#This code may NOT work. You need to figure out which way to round a and b.
#To round up, use ceiling(x). To round down, use floor(x), where x is the value. 
#For example, ceiling(7.49) = 8, and floor(9.999) = 9.
sort(rainfall)[c(a,b)] #Gives the value of the sample corresponding to the order statistic a and b.

#Exact
find_a_and_b = function(a, b, n) {
  #Function takes integers a and b, representing the lower and upper upper statistic bounds
  #as well as the number of trials, n, and outputs the probability of a binomial lying between
  #those two values if probability of success was 1/2. 
  k = a : (b-1)
  sum(choose(n,k) * .5^n)
}

#Remember to use the ROUNDED versions of a and b. Code will not work if a and b are not whole numbers!!
find_a_and_b(a, b, n) #Should confirm the results pretty well

#What if we assumed normality and naively built a CI for the MEAN?
lower = mean(rainfall) - sd(rainfall) * qnorm(.975) / sqrt(n)
upper = mean(rainfall) + sd(rainfall) * qnorm(.975) / sqrt(n)
c(lower, upper) #Is this close? Depends on the amount of skew and the sample size.

#5
#rep(value, times) repeats a value a given number of times. Ex: rep(2, 4) will output c(2,2,2,2).
fake_data = c(rep(71.1, 39), 100) #39 values of 71.1, 1 of 100.
n = length(fake_data) #Number of observations
binom_data = as.numeric(fake_data > 71) #Which obs are more than 71?
x = sum(binom_data) #Number of obs more than 71
binom.test(x, n, p = .5, alternative = "greater") #Test that proportion of obs less than 71 is p = .50

#CLT test
Ztest = function(X) {
  #Function returns the z-test value, given a vector of data.
  #This z-test assumes the null hypothesis is zero mean. 
  sqrt(length(X)) * mean(X) / sd(X)
}
Ztest(fake_data)

#Let's change the 100 to a 90. What's the z-score here?
#Does this jive with intuition?
fake_data2 = c(rep(71.3, 39), 90)
Ztest(fake_data2)
