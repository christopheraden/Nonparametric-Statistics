library(clinfun) #If you don't have this, do install.packages("clinfun") to download it, then run this line.
loc1 = c(26.5, 15, 18.2, 19.5, 23.1, 17.3)
loc2 = c(16.5, 15.8, 14.1, 30.2, 25.1, 17.4)
loc3 = c(19.2, 21.4, 26, 21.6, 35, 28.9)
loc4 = c(26.7, 37.3, 28, 30.1, 33.5, 26.3)
n1 = length(loc1)
n2 = length(loc2)
n3 = length(loc3)
n4 = length(loc4)
x = c(loc1, loc2, loc3, loc4)
grp = rep(1:4, times=c(n1, n2, n3, n4))

#Jonckheere requires the groups be given as NUMERIC or an ordered factor. Give it numbered labels.
#Figure out the alternative based on what the problem asks.
jonckheere.test(x, grp, alternative = c("two.sided", "increasing", "decreasing"), nperm = 1E4)

