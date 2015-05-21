HSD.Perm = function(x, grps, nsim=1E4)
{
  #Takes in dataset as a long combined vector of data, 
  #vector of group labels, and optional number of simulations to run.
  #Outputs a histogram of the permutation distribution of the pairwise differences,
  #drawing the HSDs on the histograms and labeling the graph.
  #Also returns a list containing all HSDs and all Q permutation samples, used to calculate p-values.
  
  #If the group is not a factor, make it one.
  if(!is.factor(grps)) grps = as.factor(grps)
  samp.sizes = as.numeric(by(x, grps, length)) #Calculate the sample sizes, n1 n2 n3 ...
  n = sum(samp.sizes) #Calculate total sample size.
  MSE <- summary(aov(x ~ grps))[[1]][2,3] #My god, that's a lot of work to get the MSE.
  
  get.Tij = function(x, grps, MSE, calc.Q=FALSE){
    #Calculates the pairwise differences for a given permutation of the data.
    #Either returns all HSDs (calc.Q=FALSE), or just returns the biggest one,
    #used to calculate the permutation distribution.
    samp.sizes = as.numeric(by(x, grps, length))
    samp.means = as.numeric(by(x, grps, mean))
    k = length(samp.sizes)
    #Make a grid of i and j, keeping only i > j.
    IJgrid = expand.grid(i = 2:k, j = 1:k)
    IJgrid = IJgrid[IJgrid$i > IJgrid$j, ]
    Tij = sapply(1:nrow(IJgrid), function(r){ #calculates Tij according to the formula on p98.
      abs(samp.means[IJgrid[r,"i"]] - samp.means[IJgrid[r,"j"]])/sqrt(MSE * (1/samp.sizes[IJgrid[r,"i"]] + 1/samp.sizes[IJgrid[r,"j"]]))
    })
    if(calc.Q) { return(max(Tij)) } else{ return(cbind(IJgrid, Tij)) }
  }
  
  #Generate the Q distribution by shuffling the data nsim times, saving only the max(Tij)
  Q.dist = sapply(1:nsim, function(sim) get.Tij(sample(x, n), grps, MSE, calc.Q=TRUE))
  
  #Make the histogram, with all differences labeled on it, and a legend created.
  Tij.obs = get.Tij(x, grps, MSE)
  plotmin = min(c(min(Q.dist), Tij.obs$Tij))
  plotmax = max(c(max(Q.dist), Tij.obs$Tij))
  hist(Q.dist, xlab="Q*", main="Histogram of Permuted Maximum Pairwise Differences", xlim=c(plotmin, plotmax))
  for(r in 1:nrow(Tij.obs)) abline(v = Tij.obs[r, "Tij"], col=r)
  legend("topright", col=1:nrow(Tij.obs), pch=15, legend = paste(Tij.obs$i, "-", Tij.obs$j, sep=' '))
  
  #Save the observed Tij and the set of all permuted Q's.
  list(HSD = Tij.obs, Qdist = Q.dist)
}


##EXAMPLE DATA, taken from Page 95
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
HSDs = HSD.Perm(x, grp)