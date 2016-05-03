# Discretize: 3 Bins Equal of near equal amounts

# possible algorithm for creating bins of equal or near equal amounts
# The idea is to determine the bin's grid coords such that the standard deviation
# of the number of elements within the bins (computed for a given set of 
# grid coords) is minimized
# for a vector y, 
# 1. y<-sort(y)
# 2. Possible space of grid points is:grid.possible<- y[unique(rank(y))]
# 3. From grid.possible grid point set, select nbin+1 grids points for ith iteration  
# a. allocate x into these bins
# d. compute sd of the number of elements within the bins
# d. accept grid coord sol if sd for ith iteration < sd for i-1th iteration
# 4. Accept grid coords where the sd is minimized.
sd.BinCount<-function(y,gridpts)
{
  # note that gridpt[1]=-Inf and gridpt[nbin+1]=y[length(y)], where y is a ascending order sorted vector!
  nbins=length(gridpts)-1
  ydisc=rep(NA,length(y))
  for (i in 1:nbins){ydisc[(gridpts[i] < y) & (y <= gridpts[i+1])]<-i}
  N.perbin=rep(NA,nbins)
  for (i in 1:nbins){N.perbin[i]<-sum(ydisc == i)}
  result<-sd(N.perbin)
}

EqualCount<-function(y,nbins)
{
  Grid.possible<-y[unique(rank(y))]
  gridpts<-rep(NA,nbins+1)
  Finalpts<-rep(NA,nbins+1)
  gridpts[1]<- -Inf
  gridpts[nbins+1]<-y[length(y)] # i.e., last element
  
  GridSet.possible<-combn(Grid.possible,nbins-1)# set of combinations of size nCr where n = length(Grid.possible), and r = nbins-1
  ncombos<-dim(GridSet.possible)[2]
  sd.minima<-Inf
  for (i in 1:ncombos)
  {
    for (j in 1:(nbins-1)){gridpts[j+1]<-GridSet.possible[j,i]}
    sd.candidate<-sd.BinCount(y,gridpts)
    if (sd.candidate < sd.minima)
    {
      sd.minima<-sd.candidate
      Finalpts<-gridpts
    }  
  }
  result<-Finalpts
}


y<-c(3,4,4,5,5,5,5,5,5,5,5,6,6,6,6,6,7,7,7,7,8,8,9,12,23,23,25,81)
y<-sort(y)
nposs<-length(y[unique(rank(y))])
"no of possible bins"
nposs
# nbins <= nposs!!! <- because members of equal rank are ALWAYS assigned to same bin!
nbins<-3
Final.gridpts<-EqualCount(y,nbins)
ydisc<-rep(NA,length(y))
for (i in 1:nbins){ydisc[(Final.gridpts[i] < y) & (y <= Final.gridpts[i+1])]<-i}
y
ydisc