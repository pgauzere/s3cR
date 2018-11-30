bootstrap_cwi <-
function(df, trait_val_col="trait_val", k , bootstrap_ci ,bessel){

  require(plyr)
#      Bootstrap estimator of CWM/CWV.
# Bootstrap is performed /at individual level/. 
# 
# Args:
# df (pandas.DataFrame): Census dataframe.

# k (int): Number of bootstrap re-sampling.
# bootstrap_ci (num): Percentile of bootstrap confidence interval.
# bessel (bool): If true, use bessel correction for an unbiased variance
# estimator (N/(N-1)).
# 
# Returns:
# A dict. with the bootstrap estimators of CWM/CWV and the 
# corresponding confidence interval.

  
  
  
  
# Get relative abundances: this is the distribution from
# which the bootstrap sample will be drawn.
N = sum(df$n) # number of individuals. 
df$rN <- df$n/N 

# Sort by relative abundances to speed up computations.
df<-df[order(df$rN, decreasing = T),]

# Do the cummulative sum of all relative abundances to
# divide the segment [0,1] between all species. 
df$proba =  cumsum(df$rN)
sp = length(df$proba) # number of species.

# Perform k bootstrap.
cwm = array(0,k)
cwv = array(0,k)
for (i in 1:k){
  
  # Draw a new community composition from the distribution.
  df$n<-hist(runif(N),breaks=c(0,df$proba), plot=F)$count
  
  # Compute the indicies. 
  cwm[i] = cwmean(df)
  cwv[i] = cwvar(df, cwm=cwm[i])
  
}


# Bootstrap estimators are derived from the bootstrap distribution. 
out<-data.frame(
bootstrap_cwm = mean(cwm),
bootstrap_cwv = mean(cwv),
bootstrap_cwm_lower_ci = quantile(cwm,1 - (bootstrap_ci/100)),
bootstrap_cwv_lower_ci = quantile(cwv,1 - (bootstrap_ci/100)),
bootstrap_cwm_higher_ci = quantile(cwm,bootstrap_ci/100),
bootstrap_cwv_higher_ci = quantile(cwv,bootstrap_ci/100)
)

return(out)
}
