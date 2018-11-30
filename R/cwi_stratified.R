cwi_stratified <-
function(census, traits, trait_val_col="trait_val",
                   bootstrap = FALSE, bootstrap_n = 100, bootstrap_ci=95,
                   bessel = TRUE){
  require(plyr)
  ### Stratified computation of community weighted indexes of a community.

# Observations will be grouped by site and date.
# Bootstrap is performed /at individual level/. 
# 
# Args:
# census (data.frame)  : Census dataframe. Required columns are "n" (number of individual, numeric), "species", "site", "date".
# traits (data.frame)  : trait value dataframe. Required columns are "species" and the trait value colums defined in trait_val_col argument.
# trait_val_col (text) : name of column in "traits" for which cwi will be applied.       
# bootstrap (bool)     : Perform bootstrap if true.
# bootstrap_n (int)    : Number of bootstrap re-sampling.
# bootstrap_ci (num)   : Percentile of bootstrap confidence interval.
# bessel (logical)     : If TRUE, use bessel correction for an unbiased variance estimator (N/(N-1)).
# 
# Returns:
# A data.frame with CWM/CWV estimation and bootstraps estimators and the corresponding confidence interval if required.


# check present species.
census$species <- droplevels(census$species)
present_sp     <- unique(census$species)
traits<-traits[traits$species %in% census$species,]


# Merge census with species traits. 
merged = merge(census, traits, by="species")

# Check trait name columns name sanity.
if(trait_val_col %in% colnames(traits)) {merged$trait_val<-merged[[trait_val_col]]
}else {print("trait value columns is not defined")}

# Compute indexes and sum of observation 
out<-ddply(merged, .(site, date), function(x){
  return(cbind(cwm=cwmean(x), cwv=cwvar(x, cwm=cwm), n=sum(x$n)))})
               
# Perform bootstrap if needed.
if (bootstrap==TRUE){
  out_boot = bootstrap_cwi(merged, k=bootstrap_n, bootstrap_ci=bootstrap_ci, bessel=T)
  out<-cbind(out, out_boot)
}

return (out)}
