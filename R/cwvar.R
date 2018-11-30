cwvar <-
function(df, trait_val_col="trait_val", bessel=TRUE, cwm=NA){
  require(plyr)
#   Compute the community weighted mean and community weighted variance of the dataframe.
# 
#     Args:
#         df (dataframe)       : Containing a column "n" (number of individuals) and a column referencing the trait value.
#         trai_val_col (text)  : names of the column referencing trait value 
#         bessel (bool)        : If TRUE, use bessel correction for an unbiased variance estimator (N/(N-1)).
#         cwm                  : If Community weighted means has already been computed (it is only to speed up computations)
#     Return: 
 #         Community weighted mean and community weighted variance.
  
  df$trait_val<-df[[trait_val_col]]
  
  if (is.na(cwm) == T) {cwm <- cwmean(df)}
  
  if (bessel==TRUE){
    corrective_term = sum(df$n)  / (sum(df$n) -1)
  } else {
    corrective_term = 1
  }
      
n2  <- sum ( df$trait_val^2 * df$n / sum(df$n))
return(cwv=corrective_term * (n2 - cwm^2))}
