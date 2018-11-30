cwmean <-
function(df, trait_val_col="trait_val"){
  
#   Compute the community weighted mean of the dataframe.
# 
#     Args:
#         df (dataframe)       : Containing a column "n" (number of individuals) and a column referencing the trait value.
#         trai_val_col (text)  : names of the column referencing trait value 
# 
#     Return: 
#         Community weighted mean.

  
  return(  sum( df[,trait_val_col] *  df$n / sum(df$n))) }
