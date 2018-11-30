contrib <-
 function(census_i, census_f, species, trait_val_col="trait_val"){
  require(plyr)
  
#   # Check columns name sanity.
#   col_names = col_names_checker(col_names,[census_f.columns,
#                                            census_i.columns,
#                                            species.columns])  
# colnames(species)[2:3]<- c('trait_val', 'trait_var')
  
  
  
  
  # Group observations by species and merge them. 
# census_i<-ddply(census_i, .(species), function(x){sum(x$n)})
census<-join(census_i, census_f, type='full')


# Filter species list and merge them.

species = species[, c("species",trait_val_col)]
species <- species[species$species %in% unique(census$species),]

# Originality is the diffenrence to the mean trait value.
species$originality = species[,trait_val_col] - mean(species[,trait_val_col])

# Variance originality
species$v_originality = (species[,trait_val_col] ^2) - mean(species[,trait_val_col] ^2)

# Variance cross corrective term.
#histoire du dictionnaire nom col voir fonction s3c.index.mean
# cnames_i = col_names.copy()
# cnames_i["n"] = col_names["n"] + "_i"
# cnames_f = col_names.copy()
# cnames_f["n"] = col_names["n"] + "_f"

census<- join(census, species)

# source("index.R")

S = cwmean(df=census[census$date==1,],trait_val_col) + cwmean(df=census[census$date==3,],trait_val_col)
species$v_cross =  species$originality * S

# Drop unwanted columns
# census.drop([col_names["trait_val"],col_names["trait_var"]],1,inplace=True)


# Compute differences in relative abundances.

rN_i <-daply(census_i, .(species), function(x)sum(x$n))/sum(census_i$n)
rN_f <-daply(census_f, .(species), function(x)sum(x$n))/sum(census_f$n)
species$dp   <-rN_f - rN_i
# Contribution is the product originalitt * dp.
species$contrib   = species$originality * species$dp
species$v_contrib = species$dp * (species$v_originality - species$v_cross)  


return(species)}
