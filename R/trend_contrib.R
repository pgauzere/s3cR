trend_contrib <-
function(census, traits, trait_val_col="trait_val"){
  require(plyr)

  
# Check trait name columns name sanity.
  if(trait_val_col %in% colnames(traits)) {traits$trait_val<-traits[[trait_val_col]]
  }else {print("trait value columns is not defined")}
  
  
# Present species.
census$species<-droplevels(census$species)
present_sp = unique(census$species)


# colnames(species)[2:3]<- c('trait_val', 'trait_var')
species<-traits[traits$species %in% census$species,]
species<-species[,c("species", "trait_val")]
species$species<-droplevels(species$species)

census<-census[census$species %in% species$species,]
census$species<-droplevels(census$species)



# Compute originality.
species$originality = species$trait_val - mean(species$trait_val)
species$v_originality = (species$trait_val^2) - mean(species$trait_val^2)

# Compute relative abundance trends.
census<-ddply(census, .(species, date), summarize, N=sum(n))
census<-ddply(census, .(date),mutate, n_by_date=sum(N))
census$rN<-census$N / census$n_by_date

check_rep_time<-ddply(census,.(species), summarize, n_year=length(unique(date)))
print(paste("species with less than 2 time occurences were removed from analysis : ", 
            as.character(check_rep_time[check_rep_time$n_year<3, "species"])))
census<-census[census$species %in% check_rep_time[check_rep_time$n_year>2, "species"],]
                            
species$dp<-daply(census, .(species), function(x){
    mod<-lm(rN ~ date, data=x)
  return(summary(mod)$coef[2,1])})

# Compute S
census = merge(census, species, by="species")
S = (cwmean(census[census$date==min(census$date),]) 
     + cwmean(census[census$date==max(census$date),]))

# Compute contributions
species$contrib = species$originality * species$dp
species$v_contrib = species$dp  *  (species$v_originality - species$originality * S)
return(species)}
