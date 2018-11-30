# s3cR

Type: R Package
Title: Specific Contributions to Community Changes in R
Version: 1.0
Date: 2015-01-29
Author: Gaüzère Pierre & Doulcier Guilhem
Maintainer: Who to complain to <pierre.gauzere@gmail.com>
Description: s3cR is a small R package written to compute community weighted indices and specific contributions in their variations.
License: This program is distributed under the term of the GNU General Public License v3 (or later) with ABSOLUTELY NO WARRANTY. This is free software, and you are welcome to redistribute it.

Quick Example : 

data(census)
data(species)

out<-cwi(census, species, trait_val_col = "size")

print(out)

out<-cwi(census, species, trait_val_col = "size", 
         bootstrap=TRUE, 
         bootstrap_n=100, 
         bootstrap_ci=95)
}
