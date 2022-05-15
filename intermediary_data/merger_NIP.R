##Merger Note in the Pocket

library(dplyr)

#Change as need be
dir<-"~/STATCOM/NoteInThePocket/"

orddf<-readRDS(paste0(dir,"orders_allyears_df.rds"))

zip_data<-read.csv(paste0(dir,"/zcta_tract_rel_10.txt"))

NC_18<-read.csv(paste0(dir,"NorthCarolina.csv"))

zpoppct_summary<- zip_data %>% group_by(ZCTA5) %>% summarise(zpoppct_sum = sum(ZPOPPCT))

##Yes most census tracts are wholly accounted for by the zip codes
mean(zpoppct_summary$zpoppct_sum>=99)

NC18_ColSubset<-names(NC_18[,8:ncol(NC_18)])

merged_Ord_mat<-matrix(NA, nrow = nrow(orddf), ncol=length(NC18_ColSubset))

no_f_dat<-0
merged_mat_idx<-1

NC_18[NC_18==-999]<-NA

for (zip in orddf$Zip){
  if (!is.na(zip)){
    ##Grabs the geoids/tracts and percent of pop. covered by that geoid, for this zipcode
    one_zip_mult_tract <- zip_data[zip_data$ZCTA5 == zip, names(zip_data) %in% c("GEOID", "ZPOPPCT")]
    one_zip_mult_tract<-rename(one_zip_mult_tract, FIPS="GEOID")
  }
  else{
    one_zip_mult_tract<-data.frame(FIPS=NaN,ZPOPPCT=NaN)
  }
  
  
  ##Gets NC_18 for These Tracts
  one_zip_mult_tract_NP <- merge(one_zip_mult_tract, NC_18, by = "FIPS")
  
  col_idx<-1
  for (coln in NC18_ColSubset){
    if (nrow(one_zip_mult_tract_NP) == 0) {
      merged_Ord_mat[merged_mat_idx, col_idx] <- NaN
      
      no_f_dat <- no_f_dat + 1
      
    }
    ##There are tracts and percent pop. covered by the zip code for this tract
    else {
      ##Take weighted sum
      merged_Ord_mat[merged_mat_idx, col_idx] <- sum(one_zip_mult_tract_NP[coln] * 
                                                      one_zip_mult_tract_NP$ZPOPPCT, na.rm = T) / 
        sum(one_zip_mult_tract_NP$ZPOPPCT[!is.na(one_zip_mult_tract_NP[coln])])
    }
    col_idx <- col_idx + 1
  }
  merged_mat_idx <- merged_mat_idx+1
}

colnames(merged_Ord_mat) <-NC18_ColSubset

##Merged dataset
new_Ord_data<-data.frame(orddf,merged_Ord_mat)

write.csv(x=new_Ord_data, file=paste0(dir,"merged_CDC_NP.csv"))