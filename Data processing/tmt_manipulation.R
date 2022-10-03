library(dplyr)
library(tidyr)
library(zoo)
library(tableone)
library(survival)
library(ggplot2)
library(ggpubr)
library(ggfortify)
library(SNFtool)
library(data.table)
library(RColorBrewer)
library(plyr)
library(vcd)
library(missForest)
library(installr)
library(VIM)

prot_path<-'C:/Users/yangmu/Desktop/rosmap/proteomic'
tmt_2015<-read.csv(file.path(prot_path,'TMT/tmt_2015.csv'),row.names = 1, header = TRUE)
check_colname <- colnames(tmt_2015)
sum(nchar(check_colname)!=8)

# extract paired ID
meta <- read.csv('C:/Users/yangmu/Desktop/rosmap/metadata_for_clean/ROSMAP_biospecimen_metadata.csv',
                 header = TRUE)
meta_tmt <- meta %>%
  filter(assay == 'TMT quantitation')%>%
  select(individualID,specimenID)%>%
  filter(grepl('^R',individualID)) %>%
  mutate(specimenID = substr(specimenID,14,21))


tmt_2015_t<- transpose(tmt_2015)
tmt_2015_t$specimenID<- colnames(tmt_2015)
colnames(tmt_2015_t) <- row.names(tmt_2015)

tmt_2015_t<-join(tmt_2015_t,meta_tmt,by='specimenID',type="inner")
tmt_2015_t<- tmt_2015_t%>%
  select(-c('specimenID'))

df_missing <- function(df,patient_r=0.8,feature_r=0.8, knn =10){
  #filter by feature
  df['row_na_ratio']<-rowSums(is.na(df))/ncol(df)
  df <- df %>% filter(row_na_ratio < 1-patient_r)
  df <- df[1:(length(df)-1)]
  
  #filter by sample
  col_check<-colSums(is.na(df))/nrow(df)
  col_check <- col_check > (1-feature_r)
  na_index <- which(col_check)
  if (length(na_index)>0) {
    df <- df[-na_index]
  } else{
    df <- df
  }
  
  #Imputation
}

tmt_2015_imp <- df_missing(tmt_2015_t)
tmt_2015_imp<-lapply(tmt_2015_imp,as.numeric)
tmt_2015_imp[is.na(tmt_2015_imp)]<-"NA"
tmt_imp<-missForest(as.data.frame(tmt_2015_imp),replace = TRUE,verbose = TRUE)

tmt_imp_mx <- tmt_imp$ximp
rownames(tmt_imp_mx)<-tmt_2015_imp$individualID
tmt_out <- transpose(tmt_imp_mx)
rownames(tmt_out) <- colnames(tmt_imp_mx)
colnames(tmt_out) <- rownames(tmt_imp_mx)

write.csv(tmt_out,"C:/Users/yangmu/Desktop/rosmap/tmt_ready.csv", row.names = TRUE)
