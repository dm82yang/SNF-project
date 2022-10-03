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

metabo_path<-'C:/Users/yangmu/Desktop/rosmap/metabolomics'
data_metabo<-read.csv(file.path(metabo_path,'ROSMAP_Metabolon.csv'),row.names = 1, header = TRUE)

data_metabo <- data_metabo[-c(1,2,3,4,5,6,7)]
data_metabo$R_id <- rownames(data_metabo)

#Imputation
df_missing <- function(df,patient_r=0.6,feature_r=0.8, knn =10){
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
  
}

# Try Random Forest Imputation
data_metabo_target <- df_missing(data_metabo,patient_r=1)
data_metabo <- df_missing(data_metabo,patient_r=0.6)
data_metabo<-lapply(data_metabo,as.numeric)
data_metabo[is.na(data_metabo)]<-"NA"
test_data<-missForest(as.data.frame(data_metabo),replace = TRUE,verbose = TRUE)

metabo_impute <- test_data$ximp

rownames(metabo_impute)<-data_metabo$R_id


data_metabo_t <-transpose(metabo_impute)
data_metabo_t_col <- row.names(metabo_impute)
colnames(data_metabo_t)<-data_metabo_t_col
rownames(data_metabo_t)<-colnames(metabo_impute)
write.csv(data_metabo_t,"C:/Users/yangmu/Desktop/rosmap/metabolon_ready.csv", row.names = TRUE)
