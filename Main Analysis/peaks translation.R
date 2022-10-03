library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)
library(ggpubr)
library(ggfortify)
library(SNFtool)
library(data.table)
library(RColorBrewer)
library(plyr)
library(vcd)
library(UpSetR)
library(MASS)
library(alluvial)
library(ggalluvial)
library(readxl)
rosmap_path<-'C:/Users/yangmu/Desktop/rosmap/'
NMI_score<-readRDS("C:/Users/yangmu/Desktop/rosmap/SNF sample analysis/NMI_score.RData")
peaks <- read_excel(file.path(rosmap_path,'peak_info.xls'))

top10_rna <- c()
top10_dna <- c()
top10_histone <- c()

for (i in 1:10){
  top10_rna <- append(top10_rna,
                      colnames(Data_rnaseq)[which(NMI_score$rank[[1]] %in% c(i))])
  top10_dna <- append(top10_dna,
                      colnames(Data_methylation)[which(NMI_score$rank[[2]] %in% c(i))])
  top10_histone <- append(top10_histone,
                          colnames(Data_chipseq)[which(NMI_score$rank[[3]] %in% c(i))])
}


feature_list <- c(top10_rna,top10_dna,top10_histone)

top10_histone <- c('peak17850','peak22450','peak21453','peak2459','peak14588',
                   'peak17248','peak6020','peak1163','peak7030','peak24717')

translate <- c()
for (i in top10_histone) {
  slice <- peaks[peaks$`Domain ID` == i, ]
  str <- paste('chr',slice$Chr,': ',slice$Start,'-',slice$End,sep='',collapse = NULL)
  translate<-append(translate,str)
}
translate
