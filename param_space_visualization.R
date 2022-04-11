library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)
library(ggpubr)
library(ggfortify)
library(SNFtool)
library(data.table)
library(RColorBrewer)
library(vcd)
library(UpSetR)
library(MASS)
library(alluvial)
library(ggalluvial)
library(stringr)

param_df <- read.csv("C:/Users/yangmu/Desktop/rosmap/SNF sample analysis/param_df_0411.csv",row.names = 1, header = TRUE)

# Reproduce the parameter space
d_types <- c('R','D','H','P','M')
param_space <- c()
for (i in 1:length(d_types)){
  m <-combn(d_types,i)
  num <- dim(m)[2]
  for (j in 1:num){
    param <- paste(m[,j],collapse='')
    param_space<- append(param_space,param)
  }
}

# order limit_by and snf_type in the order of parameter space
param_df$limit_by <- factor(param_df$limit_by, levels = param_space)
param_df$snf_type <- factor(param_df$snf_type, levels = param_space)

# create another dataframe to help sorting the data
help_sort <- param_df %>%
  dplyr::select(limit_by,sample)%>%
  unique()
help_sort$sort <- str_c(help_sort$limit_by,":",help_sort$sample)

# creat a varaible for x-axis (limit_by + sample size)
param_df$limitby <- str_c(param_df$limit_by,":",param_df$sample)
param_df$limitby <- factor(param_df$x_axis, levels = help_sort$sort)

# vertical line on x-axis
myLoc <- 
  (which(levels(param_df$x_axis) == "M:521") +
     which(levels(param_df$x_axis) == "RD:581")) / 2

# visualize the result
param_df %>%
  # filter for significant result
  filter(-log10(p)>-log10(0.05))%>%
  # filter for optimal cluster number 
  #filter(clust_num==opt_est)%>%
  
  # select most significant point for each condition
  group_by(limit_by,snf_type)%>%
  arrange(log10(p))%>%
  slice(1)%>%
  
  # actual plot
  ggplot(aes(x = limitby, y = -log10(p), label = snf_type, 
             color = as.factor(clust_num), size = sample)) + 
  geom_jitter(aes(size = sample)) + 
  geom_text(vjust = 1, size = 4) +
  facet_grid(response~.)+
  geom_hline(aes(yintercept = -log10(0.05)))+
  geom_vline(aes(xintercept = myLoc),linetype="dotdash",color='red')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1))
