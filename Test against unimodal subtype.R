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

param_df <- read.csv("C:/Users/yangmu/Desktop/rosmap/SNF sample analysis/param_space_after_pca_v2.csv",
                     row.names = 1, header = TRUE)

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

# Order responses
response_ordered <- c('apoe4','amyloid','tangles','nft','plaque_d','plaque_n',
                      'tdp','cvda','caa','arteriol','mct','gct',
                      'lewy_body','ad_reagan','cogdx',
                      'cogn_global','episodic_memory','semantic_memory',
                      'work_memory','perceptual_speed','perceptual_orient',
                      'global_slope','episodic_slope','work_slope','semantic_slope',
                      'speed_slope','orient_slope','resilience')
param_df$response <- factor(param_df$response, levels = response_ordered)
full_names <- c("APOE-E4","Amyloid-Beta","Paired Helical Filament Tau","Neurofibrillary Tangles",
                "Diffuse Plaque Burden",
                "Neuritic Plaque Burden","TDP-43","Cerebral Atherosclerosis Ratings",
                "Cerebral Amyloid Angiopathy","Arteriolosclerosis","Chronic Microinfarcts",
                "Gross Chronic Cerebral Infarctions","Lewy Body Disease","NIA-Reagan Diagnosis",
                "Cognitive Diagnostic","Global Cognition","Episodic Memory","Semantic Memory",
                "Work Memory","Perceptual Speed","Perceptual Orientation","Global Cognition Slope",
                "Episodic Memory Slope","Work Memory Slope","Semantic Memory Slope",
                "Perceptual Speed Slope","Perceptual Orientation Slope","Resilience")
param_df$response<-plyr::mapvalues(param_df$response,from = response_ordered, to = full_names)
param_df$snf_type <- plyr::mapvalues(param_df$snf_type, from = c('R','H','D','RDH'), 
                                     to = c('RNAseq','Histone Acetylation','DNA Methylation',
                                            '3-Modal Integration'))


# visualize the result
param_df %>%
  filter(!(response %in% c('Cognitive Diagnostic','Episodic Memory',
                           'Semantic Memory','Work Memory','Perceptual Speed',
                           'Perceptual Orientation','Episodic Memory Slope',
                           'Work Memory Slope','Semantic Memory Slope',
                           'Perceptual Speed Slope','Perceptual Orientation Slope'))) %>%
  filter(limit_by =="RDH") %>%
  filter((snf_type == "3-Modal Integration") & (clust_num == 3)) %>%
  #filter(((snf_type == "3-Modal Integration") & (clust_num == 3))|
  #         ((snf_type == "RNAseq") & (clust_num == opt_est))|
  #         ((snf_type == "Histone Acetylation") & (clust_num == opt_est))|
  #      ((snf_type == 'DNA Methylation') & (clust_num == opt_est))) %>%
  #mutate(p = p*19) %>%
  mutate(clust_num=as.factor(clust_num))%>%
  dplyr::rename(data_modality=snf_type) %>%
  ggplot(aes(x = response, y = -log10(p), label = data_modality, 
           color = data_modality)) +
  geom_point(size = 4, color = 'purple') +
  geom_hline(aes(yintercept = -log10(0.05)))+
  annotate("text", x = "Arteriolosclerosis", y = -log10(0.035), label="P value unadjusted", color = "black") +
  geom_hline(aes(yintercept = -log10(0.05/16)), col = 'purple')+
  annotate("text", x = "Arteriolosclerosis", y = -log10(0.0018), label="P value adjusted for 16 tests", color = "purple") +
  theme_bw()+
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10))+
  scale_fill_brewer(palette="PuBuGn")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5)) +
  #theme(legend.position="none") +
  labs(x="Neuropathologies & Cognition Measurements",y="Significance of Association (-log10(p-value))",
       title = "Neuropathology and Cognition Associations with Three-Modality Subtypes (Three Molecular Subtypes)")


#param<-param_df %>%
#  filter(!(response %in% c('Cognitive Diagnostic','Episodic Memory',
#                           'Semantic Memory','Work Memory','Perceptual Speed',
#                           'Perceptual Orientation','Diffuse Plaque Burden'))) %>%
#  filter(limit_by =="R") %>%
#  filter(((snf_type == "RNAseq") & (clust_num == opt_est)))
  
