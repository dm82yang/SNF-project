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

# create another dataframe to help sorting the data
help_sort <- param_df %>%
  dplyr::select(limit_by,sample)%>%
  unique()
help_sort$sort <- str_c(help_sort$limit_by,":",help_sort$sample)

# creat a varaible for x-axis (limit_by + sample size)
param_df$limitby <- str_c(param_df$limit_by,":",param_df$sample)
param_df$limitby <- factor(param_df$limitby, levels = help_sort$sort)

# vertical line on x-axis
myLoc <- 
  (which(levels(param_df$limitby) == "M:521") +
     which(levels(param_df$limitby) == "RD:581")) / 2
myLoc2 <- 
  (which(levels(param_df$limitby) == "HPM:127") +
     which(levels(param_df$limitby) == "RDHP:137")) / 2



### Just rename few variables
response_ordered <- c('amyloid','tangles','nft','plaque_d','plaque_n',
                      'tdp','cvda','gct','mct','arteriol','caa','lewy_body','ad_reagan',
                      'cogn_global','episodic_memory','work_memory','semantic_memory',
                      'perceptual_speed','perceptual_orient',
                      'global_slope','episodic_slope','work_slope','semantic_slope',
                      'speed_slope','orient_slope','resilience')

output_map <- c("Amyloid-Beta","Paired Helical Filament Tau","Neurofibrillary Tangles",
                "Diffuse Plaque Burden",
                "Neuritic Plaque Burden","TDP-43","Cerebral Atherosclerosis Ratings",
                "Cerebral Amyloid Angiopathy","Arteriolosclerosis","Chronic Microinfarcts",
                "Gross Chronic Cerebral Infarctions","Lewy Body Disease","NIA-Reagan Diagnosis",
                "Global Cognition","Episodic Memory","Semantic Memory",
                "Work Memory","Perceptual Speed","Perceptual Orientation","Global Cognition Slope",
                "Episodic Memory Slope","Work Memory Slope","Semantic Memory Slope",
                "Perceptual Speed Slope","Perceptual Orientation Slope","Resilience")
param_df$output <- plyr::mapvalues(param_df$response, 
                                        from=response_ordered, 
                                        to=output_map)


### visualize the result
param_df %>%
  mutate(number_of_subtype=as.factor(clust_num),
         integrated_modality = snf_type)%>%
  filter(response %in% c('cogn_global','global_slope'))%>%
  # actual plot+++
  ggplot(aes(x = integrated_modality, y = -log10(p), label = limitby,colour = number_of_subtype)) + 
  scale_fill_brewer()+
  geom_point(aes(size = sample, 
                 colour = number_of_subtype)) + 
  geom_text(aes(label = ifelse(-log10(p) > -log10(0.05/16),
                               as.character(limitby),'')),
            vjust = 1, size = 4,show.legend = F) +
  facet_grid(output~.)+
  geom_hline(aes(yintercept = -log10(0.05/16)))+
  annotate("text", x = "HPM", y = 5, label="p-value adjusted\n for 16 tests", color = "black",size=3.2)+
  #geom_vline(aes(xintercept = myLoc),linetype="dotdash",color='red')+
  #geom_vline(aes(xintercept = myLoc2),linetype="dotdash",color='red')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1)) +
  labs(x="Integrated Data Modality
  R:RNAseq  D:DNA methylation  H:Histone Acetylation  P:Proteomics  M:Metabolomics",
       y="Significance of Association (-log10(p-value))",
       title = "Gloabl Cognition and Rate of Cognitive Decline Associations with Full Parameter Space")

