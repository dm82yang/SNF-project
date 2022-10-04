# SNF-project
### This project is aiming to identify informative molecular subtype from 5 post-mortem multi-omic data modalities obtained from the ROS/MAP dataset (https://www.synapse.org/#!Synapse:syn3219045). Similarity network fusion (SNF, https://www.nature.com/articles/nmeth.2810) followed by spectral clustering was used to identify clusters, while multiple methods were used to validate subtyping solutions (modified based on 'clValid' package in R). Visualizations for this project including upset plot, heatmap, scree plot, alluvial plot, scatter plot, bar plot and box plot; all visualizations were created in R.

## Data processing
### The Data processing folder contains the code for processing data before the main analysis
#### "metabolon manipulation.R" and "tmt_manipulation.R" helped to process metabolomic and proetomic data correspondingly, with the RF data imputation method
#### "pca_processing.Rmd" file contains the data pre-processing process using PCA to detect and regress out confounders

## Main Analysis
### The Main Analysis folder contains the code for the analysis, including SNF on the whole parameter space, estimating cluster stability and translating features from Histonce Acetylation data
#### "param_loop_LRT.Rmd" is the main analysis in this project where SNF and spectral clustering were used over the whole parameter space:
#####  a) response variables
#####  b) datatypes limiting sample size
#####  c) datatypes used in SNF (subset of b)
#####  d) cluster number
#### "cluster stability.Rmd" contains codes evaluating cluster stability based on 'clValid', it also tested multiple evaluation methods (clValid(), modification of clValid() and Nbclust)
#### "peaks translation" helps to translate H3K9ac chip-seq features from peak number to gene areas (i.e., chr5:3326444-3326480)

## Visualization
### The Visualization folder contains the code to create visualizations for the preprint:()
#### "Detailed regression and visualization.Rmd" produce a dataframe of regression output for individual cluster memberships and corresponding bar-plots
#### "Test against unimodal subtype.R" visualize overall associations between neuropathologies and clustering output
#### "alluv.rmd" contains a frame work for creating alluvial plots for subtype overlap
#### "param_space_visualization.R" help to visualize and investigate the whole parameter space

