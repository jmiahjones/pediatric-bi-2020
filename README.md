# pediatric-bi-2020

**Disclaimer:** The results presented below have not been externally validated. Do not use the information here for medical decisions.

This project contains all the code and data necessary for reproducing the results of [this paper](https://doi.org/10.1016/j.jpeds.2020.12.079). The purpose of this project is to provide a risk calculator for Bacterial Infections among febrile infants. An example of this risk calculator may be found at https://www.urmc.rochester.edu/sites/biriskcalculator/.

# Data Codebook

Information on this data can be found in the [derivation paper](https://doi.org/10.1016/j.jpeds.2020.12.079). The two main data files are `main_analysis_data.csv` and `sensitivity_analysis_data.csv`. The codebook for the variables in these files is given below.

Variable Name | Coding | Notes
--------------|--------|------------
Record ID| Int | Unique identifier
Sex|0 = Male, 1 = Female| 
Insurance Type|0 = Private, 1 = Public,2 = None|
Presence of Chronic Medical Condition (CMC)|0 = No,1 = Yes|
Age|Continuous | Days
Gestational Age|Continuous | Weeks; 37.5 = "Full Term"
Ill Appearing|0 = No, 1 = Yes, 2 = Unknown|
Maximum Temperature| Continuous | Celsius
Number of Days of Illness| Continuous |
Cough Present During Illness?|0 = Yes, 1 = No, 2 = Unknown|
Positive Urinary Tract Inflammation|0 = No, 1 = Yes, 2 = Unknown|
RLR|0 = Low, 1 = High, 2 = Unknown|Rochester Low Risk Criteria; Competing risk stratification technique
Bacterial Infection|0 = No, 1 = Yes|BI = UTI, Meningitis or bacteremia

The first column is a unique identifier for each infant. The final column represents the outcome being studied. The RLR column represents the risk stratification technique we compare to our classifiers. The remaining variables are predictors.

# Project Workflow

`CV_SL_GLM_main.R` produces the results for the main analysis. Variable importance was then examined by `CV_SL_GLM_vimp.R`. The sensitivity analysis, removing medical history variables, was performed in `CV_SL_GLM_nohist.R`. The model for the web calculator is produced in `web_glm_fit.R`.

Intermediate files and output are available in the `results/` directory.

# Session Info

```r
> sessionInfo()
R version 3.5.0 (2018-04-23)
Platform: x86_64-redhat-linux-gnu (64-bit)
Running under: CentOS release 6.10 (Final)

Matrix products: default
BLAS: /usr/lib64/R/lib/libRblas.so
LAPACK: /usr/lib64/R/lib/libRlapack.so

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
 [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
 [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
 [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
 [9] LC_ADDRESS=C               LC_TELEPHONE=C            
[11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] parallel  stats     graphics  grDevices utils     datasets  methods  
[8] base     

other attached packages:
 [1] vimp_2.1.0          pROC_1.15.3         doParallel_1.0.15  
 [4] iterators_1.0.12    readxl_1.3.1        SuperLearner_2.0-24
 [7] nnls_1.4            foreach_1.5.0       caret_6.0-81       
[10] ggplot2_3.3.0       lattice_0.20-41     dplyr_1.0.2        

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.4.6       cellranger_1.1.0   pillar_1.4.3       compiler_3.5.0    
 [5] gower_0.1.2        plyr_1.8.6         class_7.3-17       tools_3.5.0       
 [9] rpart_4.1-15       ipred_0.9-8        lubridate_1.7.9    lifecycle_0.2.0   
[13] tibble_3.0.1       nlme_3.1-149       gtable_0.3.0       pkgconfig_2.0.3   
[17] rlang_0.4.7        Matrix_1.2-18      prodlim_2018.04.18 withr_2.2.0       
[21] stringr_1.4.0      generics_0.0.2     vctrs_0.3.4        recipes_0.1.4     
[25] stats4_3.5.0       nnet_7.3-14        grid_3.5.0         tidyselect_1.1.0  
[29] glue_1.4.0         data.table_1.13.0  R6_2.4.1           survival_3.2-3    
[33] lava_1.6.4         purrr_0.3.4        reshape2_1.4.4     magrittr_1.5      
[37] splines_3.5.0      MASS_7.3-52        scales_1.0.0       codetools_0.2-16  
[41] ModelMetrics_1.2.2 ellipsis_0.3.0     timeDate_3043.102  colorspace_1.4-1  
[45] stringi_1.4.3      munsell_0.5.0      crayon_1.3.4 
```
