# Using-OpenStreetMap-Census-and-Survey-Data-to-Predict-Interethnic-Group-Relations
This repository is set up to showcase the analytical strategy behind our paper"Using OpenStreetMap, Census and Survey Data to Predict Interethnic Group Relations in Belgium: A Machine Learning Approach" published in Social Science Computer Review.

# Background
This repository is set up to showcase the analytical strategy undertaken for our article "Using OpenStreetMap, Census and Survey Data to Predict Interethnic Group Relations in Belgium: A Machine Learning Approach" published in Social Science Computer Review.
Please see and reference the published version in any publication.
Please note that analyses and visualization of the results were undertaken using R and RStudio.

# Notes on Replication

# Accessing Data

All the datasets are available upon request. The Belgian National Election Study 2020 (BNES) data that support the findings of this study are available on request from the authors. The data are not publicly available due to restrictions, associated with sensitive geoinformation that could compromise the (geo)privacy of respondents. Raw census and OpenStreetMap (OSM) data were generated at Statbel, the Belgian statistical office, and Geofabrik, respectively. Pre-processed census and OSM data supporting the findings of this study are available from the corresponding author on request.

# Exploring the Scripts

The pre-processing scripts are made available for the whole of Belgium as to not compromise the (geo)privacy of respondents. 

In total, there are 4 scripts.

1. **Ethnic_Entropy_Index_Open_Access.Rmd.** The .rmd is made available to showcase the calculation of the ethnic entropy index. It relies on the georeferenced Belgian census data and is calculated for each statistical sector.

2. **Neighborhood_SES_Open_Access.Rmd.** The .rmd is made available to showcase the calculation of the socioeconomic status variable. It relies on the georeferenced Belgian census data. The final indicator is PCA-based and produced for each statistical sector.

3. **OSM_Pre_Processing_OpenAccess.R.** The R script is made available to showcase the OSM spatial overlay onto the Belgian statistical sectors and the feauture engineering approach behind the typology of OSM spaces of encounter. The typology is computed for each statistical sector.

4. **Using_OpenStreetMap_Census_Survey_Data_to_Predict_Interethnic_Group_Relations_OpenAccess.Rmd.** The .rmd is made available to demonstrate the workflow behind the main ML analyses.

# Random Seeds

Train and test splits, as well as some of the algorithms used in this analysis, rely on randomness. We have pre-set the same seed everywhere to control the randomness. For data splitting and the ML training process, we used the set.seed() function just prior to initiating the training process to control the randomness and ensure reproducible results. Please note that differences in operating systems, R versions, or changes to the initial code could alter the results. Similarly, reproducibility depends on how package developers handle random number generation. However, we believe this should not be the case here.

Notes on Code Re-Use

You are free to modify or re-use the code for your research purposes. If you build up on or inspire from our codebase substantively, we would kindly ask you to cite our paper and acknowledge our contribution to you research application. 
![image](https://github.com/user-attachments/assets/f9a9a29d-eb00-42bb-b636-ae37e61feaa6)


