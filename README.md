# Information about the repository

This repository contains the data used to perform the soft-data collection methodology presented in the manuscript *Soft data collection for realistic hydrological modelling: a reproducible methodology developed in R for the Tagus River basin*.

The data necessary to reproduce the work is located in the **used_files** folder. Three folder are can be found within:

 + **Data** Contains both the Streamflow and weather data used for this work. In fact, the streamflow data contains data for all the gauging stations of the Tagus River basin. The weather (pcp and tmp) files have been filtered in order to reduce the weight of the data, but can be downloaded for all Spain in (https://swat.tamu.edu/data/spain/).

 + **Created_csv** Contains the csv that should be created for making the scripts work. To apply this methodology in other regions, these script have to be reproduced.

 + **Shapefiles** includes some shp files that has been used for this work (i.e., delineated subbasins, weather grids, gauging stations, permeability map).


The manuscript can be generated with the **soft_data_paper.qmd** file, as all the needed data is located in the repository. However, a pdf of the manuscript is included **soft_data_paper.pdf**. The code for generating the results included as examples is located in the .qmd document. However, the code for reproducing all the results of the paper can be found in the folder **scripts**, where 5 scripts are located: 
 
 + **script_1_CSV_files_preparation.R** Can be used to create the csv files needed to use this methodology. As inputs, two vector files (delineated basins and gridded data) are needed.

 + **script_2_Runoff_rate calculation.R** Include all the code necessary to calculate runoff rate at annual and average basins, for subbasins and geological regions.
 
 + **script_3_alphas_calculation.R** Allow to calculate the *alpha* values using a linear regression. Three recession curves for each subbasins have been performed, and the R² and standard variation values have also been extracted.
 
 + **script_4_Groundwater_contribution_estimation.R** Include all the code necessary to apply the baseflow filter for three peaks in each subbasins and calculating the groundwater contribution to the streamflow. 
 
 + **script_5_Plots_scripts.R** Allow to generate the figures included in the manuscript. Note that Figure 2 it is not possible to be reproduced with the included precipitation data, as it uses data from all the upper third of the Tagus River basin.  

**Figs** directory contains the images used in the manuscript

**Paper_soft_data_obtention.bib** contains the references in BibTex format.

Other files are related to format adjustments and can be ignored.

The developed methodology can be reproduced but the authors shall not be liable for their use. 

If the methodology is used, authors will appreciate that the developed manuscript is cited.
