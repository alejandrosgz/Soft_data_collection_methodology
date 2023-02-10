# Information about the repository

This repository contains the data and code used to perform the soft-data collection methodology presented in the manuscript *Soft data collection for realistic hydrological modelling: a reproducible methodology developed in R for the Tagus River basin*.

### Purpose and utility

Soft data in hydrology can be used to characterize the hydrological behavior of a basin or region, and also to guide hydrological models soft calibration processes. Despite in countries such as Spain there are available weather and hydrological datasets to collect soft data for all the territory, studies which aim to do it are few and limited to small areas and time series. In this work, a soft data collection methodology has been developed to obtain soft data from available weather and streamflow data, focusing into two variables: the runoff coefficient and the baseflow index. This methodology can be reproduced for any gauged catchment in Spain, and can be also used for any other region with similar available datasets. The upper sector of the Tagus River basin has been used as study case, evaluating the two variables mentioned in 19 subbasins of this river located in different geological regions.

### Structure

Within the repository, three directories can be found:

#### 1. used_files directory

The data necessary to reproduce the work is located in the **used_files** folder, which contains three folders within:

 * **Created_csv** Contains the csv files that should be created for making the scripts work. To apply this methodology in other regions, these script have to be reproduced as explained in the manuscript or in corresponding script (**script_1_CSV_files_preparation.R**). A vector layer with the weather data grid and a vector layer with the basin/s that will be assessed are necessary to create these files. A brief description of these files follows:
 
    + **1_basins_file.csv** This file contains basic data about the basin or subbasins that will be assessed. Concretely, the name, ID, and area of each basin, and the ID of the gauging station are the fields of this table (*Basin*, *Basin_ID*, *area*, *gauging_code*, respectively). Another field, (which has been named *region* in this case) can be used to group the assessed basins according to any characteristic.
    
    + **2_ids_stations_file.csv** Contains the ID, name and location of each weather grid point located within the buffer of each basin (*ID*, *NAME*, *LAT*, *LONG*, *ELEVATION*). The basins where they are located (both ID and name are also indicated, as some points are present in more than one contiguous subbasins). 
    
    + **4_groundwater_results.csv** This file has been used to save the data obtained during the baseflow index estimation. Concretely, for each basin, the alpha and BFImax parameters values used for the filter and the estimated baseflow index (columns *alpha*, *BFImax*, *BF_Rate*, respectively) are stored.
 
 * **Data** Contains both the Streamflow and weather data used for this work. In fact, the streamflow data contains data for all the gauging stations of the Tagus River basin. The weather (pcp and tmp) files have been filtered in order to reduce the weight of the data, but can be downloaded for all Spain in (https://swat.tamu.edu/data/spain/).


 * **Shapefiles** includes some shp files that has been used for this work (i.e., delineated subbasins, weather grids, gauging stations, permeability map).


The manuscript can be generated with the **soft_data_paper.qmd** file, as all the needed data is located in the repository. However, a pdf of the manuscript is included **soft_data_paper.pdf**. The code for generating the results included as examples is located in the .qmd document. However, the code for reproducing all the results of the paper can be found in the folder **scripts**, where 5 scripts are located: 
 
 * **script_1_CSV_files_preparation.R** Can be used to create the csv files needed to use this methodology. As inputs, two vector files (delineated basins and gridded data) are needed.

 * **script_2_Runoff_rate calculation.R** Include all the code necessary to calculate runoff rate at annual and average basins, for subbasins and geological regions.
 
 * **script_3_alphas_calculation.R** Allow to calculate the *alpha* values using a linear regression. Three recession curves for each subbasins have been performed, and the R² and standard variation values have also been extracted.
 
 * **script_4_Groundwater_contribution_estimation.R** Include all the code necessary to apply the baseflow filter for three peaks in each subbasins and calculating the groundwater contribution to the streamflow. 
 
 * **script_5_Plots_scripts.R** Allow to generate the figures included in the manuscript. Note that Figure 2 it is not possible to be reproduced with the included precipitation data, as it uses data from all the upper third of the Tagus River basin.  



**Figs** directory contains the images used in the manuscript

**Paper_soft_data_obtention.bib** contains the references in BibTex format.

Other files are related to format adjustments and can be ignored.

## Workflow



The developed methodology can be reproduced but the authors shall not be liable for their use. 

If the methodology is used, authors will appreciate that the developed manuscript is cited.
