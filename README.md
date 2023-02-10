# Information about the repository

This repository contains the data and code used to perform the soft-data collection methodology presented in the manuscript *Soft data collection for realistic hydrological modelling: a reproducible methodology developed in R for the Tagus River basin*.

### Purpose and utility

Soft data in hydrology can be used to characterize the hydrological behavior of a basin or region, and also to guide hydrological models soft calibration processes. Despite in countries such as Spain there are available weather and hydrological datasets to collect soft data for all the territory, studies which aim to do it are few and limited to small areas and time series. In this work, a soft data collection methodology has been developed to obtain soft data from available weather and streamflow data, focusing into two variables: the runoff coefficient and the baseflow index. This methodology can be reproduced for any gauged catchment in Spain, and can be also used for any other region with similar available datasets. The upper sector of the Tagus River basin has been used as study case, evaluating the two variables mentioned in 19 subbasins of this river located in different geological regions.

### Structure

Within the repository, three directories can be found:

#### 1. used_files directory

The data necessary to reproduce the work is located in the **used_files** folder, which contains three folders within and a zip file:

 * **Created_csv** Contains the csv files that should be created for making the scripts work. To apply this methodology in other regions, these script have to be reproduced as explained in the manuscript or in the corresponding script (Script 1, **script_1_CSV_files_preparation.R**). A vector layer with the weather data grid and a vector layer with the basin/s that will be assessed are necessary to create these files. A brief description of these files follows:
 
    + **1_basins_file.csv** This file contains basic data about the basin or subbasins that will be assessed. Concretely, the name, ID, and area of each basin, and the ID of the gauging station are the fields of this table (*Basin*, *Basin_ID*, *area*, *gauging_code*, respectively). Another field, (which has been named *region* in this case) can be used to group the assessed basins according to any characteristic. Created with Script 1.
    
    + **2_ids_stations_file.csv** Contains the ID, name and location of each weather grid point located within the buffer of each basin (*ID*, *NAME*, *LAT*, *LONG*, *ELEVATION*). The basins where they are located (both ID and name are also indicated, as some points are present in more than one contiguous subbasins). Note that in this case, precipitation and temperature points IDs were the same, but if not one file for each grid should be created and used. Created with Script 1.
    
    + **3_alpha_estimation.csv** This file contains for each basin the alpha obtained with the regression for the three peaks (*alphas*) and their determination coefficients *det_coefs*. The name, ID and region of each basin is also included. Created with the results of the Script 3 (**script_3_alphas_calculation.R**).
    
    + **4_groundwater_results.csv** This file has been used to save the data obtained during the baseflow index estimation. Concretely, for each basin, the alpha and BFImax parameters values used for the filter and the estimated baseflow index (columns *alpha*, *BFImax*, *BF_Rate*, respectively) are stored. Created with the results of the Script 4 (**script_4_Groundwater_contribution_estimation.R**).
 
 * **Data** This directory contains two folders:
 
    + **climate_data_extracted** Contains two folders (one for each variable, *pcp_spain* and *tmp_spain*, respectively) which contain the files with the data for each point of the grid used in this work. Files for all the Spanish territory can be downloaded from (https://swat.tamu.edu/data/spain/). These files have one column with the daily value of the variable, being the name of the column the initial date of the time series (19510101).
    
    + **Gauging_data** Contains the file **afliq.csv**, downloaded from (https://ceh.cedex.es/anuarioaforos/TAJO_csv.asp). This file contains daily streamflow data for all the gauging stations located within the Tagus River basin, and have four columns: the gauging station code (*indroea*), the date (*fecha*), the height of the water (*altura*, which has not been used), and the streamflow (*caudal*, in cubic meters per second).
    
 * **GIS** This directory contains all the vector and raster data used for this work. It have two folders:
 
    + **Shapefiles** Directory that includes the shp (and related) files that has been used for this work: the upper sector of the Tagus River basin (**modeled_basin**), the delineated subbasins (**basins_studied**), weather grids (**grid_tagus**), gauging stations (**gauging_stations**) and the permeability map(**permeabilit_map**).
 
    + **Delineation.zip** Compressed folder which contains three raster layers: the digital elevation model (**ClipMDTproj.tif**) used to create the drainage direction (**drainage_direction_Tagus.tif**) and accumulation rasters (**drainage_acumulation_Tagus.tif**), which were used to delineate the subbasins. This file was compressed to reduce its weight.


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
