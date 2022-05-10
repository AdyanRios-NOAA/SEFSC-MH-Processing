# Management History Processing Project

This SEFSC - MH - Processing repository provides a method for generating an analysis-ready version of the Management History Database. In their original format, the records in this database represent changes in management actions affecting federally managed species throughout the Gulf of Mexico, South Atlantic, and U.S. Caribbean regions. The data files and code contained in this repository define and execut logic for grouping and creating streamlined time series of related management events. This process is actively being updated. Currently, the project is focused on the Gulf of Mexico Reef Fish Resources FMP data. 

### Data folder: All data are stored within the data folder within the repository.
For processing, data are pulled from the raw and interim folders. The raw data folder contains the data downloaded straight from the Management History Database via a CSV file. The interim data folder contains data tables cleaned from the database. These include the following: 
* MH_clean_spp_tables.RData - the saved workspace of the MH_clean_spp_tables.R code 
* MHpreprocess_expansions.csv - zone expansion data to clean zone names
* Clusters folder - location of the unique pre-existing clusters created during data analysis

### Code folder: All scripts used to process the data are stored within the code folder
The code is divided into six subfolders, five  of which contain the analysis and cleaning and one which runs the entire code in the correct order (main_MH_prep.R). To run the code effectively, R and seven packages are required. More details can be found in the READMEcode.md file within the code file. 

### The code is still being edited and updated weekly. 
Bugs have been identified and discussed within the issues tab. Known permanent bugs are included in the [appendix](https://docs.google.com/document/d/1Sby7u3XKtg06HAFJmS8x0fvtaX0yqydQeOYfy-uNeyM/edit#heading=h.49x2ik5) of the meta data documents provided for [data entry](https://docs.google.com/document/d/18k_0_Y9DFTp7fFEo8yivoMowbqdCAddfEcWtdNDM8eA/edit), [processing](https://docs.google.com/document/d/1l1DqJUVhFwBkokm5tZOpsls_coAoSaCG/edit?rtpof=true), and [analysis ready data](https://docs.google.com/document/d/1od-zSiffovacy5wCg9pOdd1xTcywzAUYDyJeaJmCC_o/edit). For training on how to use the database and materials provided, please refer [here](https://docs.google.com/presentation/d/1CktN_RLleF2uOPvBziiSvaoyuzJzQ-dT/edit#slide=id.g12083173c8e_0_0). 

# Disclamer:
This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
