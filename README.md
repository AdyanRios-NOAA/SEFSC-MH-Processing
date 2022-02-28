# Management History Processing Project

The SEFSC - MH - Processing repository contains the data analysis and cleaning of the raw data from the Management Histroy Database. The data points are pulled from the federal fisheries managment regulations for the southeast region of the United States. This dataset is actively being mutated into a usable format for researchers' to use. Currently, the project is focused on the Gulf of Mexico Reef Fish Resources FMP data. 

### The data cleaning and analysis takes place using R and requires the following packages: 
* here
* tidyverse
* lubridate
* googlesheets4
* dotenv
* ROracle
* keyring

### To execute the code most efficiently, run the main_MH_prep.R file to run the entirety of the code available. Within the main_MH_prep.R file you will find links to the following files: 
1. Bug related cleaning (MH_data_bugs.R)
2. Pre-processing clean up (MH_pre_processing.R)
3. Cluster sorting and date fill-ins (MH_process.R)
4. Cleaning of dates (MH_spp_expansion.R)
5. Post-processing by collection - grouping related mtypes (in progress, not included yet)

### The code is still being edited and updated weekly. 
Bugs have been identified and discussed within the issues tab. Known permanent bugs are included in the [appendix](https://docs.google.com/document/d/1Sby7u3XKtg06HAFJmS8x0fvtaX0yqydQeOYfy-uNeyM/edit#heading=h.49x2ik5) of the meta data documents provided for [data entry](https://docs.google.com/document/d/18k_0_Y9DFTp7fFEo8yivoMowbqdCAddfEcWtdNDM8eA/edit), [processing](https://docs.google.com/document/d/1l1DqJUVhFwBkokm5tZOpsls_coAoSaCG/edit?rtpof=true), and [analysis ready data](https://docs.google.com/document/d/1od-zSiffovacy5wCg9pOdd1xTcywzAUYDyJeaJmCC_o/edit). 

# Disclamer:
This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
