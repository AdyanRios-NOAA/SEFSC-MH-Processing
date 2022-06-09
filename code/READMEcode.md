# Read Me - Code
Within this folder is the code used to clean and analyze the data collected in the Management History Database. 

### The data cleaning and analysis takes place using R and requires the following packages: 
* here
* tidyverse
* lubridate
* googlesheets4
* dotenv
* ROracle
* keyring

### To execute the code most efficiently, run the main_MH_prep.R file to run the entirety of the code available. Within the main_MH_prep.R file you will find links to the following files: 
0. Bug related cleaning (MH_data_bugs.R), Clusters and expansions ('data/interim'), and Species table reformatting and standardization (MH_clean_spp_tables.R)
1. Create new variables (MH01_new_variables.R)
2. Create sector and cluster IDs (MH02_static_ids.R)
3. Fill in dates (MH03_dates.R)
4. Species expansion and clean up dates (MH04_spp_expansion.R)
6. Process Adjustments (in progress, not included yet)

### The code is still being edited and updated weekly. 
Bugs have been identified and discussed within the issues tab. Known permanent bugs are included in the [appendix](https://docs.google.com/document/d/1Sby7u3XKtg06HAFJmS8x0fvtaX0yqydQeOYfy-uNeyM/edit#heading=h.49x2ik5) of the meta data documents provided for [data entry](https://docs.google.com/document/d/18k_0_Y9DFTp7fFEo8yivoMowbqdCAddfEcWtdNDM8eA/edit), [processing](https://docs.google.com/document/d/1l1DqJUVhFwBkokm5tZOpsls_coAoSaCG/edit?rtpof=true), and [analysis ready data](https://docs.google.com/document/d/1od-zSiffovacy5wCg9pOdd1xTcywzAUYDyJeaJmCC_o/edit). 
