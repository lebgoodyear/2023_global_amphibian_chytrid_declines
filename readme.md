# <ins>Current global-scale amphibian declines are not associated with chytrid fungus</ins>

### Descriptions of all datasets and code required to reproduce the analysis and plots for Goodyear et al. "Current global-scale amphibian declines are not associated with chytrid fungus". (2023). 

&nbsp;

## <ins>Datasets</ins>

1. **iucn_olson_dataset_230724.csv** 

This is the main dataset used for analysis and is a concatentation of the following datasets: 

(1) IUCN Red List catogories and populations trends for 2020 (The IUCN Red List of Threatened Species. IUCN Red List of Threatened Species. https://www.iucnredlist.org/en; accessed 26 May 2022. (2022).)

(2) *Bd* detection data provided by Olson, D. H., Ronnenberg, K. L., Glidden, C. K., Christiansen, K. R. & Blaustein, A. R. "Global patterns of the fungal pathogen Batrachochytrium dendrobatidis support conservation urgency". Front Vet Sci 8, art685877 (2021).

(3) Backcast IUCN Red List categories provided by Luedtke, J. A. et al. "Status of the world’s amphibians: emerging threats and ongoing declines". Nature (In press 2023). Note that there is currently an embargo on this dataset and so this data has been removed from the dataset until the embargo is lifted. This means that analysis can currently only be replicated on the IUCN 2020 data (IUCN Red List category 2020 and IUCN population trend 2020).

2. **olson1980_all_fields_230724.csv**

All Bd detections records (not merged at species level) for detections before or during 1980. This dataset is only used to generate maps.

3. **olson2004_all_fields_230724.csv**

All Bd detections records (not merged at species level) for detections before or during 2004. This dataset is only used to generate maps.

4. **olson2020_all_fields_230724.csv**

All Bd detections records (not merged at species level) for detections before or during 2020. This dataset is only used to generate maps.

5. **amphibia_nexus_spp.txt**

Amphibian phylogenetic tree from Jetz, W. & Alexander Pyron, R. "The interplay of past diversification and evolutionary isolation with present imperilment across the amphibian tree of life". Nat Ecol Evol 2(5), 850–858 (2018).

&nbsp;

## <ins>Code</ins>

### <ins>Set up scripts</ins>

1. **conda_env_r_chytrid_declines.sh**

This script can be run through the commandline to create a conda environment containing all R packages required for the analysis. By activating the environment and then running rstudio from the commandline, the conda environment is loaded into rstudio and all packages can be accessed using the library() function, as usual.

### <ins>Plotting scripts</ins>

2. **mapping.R**

This script generates maps to show Bd distributions before 1980, 2004, and 2020 respetively (see Figure 1 in manuscript). Maps are saved as pdfs into a desginated output folder.

3. **plots.R**

This script generates the pie charts for numbers of Bd detections before 1980, 2004, and 2020 respectively (see Figure 1 in manuscript). It also generates line graphs charting the trajectories of each IUCN Red List category through time, both for all species and Bd detected species alone (see Figure 2 in manuscript). Finally, it generates pie charts showing the breakdown of Bd detections in species between 1980 and 2004, and 2004 and 2020 respectively.

Note that the line graphs and many of the pies are not able to be plotted without the embargoed datase. Therefore, running this script without modification will result in errors until the embargoed dataset is released.

### <ins>Analysis scripts</ins>

4. **workflow.R**

All other scripts are called through this script. Here is where you assign all inputs, which are read into the other scripts. All analyses can be run by opening this one script so unless the details of the process are of interest to you (or you get an error!), you shouldn't need to modify any of the below scripts.

5. **functions.R**

This script contains all functions to be called by the other analysis scripts.

6. **analysis_prep.R**

This script prepares the data by setting up a dichotomous variable for the chosen response variable.

7. **main.R**

This script runs the main analysis: creates contingency tables, generates mosaic plots, runs chi-sqaured test, frequentist logistic regression and Bayesian logistic regression.

8. **main_phylo.R**

This script runs the Bayesian phylogenetic analysis (a logistic mixed model including phylogeny as a random effect, coded as a variance-covariance matrix), using the brms package. This is included as a separate script as the analysis can take a long time so there is the option to run entirely separately.

&nbsp;

### End of readme.