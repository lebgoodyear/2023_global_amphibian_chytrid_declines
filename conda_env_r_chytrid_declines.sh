# Script: conda_env_r_chytrid_declines.sh

# Author: Luke Goodyear lgoodyear01@qub.ac.uk
# Date created: Apr 2022
# Last edited: Sep 2023

# Description:
# 1) cleans conda environment 
# 2) creates new environment and installs packages
# 3) cleans conda environment
# Arguments: None


# remove any unused packages and caches
conda clean -a -y

# create new environment in one go with all packages to prevent dependency conflicts
echo "Creating new R conda environment..."
conda create -n r_chytrid_declines -c conda-forge r-base=4.1.0 r-dplyr r-tidyr r-tibble r-reshape2 r-ggplot2 r-ggpubr r-ape r-geiger r-phytools r-mcmcglmm r-brms r-bh r-sf r-rnaturalearth r-rnaturalearthdata -y

# remove any unused packages and caches after installations
conda clean -a -y


## end of script
