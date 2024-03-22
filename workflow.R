################################################################################
######################### Bd vs declines workflow ##############################
################################################################################


# Author: Luke Goodyear (lgoodyear01@qub.ac.uk)
# Date created: Dec 2022
# Last edited: Mar 2024

# clear workspace
rm(list=ls())


################################################################################
########################### REQUIRED: User inputs ##############################


print("Reading in user inputs...")


################################# Data #########################################


## date corresponding to dataset
dater <- "240117" #"231123" #"230724" # note that sampling bias analysis will always 
# run on 231123, regardless of dater value
# date corresponding to today (day of running)
datt <- "240317" #240208" #231123a"#"230905"

# set path in which to save output subdirectories
output_dir_core <- paste0("~/Documents/scripts/2023_global_amphibian_chytrid_declines/outputs/", datt,"/")

# path to scripts and data
scripts_path <- "~/Documents/scripts/2023_global_amphibian_chytrid_declines/" 
data_path <- "~/Documents/scripts/2023_global_amphibian_chytrid_declines/data/" 

# read in phylogeny
library("ape")
phylo <- read.nexus(paste0(data_path, "amphibia_nexus_spp.txt"))

# read in data
df <- read.csv(paste0(data_path, "iucn_olson_dataset_", dater, "_full.csv"))


############################# Basic variables ##################################


## note the following variables will be overwritten if running from commandline

# select response variable by column name in dataframe
resp <- "RL1980" # or "StatusChangeRL2004to2020" or "PopTrend2020" for example
# select predictor variable by column name in dataframe
pred <- "Bd1980"
# note that if the response includes a date or date range and we are using
# Bd as the predictor, the date of Bd must match the date (or final date if range)

# select if subset is being run
# (1) NA to run full dataset
# (2) "sampling_biases"
# (3) "tropical"
# (4) "temperate"
# (5) "species_continuity"
sub <- NA#"temperate"

## end of command line variables


# Which parts of main program do you want to run?

# UNIVARIATE ANALYSIS
# set to 1 to run, 0 to not run
# plot mosaics for chosen variable?
plot_mosaics <- 1
# calculate chi-squared and related phi correlation coefficient?
calc_chi_phi <- 1
# run frequentist logistic regression model?
calc_freq_log <- 1
# run Bayesian logsitic regression model?
calc_bay_log <- 1
# set iterations
iter_log <- 10000
# run model controlling for phlogeny?
calc_phylo <- 0
# set iterations
iter_phylo <- 5000


########################### If using command line ##############################


# if running from command line, this will overwrite area, var and date variables
# run from the command line locally using:
# Rscript --vanilla workflow.R "RL2004" "Bd2004" "tropical"
# import command line arguments
#!/usr/bin/env Rscript
# set up to accept arguments from command
# load arguments from command line
args <- commandArgs(trailingOnly=TRUE)
# load arguments into script as required variable names
if (length(args) > 0) { # check to see if running from command line
  varr <- args[1]
  varp <- args[2]
  sub <- args[3]
} 


################################################################################
####################### OPTIONAL: Advanced variables ###########################


# if response has multiple categories (is not 1/0 binary)
# first set any unwanted/undefined variables to be removed (otherwise set to NA)
to_remover <- c("EW","EX")
# c("EX") to remove stable extinct for category change
# c("EW","EX") to remove extinct and extinct in wild for category
# NA for pop trend
# then specify variable of interest so that new binary column will be 'yes' (1) if 
# equal to this variable and 'no' (0) otherwise (this variable is ignored if 
# column is already binary)
varr_int <- c(3,4,5)
# NA for status change
# c(3,4,5) for category
# -1 for decreasing variable for population trend
# name of new binary colunn
varr <- "Threatened1980"
# NA for category change 
# "Threatened2020" for category
# "PopDecreasing2020" for pop trend
# if response column is already binary column, set variable to equal column name
if (is.na(varr)) {
  varr <- resp  
}

# for contingency tables and mosaic plots
# names for response labels
key_name <- "Threatened status" #"Category worsened" #"Threatened status" #"Population trend"
varr_int_name <- "Threatened"  #"Worsened" #"Threatened" # "Declining" corresponding to 1 factor level
varr_other_name <- "Not threatened" # "Stable/increasing" #"Not threatened" # "Stable/increasing" corresponding to 0 factor level

# set predictor equal to variable varp
varp <- pred

# for contingency tables and mosaic plots
# names for predictor labels
varp_int_name <- "Bd detected"
varp_other_name <- "Bd not detected"


# set colours for plotting stacked bar charts
cols_bar <- c("#ffffff", "#000000", "#8aa1a4")#c("#989898", "#CCCCCC", "#323232")


# set seed for repeatability
set.seed(26)


################################################################################
########################### Subsetting the data ################################


print("Running subsetting script...")
# run plot to subset data as needed
source(paste0(scripts_path, "subsetting.R"))


################################################################################
########################## Create output directory #############################


# check if results directory exists and if not, create it
if (!is.na(sub)) {
  ifelse(!dir.exists(file.path(paste0(output_dir_core, sub, "/", resp, "_", pred, "/"))),
         dir.create(file.path(paste0(output_dir_core, sub, "/", resp, "_", pred, "/")), recursive=T), 
         FALSE)
  path_base <- paste0(output_dir_core, sub, "/")
  path_out <- paste0(path_base, resp, "_", pred, "/")
} else {
  ifelse(!dir.exists(file.path(paste0(output_dir_core, resp, "_", pred, "/"))), 
         dir.create(file.path(paste0(output_dir_core, resp, "_", pred, "/")), recursive=T), 
         FALSE)
  path_base <- paste0(output_dir_core)
  path_out <- paste0(path_base, resp, "_", pred, "/")
}


################################################################################
############################# Run analysis scripts #############################


print("Running plots script...")
# run script to plot line graphs and pie charts
source(paste0(scripts_path, "plots.R"))

# this script formats the user input variables for the main script
# by subsetting dataset by specified variables and creating output directories
print("Running set up script...")
source(paste0(scripts_path, "analysis_prep.R"))
       
print("Running main script...")
# run script with all base stats
source(paste0(scripts_path, "main.R"))

print("Running phylogeny-specific script...")
# run script with all methods of controlling for phylogeny
source(paste0(scripts_path, "main_phylo.R"))


print("Script finished.")


## end of script
