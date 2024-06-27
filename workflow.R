################################################################################
######################### Bd vs declines workflow ##############################
################################################################################


# Author: Luke Goodyear (lgoodyear01@qub.ac.uk)
# Date created: Dec 2022
# Last edited: Jun 2024

# clear workspace
rm(list=ls())


################################################################################
########################### REQUIRED: User inputs ##############################


print("Reading in user inputs...")


################################# Data #########################################


## date corresponding to dataset
dater <- "240117" # note that sampling bias analysis will always 
# run on 231123, regardless of dater value
# date corresponding to today (day of running)
datt <- "240625" #"240429"

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

# select response variable by column name in dataframe, one of:
# (1) RL2004
# (2) RL2020
# (3) StatusChangeRL1980to2004
# (4) StatusChangeRL2004to2020
resp <- "RL2004"
# note that correct year of Bd predictor is chosen automatically

# select one of the following if subset is being run
# (1) NA to run full dataset
# (2) "tropical"
# (3) "temperate"
sub <- "temperate"

# select one to run analysis for checking
# (1) NA for actual analysis
# (2) "sampling_biases" to test for sampling bias in results
# (3) "species_continuity" to test only species tested in both time periods
testv <- NA

## end of command line variables


# Which parts of main program do you want to run?

# UNIVARIATE ANALYSIS
# set to 1 to run, 0 to not run

# FREQUENTIST
# plot mosaics for chosen variable?
plot_mosaics <- 1
# calculate chi-squared and related phi correlation coefficient?
calc_chi_phi <- 1
# run frequentist logistic regression model?
calc_freq_log <- 1

# BAYESIAN
# set number of cores for Bayesian models (how many cores does your computer have?)
num_cores <- 4
# run Bayesian logisitic regression model?
calc_bay_log <- 1
# set iterations
iter_log <- 5000
# run model controlling for phlogeny?
calc_phylo <- 1
# set iterations
iter_phylo <- 5000


########################### If using command line ##############################


# if running from command line, this will overwrite area, var and date variables
# run from the command line locally using:
# Rscript --vanilla workflow.R "RL2004" "tropical"
# import command line arguments
#!/usr/bin/env Rscript
# set up to accept arguments from command
# load arguments from command line
args <- commandArgs(trailingOnly=TRUE)
# load arguments into script as required variable names
if (length(args) > 0) { # check to see if running from command line
  resp <- args[1]
  sub <- args[2]
} 


################################################################################
####################### OPTIONAL: Advanced variables ###########################


# these variables are automatically set up but can be changed manually if required


# settings for variable groups
if (resp %in% c("RL2004", "RL2020")) {
  # set any unwanted/undefined variables to be removed
  to_remover <- c(6,7) # remove extinct and extinct in wild
  # specify variable/s of interest, varr_int, so that new binary column will be 
  # 'yes' (1) if equal to this variable/s and 'no' (0) otherwise
  varr_int <- c(3,4,5)
  # for contingency tables and mosaic plots
  # names for response labels
  key_name <- "Threatened status" 
  varr_int_name <- "Threatened"  # corresponding to 1 factor level
  varr_other_name <- "Not threatened" # corresponding to 0 factor level
} else if (resp %in% c("StatusChangeRL1980to2004", "StatusChangeRL2004to2020")) {
  # set any unwanted/undefined variables to be removed
  to_remover <- c("EX") # remove stable extinct for category change
  # resp is already a binary column so we do not need to specify
  # the variables of interest
  varr_int <- NULL 
  # for contingency tables and mosaic plots
  # names for response labels
  key_name <- "Category worsened"
  varr_int_name <- "Worsened"  # corresponding to 1 factor level
  varr_other_name <- "Stable/increasing" # corresponding to 0 factor level
} else {
  stop("Response variable name (resp) contains an error. It must be one of: 
        RL2004, RL2020, StatusChangeRL1980to2004, StatusChangeRL2004to2020")
}


# settings for individual response variables
if (resp == "RL2004") {
  # name of new binary column 
  varr <- "Threatened2004"
  # set predictor column to Bd detections for correct year (matching response)
  varp <- "Bd2004"
}
if (resp == "RL2020") {
  # name of new binary column 
  varr <- "Threatened2020"
  # set predictor column to Bd detections for correct year (matching response)
  varp <- "Bd2020"
}
if (resp == "StatusChangeRL1980to2004") {
  varr <- resp # resp is already binary column
  # set predictor column to Bd detections for correct year (matching response)
  varp <- "Bd2004"
}
if (resp == "StatusChangeRL2004to2020") {
  varr <- resp # resp is already a binary column
  # set predictor column to Bd detections for correct year (matching response)
  varp <- "Bd2020"
}


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
# run to subset data as required
source(paste0(scripts_path, "subsetting.R"))


################################################################################
########################## Create output directory #############################


# check if results directory exists and if not, create it
if (!is.na(sub)) {
  ifelse(!dir.exists(file.path(paste0(output_dir_core, sub, "/", resp, "_", varp, "/"))),
         dir.create(file.path(paste0(output_dir_core, sub, "/", resp, "_", varp, "/")), recursive=T), 
         FALSE)
  path_base <- paste0(output_dir_core, sub, "/")
  path_out <- paste0(path_base, resp, "_", varp, "/")
} else {
  ifelse(!dir.exists(file.path(paste0(output_dir_core, resp, "_", varp, "/"))), 
         dir.create(file.path(paste0(output_dir_core, resp, "_", varp, "/")), recursive=T), 
         FALSE)
  path_base <- paste0(output_dir_core)
  path_out <- paste0(path_base, resp, "_", varp, "/")
}


################################################################################
############################# Run analysis scripts #############################


print("Running plots script...")
# run script to plot line graphs and pie charts
source(paste0(scripts_path, "plots.R"))

# this script formats the user input variables for the main script
print("Running set up script...")
source(paste0(scripts_path, "analysis_prep.R"))
       
print("Running main script...")
# run script with frequentist stats
source(paste0(scripts_path, "main.R"))

print("Running phylogeny-specific script...")
# run script with bayes models
source(paste0(scripts_path, "main_bayes.R"))


print("Script finished.")


## end of script
