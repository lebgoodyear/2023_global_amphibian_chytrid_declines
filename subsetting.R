################################################################################
######################## Code for subsetting data ##############################
################################################################################


# Author: Luke Goodyear (lgoodyear01@qub.ac.uk)
# Date created: Dec 2023
# Last edited: Sept 2024


################################################################################
############################## Biases in sampling ##############################
  
  
# do we have a higher Bd-detection rate in 2004-2020 since we know better where/
# when to look?
  
  
if (sampling_biases == TRUE) {
  # read in different dataset corresponding to splitting the dataset by year 
  # (partitioning data completely by time, i.e. not cumulative)
  df <- read.csv(paste0(data_path, "iucn_olson_dataset_231123_sampling_biases.csv"))
}

  
################################################################################
################# Using the same species in both time periods ##################
  
  
### Analysis on same species for all analysis to account for 'susceptibility' bias
  
  
if (species_continuity == TRUE) {
  # subset 2020 dataset to include only those species present in the 2004 dataset
  df_sub <- df[which(!is.na(df$Bd2004)),]
  df_sub <- df_sub[which(!is.na(df_sub$RL2004)),]
  # subset 2004 dataset by removing any NA in RL2020 or Bd2020
  df_sub <- df_sub[which(!is.na(df_sub$RL2020)),]
  df_sub <- df_sub[which(!is.na(df_sub$Bd2020)),]
  # remove unwanted/undefined variables
  df_sub <- df_sub[which(!(df_sub$RL2004 %in% to_remover)),]
  df_sub <- df_sub[which(!(df_sub$RL2020 %in% to_remover)),]

  #data <- df_sub
  df <- df_sub
}


# only run if subsetting is required
if (!is.na(sub)){
  
  ##############################################################################
  ########################### Tropical/temperate ###############################
  
  
  ### Do we see different patterns in tropical/temperate species?
  
  
  if (sub %in% c("tropical", "temperate")) {
    # separate temperate/tropical
    df <- df[!is.na(df$Latitude),]
    df$Climate <- "Tropical"
    for (i in 1:nrow(df)) {
      if ((df$Latitude[i] > 23.5) | (df$Latitude[i] < -23.5)){
        df$Climate[i] <- "Temperate"
      }
    }
    
    # set up tropical dataset
    if (sub == "tropical") {
      df_trop <- df[which(df$Climate=="Tropical"),]
      df <- df_trop
    }
    
    # set up temperate dataset
    if (sub == "temperate") {
      df_temp <- df[which(df$Climate=="Temperate"),]
      df <- df_temp
    }
  }
}


## END OF SCRIPT
