############################################################################
######################## Bd vs declines prep ###############################
############################################################################


# Author: Luke Goodyear (lgoodyear01@qub.ac.uk)
# Date created: Dec 2022
# Last edited: Sep 2023


############################################################################
######################### Load prep functions ##############################


print("Loading functions...")
source(paste0(scripts_path, "functions.R"))


############################################################################
########################## Variable set up #################################


print("Removing NAs from dataset...")
df <- df[!is.na(df[[pred]]),]
df <- df[!is.na(df[[resp]]),]

# if response column is not already a binary column, convert it to one
if (length(setdiff(df[[resp]], c(0, 1))) != 0) {
  if (!is.na(to_remover)) {
    print("Removing undesginated entries from response...")
    df <- df[!(df[[resp]] %in% to_remover),]
  }
  if (!is.na(varr_int)) {
    print("Setting up new binary column variable for response...")
    df <- make_new_col(df, varr, resp, varr_int)
  }
}

# if predictor column is not already a binary column, convert it to one
if (length(setdiff(df[[pred]], c(0, 1))) != 0) {
  if (!is.na(to_removep)) {
    print("Removing undesginated entries from predictor...")
    df <- df[!(df[[pred]] %in% to_removep),]
  }
  print("Setting up new binary column variable for predictor...")
  df <- make_new_col(df, varp, pred, varp_int)
}

# make sure both columns are numeric
df[[varr]] <- as.numeric(df[[varr]])
df[[varp]] <- as.numeric(df[[varp]])


print("Preparation script completed.")


# end of script
