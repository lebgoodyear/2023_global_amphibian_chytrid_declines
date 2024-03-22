################################################################################
########################## Bd vs declines analysis #############################
######################### controlling for phylogeny ############################
################################################################################


# Author: Luke Goodyear (lgoodyear01@qub.ac.uk)
# Date created: Apr 2023
# Last edited: Sep 2023


################################################################################
############################# Run set up script ################################


# this script formats the user input variables for the main script
# by subsetting dataset by specified variables and creating output directories
print("Running set up script...")
source(paste0(scripts_path, "analysis_prep.R"))

# stop Rplots.pdf from being automatically produced when run on command line
pdf(NULL)


################################################################################
################################ Load data #####################################


# prep phylogeny
datls <- phylo_compare(df, phylo)
saveRDS(datls, paste0(path_out, varr, "_", varp, "_data.rds"))
traits <- datls[[1]] # first list object is dataset
phylo <- datls[[2]] # second list object is phlogeny


################################################################################
################################### brms #######################################


# load packages
library("brms")

# create variance-covariance matrix
A <- ape::vcv.phylo(phylo)

dat_brms <- as.data.frame(cbind(traits$PhyloName, traits[[varr]], traits[[varp]]))
names(dat_brms) <- c("PhyloName", "varr", "varp")

# model with response, predictor, phylogeny and intercept
# get priors
if (calc_phylo == 1) {
  # get priors
  priors1 <- get_prior(varr ~ 1 + varp + (1|gr(PhyloName, cov = A)), 
                      data = dat_brms, 
                      family = bernoulli(),
                      data2 = list(A = A))
  # run brms model
  mod1 <- brm(varr ~ 1 + varp + (1|gr(PhyloName, cov = A)), 
            data = dat_brms, 
            family = bernoulli(), 
            data2 = list(A = A),
            prior = priors1,
            iter = iter_phylo)
  # view model
  summary(mod1)
  # save model
  saveRDS(mod1, paste0(path_out, varr, "_", varp, "_phylo_mod.rds"))
  # generate diagnostic and posterior distribution plots
  pdf(file=paste0(path_out, "phylo_fixed_postdis.pdf"))
  hist(mod1[["fit"]]@sim[["samples"]][[1]][["b_varp1"]], breaks=50)
  dev.off()
  pdf(file=paste0(path_out, "phylo_fixed_postdis_plots.pdf"))
  plot(mod1)
  dev.off()
}

# model with predictor and intercept only
if (calc_bay_log == 1) {
  # get priors
  priors2 <- get_prior(varr ~ 1 + varp, 
                       data = dat_brms, 
                       family = bernoulli())
  # run brms model
  mod2 <- brm(varr ~ 1 + varp, 
              data = dat_brms, 
              family = bernoulli(), 
              prior = priors2,
              iter = iter_log)
  # view model
  summary(mod2)
  # save model
  saveRDS(mod2, paste0(path_out, varr, "_", varp, "_fixed_mod.rds"))
  # generate diagnostic and posterior distribution plots
  pdf(file=paste0(path_out, "fixed_postdis.pdf"))
  hist(mod2[["fit"]]@sim[["samples"]][[1]][["b_varp1"]], breaks=50)
  dev.off()
  pdf(file=paste0(path_out, "fixed_postdis_plots.pdf"))
  plot(mod2)
  dev.off()
}

# read models in for plotting for further analysis
#mod1 <- readRDS(paste0(path_out, varr, "_", varp, "_phylo_mod.rds"))
#mod2 <- readRDS(paste0(path_out, varr, "_", varp, "_fixed_mod.rds"))


################################################################################
################################# Output results ###############################


sink(file=paste0(path_out, "brms_results.txt"))
print("brms - fixed effect only")
print("")
if (calc_bay_log == 1) {print(summary(mod2))}
print("")
print("brms - phylogeny control")
print("")
if (calc_phylo == 1) {print(summary(mod1))}
print("")
sink()


## end of script
