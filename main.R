################################################################################
########################## Bd vs declines analysis #############################
################################################################################


# Author: Luke Goodyear (lgoodyear01@qub.ac.uk)
# Date created: Dec 2022
# Last edited: Sep 2023


################################################################################
############################# Run set up script ################################


# stop Rplots.pdf from being automatically produced when run on command line
pdf(NULL)


################################################################################
########################### Contingency tables #################################


if((plot_mosaics == 1) | (calc_chi_phi == 1)) {
  print("Creating contingency tables...")
  # create contingency tables for var2 and variable of interest
  cont_tab <- create_cont_tab(df, varr, varp, varr_int_name, varr_other_name,
                              varp_int_name, varp_other_name)
  # save var2 and variable of interest breakdown table
  write.csv(cont_tab[[3]], paste0(path_out, varr, "_", varp, "_breakdown.csv"))
}


################################################################################
############################## Plot mosaics ####################################


if (plot_mosaics == 1) {
  print("Plotting mosaics...")
  # view data graphically
  
  # swap columns 1 and 2 to get variable of interest on bottom of bar stack
  obsvd <- cont_tab[[1]][,c(2,1)]
  exptd <- cont_tab[[2]][,c(2,1)]
  
  # plot mosaics for both observed and expected data
  plot_obsvd <- plot_mosaic(data=obsvd, 
                            var1=varr, var2=varp, 
                            leg_name=key_name, 
                            var1_lab1=varr_int_name, var1_lab2=varr_other_name,
                            var2_lab1=varp_int_name, var2_lab2=varp_other_name)
  plot_exptd <- plot_mosaic(data = exptd, 
                            var1=varr, var2=varp, 
                            leg_name=key_name, 
                            var1_lab1=varr_int_name, var1_lab2=varr_other_name,
                            var2_lab1=varp_int_name, var2_lab2=varp_other_name)
  
  # plot on same grid
  mosaic_plot <- ggarrange(plot_obsvd, plot_exptd, ncol=2, 
                           labels = c("Observed", "Expected"),
                           common.legend = TRUE, legend="right")
  print(mosaic_plot)
  
  # save as png
  ggsave(file=paste0(path_out, varr, "_", varp, "_mosaic_plot.png"), 
         width=210, height=297, units="mm", 
         mosaic_plot)
}


################################################################################
############################# Run base stats ###################################


print("Calculating stats...")

if (calc_chi_phi == 1) {
  # calculate X2 and phi
  chi_phi <- generate_chi_phi(cont_tab)
  # view results
  print(chi_phi)
  # save results
  sink(paste0(path_out, varr, "_", varp, "_stats_results.txt"), append=TRUE)
  cat("\nResults from Chi-Squared:\n\n")
  print(chi_phi)
  sink()
}

if (calc_freq_log == 1) {
  # Frequentist logistic regression
  logit <- glm(get(varr) ~ get(varp), data=df, family=binomial(link="logit"))
  # view results
  summary(logit)
  # save results
  sink(paste0(path_out, varr, "_", varp, "_stats_results.txt"), append=TRUE)
  cat("\n\nResults from Frequentist Logistic Regression:\n")
  print(summary(logit))
  sink()
}


print("Script completed")


# end of script
