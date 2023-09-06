################################################################################
######################### Bd vs declines functions #############################
################################################################################


# Author: Luke Goodyear (lgoodyear01@qub.ac.uk)
# Date created: Dec 2022
# Last edited: Sep 2023


################################################################################
################################# Set up #######################################


# load packages
library("ggplot2")
library("reshape2") # for melting data for plotting
library("ggpubr") # for plotting on same grid and shared legend
library("ape") # for dealing with phylogenetic trees
library("geiger") # for name check
library("tibble") # for removing rownames and converting a column to rownames
library("phytools") # for force.ultrametric()
library("MCMCglmm") # for inverseA()


################################################################################
############ Function to create new 1/0 column for binary variables ############


# create new columns for binary variables
make_new_col <- function(data, new, old, threshvar) {
  
  # INPUTS
  
  # threshvar should be a vector, even if only one value,
  # e.g. c("Decreasing")
  data[[new]] <- NA
  for (i in 1:nrow(data)) {
    # set up binary variable for threshvar
    if (!is.na(data[[old]][i])) { # set NA values as NA
      if (data[[old]][i] %in% threshvar) {
        data[[new]][i] <- 1
      } else {data[[new]][i] <- 0}
    }
  }
  return(data)
}


################################################################################
##################### Function to set up contingency tables ####################


create_cont_tab <- function(data, var1, var2, 
                            var1_int_name, var1_other_name,
                            var2_int_name, var2_other_name) {
  
  # create table of observed values
  
  cont_tab <- as.data.frame(matrix(nrow=2, ncol=2))
  names(cont_tab) <- c(var1_int_name, var1_other_name)
  row.names(cont_tab) <- c(var2_int_name, var2_other_name)
  col1row1 <- 0
  col1row2 <- 0
  col2row1 <- 0
  col2row2 <- 0
  for (i in 1:nrow(data)) {
    if (data[[var1]][i] == 1) {
      if (data[[var2]][i] == 1) {
        col1row1 <- col1row1 + 1
      } 
      if (data[[var2]][i] == 0) {
        col1row2 <- col1row2 + 1
      }
    }
    if (data[[var1]][i] == 0) {
      if (data[[var2]][i] == 1) {
        col2row1 <- col2row1 + 1
      } 
      if (data[[var2]][i] == 0) {
        col2row2 <- col2row2 + 1
      }
    }
  }
  cont_tab[1,1] <- col1row1
  cont_tab[1,2] <- col2row1
  cont_tab[2,1] <- col1row2
  cont_tab[2,2] <- col2row2
  
  # create table of expected values
  
  exp_tab <- as.data.frame(matrix(nrow=2, ncol=2))
  names(exp_tab) <- c(var1_int_name, var1_other_name)
  row.names(exp_tab) <- c(var2_int_name, var2_other_name)
  
  exp_tab[1,1] <- ((col1row1 + col2row1)*(col1row1 + col1row2))/nrow(data)
  exp_tab[1,2] <- ((col1row1 + col2row1)*(col2row1 + col2row2))/nrow(data)
  exp_tab[2,1] <- ((col1row2 + col2row2)*(col1row1 + col1row2))/nrow(data)
  exp_tab[2,2] <- ((col1row2 + col2row2)*(col2row1 + col2row2))/nrow(data)
  
  # create table with totals for checking
  cont_tab_w_tots <- cont_tab
  cont_tab_w_tots$Totals <- c((col1row1 + col2row1), (col1row2 + col2row2))
  cont_tab_w_tots[3,] <- c(col1row1 + col1row2, col2row1 + col2row2,nrow(data))
  row.names(cont_tab_w_tots)[3] <- "Totals"
  
  tabs <- list(cont_tab, exp_tab, cont_tab_w_tots)
  
  return(tabs)
  
}
# note code to auto-create contigency table (not for expected, only observed) is:
# cont_tab_auto <- xtabs( ~ var1 + var2, data=data)


############################################################################
##################### Function to generate mosaic plot #####################


plot_mosaic <- function(data, var1, var2, leg_name, var1_lab1, var1_lab2, var2_lab1, var2_lab2) {

  # melt for plotting
  datatm <- melt(as.matrix(data), varnames = c(var2, var1), id.vars = var2)
  # calculate proportions
  datatm$prop <- datatm$value/sum(datatm$value)
  
  ## bar widths
  # initialise empty vector to store Bd proportions for bar widths
  proptab <- c()
  # calculate proportions of Bd +ve and Bd -ve
  proptab[1] <- sum(datatm$prop[which(datatm[[var2]] == var2_lab1)])
  proptab[2] <- sum(datatm$prop[which(datatm[[var2]] != var2_lab1)])
  # extend to vector length 4 for 4 bars
  proptab[3] <- proptab[1]
  proptab[4] <- proptab[2]
  
  ## bar heights
  height=datatm$prop/proptab
  ## bar locations
  centre=c(0, cumsum(proptab)[1]) + proptab / 2
  
  # combine plotting data with actual data
  dataplot <- as.data.frame(cbind(datatm, proptab, centre, height))
  
  # plot data
  out <- ggplot(dataplot, aes(centre, height)) +
    geom_bar(stat = "identity", aes(width = proptab, fill = .data[[var1]]), col = "#8aa1a4") +
    geom_text(aes(label = c(var2_lab1, var2_lab2, "", ""), x = centre, y = -0.05)) +
    geom_text(aes(x = centre, y = height, label = round(value, 0), fill=.data[[var1]]),
              position = position_stack(vjust = 0.5), size = 4, col="#8aa1a4") +
    scale_fill_manual(values=cols_bar, 
                      name=leg_name, 
                      labels=c(var1_lab2, var1_lab1), 
                      guide=guide_legend(reverse=T,
                                         nrow = 2,
                                         byrow= T)) +
    ylab("Frequency") +
    theme(axis.line=element_blank(),
          axis.text.y=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank(),
          legend.title=element_text(size=10),
          legend.text=element_text(size=10),
          legend.box.spacing=unit(0.001, "cm"),
          legend.spacing=unit(0.8, "cm"),
          legend.key.size=unit(0.75,"line"),
          legend.key=element_rect(fill="white", colour="#8aa1a4", linewidth=0.2))
  
  # return plot
  return(out)
  
}


############################################################################
########### Function to manually calculate chi-squared statistic ###########


calculate_X2 <- function(ob, ex) {
  X2 <- 0
  for (i in 1:2) {
    for (j in 1:2) {
      x <- ((ob[i,j]-ex[i,j])^2)/ex[i,j]
      X2 <- X2 + x
    }
  }
  return(X2)
}


############################################################################
######## Function to manually calculate phi correlation coefficient ########


calculate_phi <- function(ob) {
  ob <- as.matrix(ob)
  ob<- unname(ob)
  numerator <- (ob[1,1]*ob[2,2] - ob[1,2]*ob[2,1])
  denominator <- sqrt((rowSums(ob)[1]*rowSums(ob)[2]*colSums(ob)[1])*colSums(ob)[2])
  phi <- numerator/denominator
  return(phi)
} 


############################################################################
######## Function to generate chi and phi values and store as list #########


generate_chi_phi <- function(tabs) {
  
  # manually calculate X2
  #X2_man <- calculate_X2(tabs[[1]], tabs[[2]])
  
  # perform X2 test
  X2 <- chisq.test(tabs[[1]], correct = FALSE)
  
  # manually calculate phi
  phi <- calculate_phi(tabs[[1]])
  
  # combine results into list
  res <- list("X2" = X2,
              "phi_man" = phi)
  
  return(res)
  
}


############################################################################
##################### Function to prep for MCMC GLMM #######################


# compare and match phylogeny and dataset in prep
phylo_compare <- function(df, phylo) {
  
  ## INPUTS
  # df is dataframe containing fixed effect variables
  # phylo is phylogeny of class "phylo"
  
  ## OUTPUTS
  # list containing three objects
  # 1) data frame subset by phylogeny
  # 2) phylogeny subset by data
  # 3) matrix of inverse phylogeny
  
  # remove any blank entries in df by replacing with current taxa name
  rownames(df) <- 1:nrow(df)
  for (i in as.numeric(as.character(rownames(df[which(df$PhyloName == ""),])))) {
    df$PhyloName[i] <- df$Taxa[i]
  }
  
  # replace spaces with dashes to match phylogeny
  df$PhyloName <- gsub(" ", "_", df$PhyloName)
  
  # check taxa names are the same in phylogeny and data
  # first use tibble functions to set data rownames as taxonomy
  #print(has_rownames(df)) # check to see if df has rownames
  df <- remove_rownames(df)
  df <- column_to_rownames(df, "PhyloName")
  
  # perform a name check
  nmch <- name.check(phylo, df)
  
  if (length(nmch) == 1) {
    # remove taxa from phylogeny that are not in data
    newphy <- phylo
  }
  
  # remove data that is not in phylogeny
  if (length(nmch) == 2) {
    # convert taxa row names to a column
    df <- rownames_to_column(df, "PhyloName")
    
    # only keep taxa that are in tree
    df <- df[which(!(df$PhyloName %in% nmch$data_not_tree)),]
  
    # remove taxa from phylogeny that are not in data
    newphy <- drop.tip(phylo, tip=nmch$tree_not_data, rooted=TRUE)
  
    # check taxa names are the same in phylogeny and data again
    # first use tibble functions to set data rownames as taxonomy
    #print(has_rownames(df)) # check to see if df has rownames
    df <- remove_rownames(df)
    df <- column_to_rownames(df, "PhyloName")
  
    # perform a second namecheck, which should show complete match
    nmch <- name.check(newphy, df) 
  }
  print("If 'OK' then phylo names match and you can proceed: ")
  print(nmch)
  
  # convert taxa row names to a column
  df <- rownames_to_column(df, "PhyloName")
  
  # check phylogney is ultrametric
  #print(is.ultrametric(newphy))
  # if not, assign tree as ultrametric (this does not perform any actual changes)
  newphy <- force.ultrametric(newphy)
  
  # root tree if unrooted (as must be rooted to find inverse)
  if (is.rooted(newphy) == FALSE) {
    newphy$root.edge <- 0
  }
  
  # get inverse phylogeny
  inv.phylo <- inverseA(newphy)$Ainv
  
  # return data, phylogeny and inverse phylogeny
  return(list(df, newphy, inv.phylo))
  
}


## end of script
