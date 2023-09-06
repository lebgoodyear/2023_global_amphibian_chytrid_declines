############################################################################
########################### Bd vs declines plots ###########################
#################### Pie charts and RL category over time ##################
############################################################################


# Author: Luke Goodyear (lgoodyear01@qub.ac.uk)
# Date created: Jan 2023
# Last edited: Sep 2023


# clear workspace
rm(list=ls())


############################################################################
############################### Set up #####################################


# date corresponding to dataset
dater <- "230724"
# date corresponding to today (day of running)
datt <- "230905"

# load packages
library("tidyr")
library("dplyr")
library("ggplot2")
library("reshape2")
theme_set(theme_bw())
theme_update(axis.line = element_line(colour = "black"),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             panel.border = element_blank(),
             panel.background = element_blank()) 

# load data
data <- read.csv(paste0("~/Documents/scripts/global_amphibian_chytrid_declines_2023/data_full/iucn_olson_dataset_", dater, ".csv"))

# set path for outputs
path_outputs <- "~/Documents/scripts/global_amphibian_chytrid_declines_2023/outputs/"

# check if results directory exists and if not, create it
ifelse(!dir.exists(file.path(paste0(path_outputs, "/", datt, "/"))), 
       dir.create(file.path(paste0(path_outputs, "/", datt, "/"))), 
       FALSE)
# set new output path by date of running
path_out <- paste0(path_outputs, "/", datt, "/")


############################################################################
####################### RL category changes over time ######################


# set categories of interest (VU, EN, CR)
cats <- c(3,4,5)
# set categories to remove (EW, EX)
to_remove <- c(6,7)

# create function to prep data to plot for each time period
prep <- function(df, bd) {
  if (length(which(df$RL1980 %in% to_remove))!=0) {
    df1980 <- df[-which(df$RL1980 %in% to_remove),]
  } else {
    df1980 <- df
  }
  if (bd==1) {
    df1980 <- df[which(df$Bd1980 == 1),]
  }
  df1980p <- df1980 %>% 
    group_by(RL1980) %>%
    summarise(n=sum(RL1980))
  df1980p$prop <- df1980p$n/sum(na.omit(df1980p$n))
  
  if(length(which(df$RL2004 %in% to_remove))!=0){
    df2004 <- df[-which(df$RL2004 %in% to_remove),]
  } else {
    df2004 <- df
  }
  if (bd==1) {
    df2004 <- df[which(df$Bd2004 == 1),]
  }
  df2004p <- df2004 %>% 
    group_by(RL2004) %>%
    summarise(n=sum(RL2004))
  df2004p$prop <- df2004p$n/sum(na.omit(df2004p$n))
  
  if(length(which(df$RL2020 %in% to_remove))!=0){
    df2020 <- df[-which(df$RL2020 %in% to_remove),]
  } else {
    df2020 <- df
  }
  if (bd==1) {
    df2020 <- df[which(df$Bd2020 == 1),]
  }
  df2020p <- df2020 %>% 
    group_by(RL2020) %>%
    summarise(n=sum(RL2020))
  df2020p$prop <- df2020p$n/sum(na.omit(df2020p$n))
  
  
  to_plot <- data.frame(matrix(nrow=3, ncol=6))
  names(to_plot) <- c("Year", "LC", "NT", "VU", "EN", "CR")
  to_plot[,1] <- c(1980, 2004, 2020)
  to_plot[1,2:6] <- df1980p$n[1:5]
  to_plot[2,2:6] <- df2004p$n[1:5]
  to_plot[3,2:6] <- df2020p$n[1:5]
  
  dat <- melt(to_plot, na.rm = FALSE, id = "Year")
  
  to_plot_prop <- data.frame(matrix(nrow=3, ncol=6))
  names(to_plot_prop) <- c("Year", "LC", "NT", "VU", "EN", "CR")
  to_plot_prop[,1] <- c(1980, 2004, 2020)
  to_plot_prop[1,2:6] <- df1980p$prop[1:5]
  to_plot_prop[2,2:6] <- df2004p$prop[1:5]
  to_plot_prop[3,2:6] <- df2020p$prop[1:5]
  
  dat_prop <- melt(to_plot_prop, na.rm = FALSE, id = "Year")
  
  return(list(dat, dat_prop))
}

# plot all species
dfall <- prep(data, bd=0) # doesn't matter what bd is set to in this case
allplot <- ggplot(data=dfall[[2]], aes(x=Year, y=value)) +
            geom_smooth(aes(color=variable)) +
            scale_color_manual(values=c("#003C86", "#3487A5", "#12BFA2", "#E8D91C", "#DA4409")) +
            guides(color=guide_legend(override.aes=list(fill=NA))) +
            labs(x="", y="Proportion of species", color="IUCN Red List category") +
            theme(legend.title = element_text(size = 10),
                  legend.text = element_text(size = 10))

# plot Bd positive only species
dfbd <- prep(data, bd=1) # set bd to one for only bd positive species
bdplot <- ggplot(data=dfbd[[2]], aes(x=Year, y=value)) +
            geom_smooth(aes(color=variable)) +
            scale_color_manual(values=c("#003C86", "#3487A5", "#12BFA2", "#E8D91C", "#DA4409")) +
            guides(color=guide_legend(override.aes=list(fill=NA))) +
            labs(x="", y="Proportion of species", color="IUCN Red List category") +
            theme(legend.title = element_text(size = 10),
                  legend.text = element_text(size = 10))

# save both plots
ggsave(paste0(path_out, "rlcats_1980-2020.png"), allplot)
ggsave(paste0(path_out, "bdrlcats_1980-2020.png"), bdplot)


############################################################################
################################ Plot pies #################################


####################### Function to plot pie chart #########################


# function to create pie chart
plot_pies <- function(pie, col_name, property, labs) {
  
  # INPUTS
  
  # df is data_frame format with at least column refered to by col_name
  # property is the name of the variable of interest to be displayed 
  # on pie chart (could be same or different as col_name)
  # labs are the names of the variables you want printed in the legend
  # (must be vector of strings)
  
  # plot pie chart
  pie_out <- ggplot(pie, aes(x= "", y= Frequency, fill= .data[[property]])) +
    geom_bar(stat = "identity", width = 0.7, alpha=0.8) +
    scale_fill_manual(values=colours, name=col_name, labels=labs) +
    #scale_fill_grey(name=leg_name, labels=c(lab1, lab2, lab3)) +
    coord_polar("y") +
    geom_col(color = "#000000", linewidth=0.8) +
    geom_text(aes(x=1.6, label = .data[["Frequency"]]),
              position = position_stack(vjust = 0.5), size=2) +
    #scale_y_continuous(breaks=cumsum(data_set$Frequency)-data_set$Frequency/2, labels=.data[["Frequency"]]) +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank(),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 10),
          legend.box.spacing = unit(0.001, "cm"),
          legend.spacing = unit(0.8, "cm"),
          legend.key.size = unit(0.75,"line"),
          legend.key = element_rect(fill = "white", colour = "#000000", linewidth=0.02)) +
    guides(fill = guide_legend(nrow = length(labs), byrow = TRUE)) # set space between legend items
  # save pie
  ggsave(file=paste0(path_out,col_name, "_pie_plot.png"), 
         width=210, height=297, units="mm", 
         pie_out)
  
  return(pie_out)
  
}


############################ Plot pie charts ###############################


df <- data # copy data since changes are made directly to column values
colours = c("#FF5608", "#0037a6", "#ffffff")#c("#000000", "#ffffff", "#8aa1a4") # set colours for plots


# split data by order and save as pie chart
pie_order <- as.data.frame(table(df$Order))
names(pie_order) <- c("Order", "Frequency")
# set order of groups in pie chart plot
pie_order$Order <- factor(pie_order$Order, levels=c("Anura", "Caudata", "Gymnophiona"))
order_plot <- plot_pies(pie_order, "Species tested for Bd", "Order", c("Anura", "Caudata", "Gymnophiona"))
order_plot


# split data by Bd and save as pie chart
pie_bd2020 <- as.data.frame(table(df$Bd2020))
names(pie_bd2020) <- c("Bd2020", "Frequency")
# set order of groups in pie chart plot
pie_bd2020$Bd2020 <- factor(pie_bd2020$Bd2020, levels=c("1", "0"))
bd_plot2020 <- plot_pies(pie_bd2020, "Test Results for Bd (2020)", "Bd2020", c("Bd detected", "Bd not detected"))
bd_plot2020


# split data by Bd and save as pie chart
# set NAs to 'No data' so they plot on pie chart
df$Bd2004[which(is.na(df$Bd2004))] <- "No data"
pie_bd2004 <- as.data.frame(table(df$Bd2004))
names(pie_bd2004) <- c("Bd2004", "Frequency")
# set order of groups in pie chart plot
pie_bd2004$Bd2004 <- factor(pie_bd2004$Bd2004, levels=c("1", "0", "No data"))
bd_plot2004 <- plot_pies(pie_bd2004, "Test Results for Bd (2004)", "Bd2004", c("Bd detected", "Bd not detected", "No data"))
bd_plot2004


# split data by Bd and save as pie chart
# set NAs to 'No data' so they plot on pie chart
df$Bd1980[which(is.na(df$Bd1980))] <- "No data"
pie_bd1980 <- as.data.frame(table(df$Bd1980))
names(pie_bd1980) <- c("Bd1980", "Frequency")
# set order of groups in pie chart plot
pie_bd1980$Bd1980 <- factor(pie_bd1980$Bd1980, levels=c("1", "0", "No data"))
bd_plot1980 <- plot_pies(pie_bd1980, "Test Results for Bd (1980)", "Bd1980", c("Bd detected", "Bd not detected", "No data"))
bd_plot1980


# split data by IUCN declines and save as pie chart
# set NAs to 'No data' so they plot on pie chart
df$PopTrend2020[which(is.na(df$PopTrend2020))] <- "No data"
df$PopTrend2020[which(df$PopTrend2020 == 1)] <- 0
pie_pop <- as.data.frame(table(df$PopTrend2020))
names(pie_pop) <- c("PopTrend", "Frequency")
# set order of groups in pie chart plot
pie_pop$PopTrend <- factor(pie_pop$PopTrend, levels=c("-1", "0", "No data"))
pop_plot <- plot_pies(pie_pop, "Population Trend (2020)", "PopTrend", c("Declining", "Stable or increasing", "No data"))
pop_plot


# split data by IUCN category worsening 1980-2004 and save as pie chart
# set NAs to 'No data' so they plot on pie chart
df$StatusChangeRL1980to2004[which(is.na(df$StatusChangeRL1980to2004))] <- "No data"
pie_19802004 <- as.data.frame(table(df$StatusChangeRL1980to2004))
names(pie_19802004) <- c("Statx", "Frequency")
# set order of groups in pie chart plot
pie_19802004$Statx <- factor(pie_19802004$Statx, levels=c("1", "0", "No data"))
s19802004_plot <- plot_pies(pie_19802004, "Category change (1980-2004)", "Statx", c("Category worsened", "Category stable or improved", "No data"))
s19802004_plot


# split data by IUCN category worsening 2004-2020 and save as pie chart
# remove species that remained extinct between 2004 and 2020 (EX includes EX and EW)
df20042020 <- df[-which(df$StatusChangeRL2004to2020 == "EX"),]
df20042020$StatusChangeRL2004to2020[which(is.na(df20042020$StatusChangeRL2004to2020))] <- "No data"
pie_20042020 <- as.data.frame(table(df20042020$StatusChangeRL2004to2020))
names(pie_20042020) <- c("Statx", "Frequency")
# set order of groups in pie chart plot
pie_20042020$Statx <- factor(pie_20042020$Statx, levels=c("1", "0", "No data"))
s20042020_plot <- plot_pies(pie_20042020, "Category change (2004-2020)", "Statx", c("Category worsened", "Category stable or improved", "No data"))
s20042020_plot


# split data by IUCN threatened category 2004 and save as pie chart
dfrl2004 <- data
# remove extinct species
dfrl2004 <- dfrl2004[!(dfrl2004$RL2004 %in% to_remove),]
# group by threatened and non-threatened categories
dfrl2004$Threatened2004 <- NA
for (i in 1:nrow(dfrl2004)) {
  if (!is.na(dfrl2004$RL2004[i])) {
    if (dfrl2004$RL2004[i] %in% cats) {
      dfrl2004$Threatened2004[i] <- 1
    } else {dfrl2004$Threatened2004[i] <- 0}
  }
}
# set NAs to 'No data' so they plot on pie chart
dfrl2004$Threatened2004[which(is.na(dfrl2004$Threatened2004))] <- "No data"
pie_RL2004 <- as.data.frame(table(dfrl2004$Threatened2004))
names(pie_RL2004) <- c("RLcat2004", "Frequency")
# set order of groups in pie chart plot
pie_RL2004$RLcat2004 <- factor(pie_RL2004$RLcat2004, levels=c("1", "0", "No data"))
RL2004_plot <- plot_pies(pie_RL2004, "Threatened 2004", "RLcat2004", c("Threatened category", "Non-threatened category", "No data"))
RL2004_plot


# split data by IUCN threatened category 2020 and save as pie chart
dfrl2020 <- data
# remove extinct species
dfrl2020 <- dfrl2020[!(dfrl2020$RL2020 %in% to_remove),]
# group by threatened and non-threatened categories
dfrl2020$Threatened2020 <- NA
for (i in 1:nrow(dfrl2020)) {
  if (!is.na(dfrl2020$RL2020[i])) {
    if (dfrl2020$RL2020[i] %in% cats) {
      dfrl2020$Threatened2020[i] <- 1
    } else {dfrl2020$Threatened2020[i] <- 0}
  }
}
# set NAs to 'No data' so they plot on pie chart
dfrl2020$Threatened2020[which(is.na(dfrl2020$Threatened2020))] <- "No data"
pie_RL2020 <- as.data.frame(table(dfrl2020$Threatened2020))
names(pie_RL2020) <- c("RLcat2020", "Frequency")
# set order of groups in pie chart plot
pie_RL2020$RLcat2020 <- factor(pie_RL2020$RLcat2020, levels=c("1", "0", "No data"))
RL2020_plot <- plot_pies(pie_RL2020, "Threatened 2020", "RLcat2020", c("Threatened category", "Non-threatened category", "No data"))
RL2020_plot


## end of script
