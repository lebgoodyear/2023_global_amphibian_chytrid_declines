################################################################################
############################## Descriptive analysis ############################
################################################################################


# Author: Luke Goodyear (lgoodyear01@qub.ac.uk)
# Date created: Sept 2024
# Last edited: Sept 2024


################################################################################
#################################### Bd ########################################


# store descriptive bd stats
sink(paste0(path_out, "descriptive_stats.txt"), append=TRUE)
    cat(
        "\n\nBd TESTS\n",
        "\nTotal number of species tested for Bd:",
        nrow(df),
        "\nTotal number of species tested for Bd between 1980 and 2004:",
        sum(table(df$Bd2004)),
        "\nTotal number of species tested for Bd between 2004 and 2020:",
        sum(table(df$Bd2020))
    )
sink()

# create contingency table for species tested for bd in each time period
chitab_bdtests <- as.data.frame(rbind(
    cbind(
        table(df$Bd2004)[1], 
        table(df$Bd2004)[2]
    ), 
    cbind(
        table(df$Bd2020)[1], 
        table(df$Bd2020)[2]
    )
))
colnames(chitab_bdtests) <- c("NoBd", "Bd")
rownames(chitab_bdtests) <- c("t2004", "t2020")
chitab_bdtests

# calculate bd detection rates in each time period
bd_detection_rate_2004 <- table(df$Bd2004)[2]/(table(df$Bd2004)[1]+table(df$Bd2004)[2])
bd_detection_rate_2004
bd_detection_rate_2020 <- table(df$Bd2020)[2]/(table(df$Bd2020)[1]+table(df$Bd2020)[2])
bd_detection_rate_2020

# store results
sink(paste0(path_out, "descriptive_stats.txt"), append=TRUE)
    cat(
        "\n\n\nBd DETECTIONS\n\n",
        "Breakdown of all Bd+ and Bd- species tested (by time period):\n\n"
    )
    print(chitab_bdtests)
    cat(
        "\n\nBd detection rate in 2004:",
        bd_detection_rate_2004,
        "\n\nBd detection rate in 2020:",
        bd_detection_rate_2020,
        "\n\nTest for difference between Bd detection rates across time periods:\n"
    )
    print(chisq.test(chitab_bdtests))
    cat(
        "Effect size (Phi):",
        calculate_phi(chitab_bdtests)
    )
sink()


################################################################################
################################# Response #####################################


############################### Category #######################################


if (resp %in% c("RL2004", "RL2020")) {


    ################################ Set up ####################################


    # dataset for subsetting
    dft <- df

    # set any EX and EW Red List category assignments to NA
    dft$RL2004[which(dft$RL2004 %in% to_remover)] <- NA
    dft$RL2020[which(dft$RL2020 %in% to_remover)] <- NA

    # crea  te binary columns for threatened
    # set names of new columns
    var2004 <- "Threatened2004"
    var2020 <- "Threatened2020"
    dft <- make_new_col(dft, var2004, "RL2004", varr_int)
    dft <- make_new_col(dft, var2020, "RL2020", varr_int)

    # make sure both columns are numeric
    dft[[var2004]] <- as.numeric(dft[[var2004]])
    dft[[var2020]] <- as.numeric(dft[[var2020]])

    # Set threatened value to NA if Bd was not tested for
    dft[[var2004]][which(is.na(dft$Bd2004))] <- NA
    dft[[var2020]][which(is.na(dft$Bd2020))] <- NA


    ############################### Analysis ###################################


    # create contingency table for breakdown of threatened species across time periods,
    # note this only includes tested species since the RL cat of any not tested species
    # was set to NA
    chitab_resp <- as.data.frame(rbind(
        cbind(
            table(dft[[var2004]])[1], 
            table(dft[[var2004]])[2]
        ), 
        cbind(        
            table(dft[[var2020]])[1], 
            table(dft[[var2020]])[2]
        )
    )) 
    colnames(chitab_resp) <- c(varr_other_name, varr_int_name)
    rownames(chitab_resp) <- c("t2004", "t2020")
    chitab_resp

    # Calculate proprtion of tested species that were threatened in each time period
    tested_resp_2004 <- table(dft[[var2004]])[2]/(table(dft[[var2004]])[1]+table(dft[[var2004]])[2])
    tested_resp_2004
    tested_resp_2020 <- table(dft[[var2020]])[2]/(table(dft[[var2020]])[1]+table(dft[[var2020]])[2])
    tested_resp_2020

    # Calculate proportions of Bd+ species that were not threatened in each time period
    bdpos_notthreat_2004 <- table(dft[[var2020]][which(dft$Bd2020 == 1)])[1]/(table(dft[[var2020]][which(dft$Bd2020 == 1)])[1]+table(dft[[var2020]][which(dft$Bd2020 == 1)])[2])
    bdpos_notthreat_2004
    bdpos_notthreat_2020 <- table(dft[[var2004]][which(dft$Bd2004 == 1)])[1]/(table(dft[[var2004]][which(dft$Bd2004 == 1)])[1]+table(dft[[var2004]][which(dft$Bd2004 == 1)])[2])
    bdpos_notthreat_2020

    # store results
    sink(paste0(path_out, "descriptive_stats.txt"), append=TRUE)
        cat(
            "\n\n\nRESPONSE\n\n",
            "Number of species tested for Bd with non-NA response between 1980 and 2004:",
            # Note response has already been set to NA if Bd not tested for between 1980 and 2004
            length(which(!is.na(dft[[var2004]]))),
            "\n\n",
            "Number of species tested for Bd with non-NA response between 2004 and 2020:",
            # Note response has already been set to NA if Bd not tested for between 2004 and 2020
            length(which(!is.na(dft[[var2020]]))),
            "\n\n",
            "Proportion of tested species that were threatened in 2004:",
            tested_resp_2004,
            "\n\n",
            "Proportion of tested species that were threatened in 2020:",
            tested_resp_2020,
            "\n\n",
            "Breakdown of all threatened and non-threatened species (by time period):\n\n"
        )
        print(chitab_resp)
        cat(
            "\n\n",
            "Test for difference between proportions of threatened species tested for Bd across time periods:\n"
        )
        print(chisq.test(chitab_resp))
        cat(
            "\n",
            "Effect size (Phi):",
            calculate_phi(chitab_resp),
            "\n\n",
            "Proportion of not threatened species that were Bd+ in 2004:",
            bdpos_notthreat_2004,
            "\n\n",
            "Proportion of not threatened species that were Bd+ in 2020:",
            bdpos_notthreat_2020
        )
    sink()
}


############################# Category change ##################################


if (resp %in% c("StatusChangeRL1980to2004", "StatusChangeRL2004to2020")) {


    ################################ Set up ####################################


    # dataset for subsetting
    dfs <- df

    dfs$StatusChangeRL1980to2004[which(dfs$StatusChangeRL1980to2004 %in% to_remover)] <- NA
    dfs$StatusChangeRL2004to2020[which(dfs$StatusChangeRL2004to2020 %in% to_remover)] <- NA
    

    ############################### Analysis ###################################


    # view breakdowns of category changes in each time period
    table(dfs$StatusChangeRL2004to2020)
    table(dfs$StatusChangeRL1980to2004)

    # create contingency table for category change in 2004 vs bd status in 2004
    tab_bd_status_2004_statx_2004 <- table(dfs$StatusChangeRL1980to2004, dfs$Bd2004)
    sum(table(dfs$Bd2004))
    sum(table(dfs$StatusChangeRL1980to2004))
    sum(table(dfs$StatusChangeRL1980to2004, dfs$Bd2004))
    colnames(tab_bd_status_2004_statx_2004) <- c("NoBd", "Bd")
    rownames(tab_bd_status_2004_statx_2004) <- c("NoChange", "Change")
    tab_bd_status_2004_statx_2004

    # create contingency table for category change in 2020 vs bd status in 2020
    tab_bd_status_2004_statx_2020 <- table(dfs$StatusChangeRL2004to2020, dfs$Bd2004)
    sum(table(dfs$Bd2004))
    sum(table(dfs$StatusChangeRL2004to2020))
    sum(table(dfs$StatusChangeRL2004to2020, dfs$Bd2004))
    colnames(tab_bd_status_2004_statx_2020) <- c("NoBd", "Bd")
    rownames(tab_bd_status_2004_statx_2020) <- c("NoChange", "Change")
    tab_bd_status_2004_statx_2020

    # create contingency table for category change on 2020 vs bd status in 2004
    tab_bd_status_2020_statx_2020 <- table(dfs$StatusChangeRL2004to2020, dfs$Bd2020)
    sum(table(dfs$Bd2020))
    sum(table(dfs$StatusChangeRL2004to2020))
    sum(table(dfs$StatusChangeRL2004to2020, dfs$Bd2020))
    colnames(tab_bd_status_2020_statx_2020) <- c("NoBd", "Bd")
    rownames(tab_bd_status_2020_statx_2020) <- c("NoChange", "Change")
    tab_bd_status_2020_statx_2020

    # store results
    sink(paste0(path_out, "descriptive_stats.txt"), append=TRUE)
        cat(
            "\n\nRESPONSE\n\n",
            sum(tab_bd_status_2004_statx_2004),
            "species were tested for Bd in 2004 and had a non-NA response between 1980 and 2004.\n\n",
            "Breakdown of 2004 Bd+ and Bd- by 1980-2004 response:\n"
        )
        print(tab_bd_status_2004_statx_2004)
        cat(
            "\n\n",
            sum(tab_bd_status_2004_statx_2020),
            "species were tested for Bd in 2004 and had a non-NA response between 2004 and 2020.\n\n",
            "Looking only species that tested Bd+ in 2004 tells us how already positive species have\n", 
            "responded to Bd over time. Species that have worsened further in category are likely still\n",
            "declining because of Bd, whereas species that were Bd positive in 2004 but didn't worsen in category\n",
            "until the 2004-2020 period are likely to be experiencing a delayed reaction or are not declining\n",
            "due to Bd.\n\n",
            "Breakdown of 2004 Bd+ and Bd- by 2004-2020 response:\n"
        )
        print(tab_bd_status_2004_statx_2020)
        cat(
            "\n\n",
            sum(tab_bd_status_2020_statx_2020),
            "species were tested for Bd in 2020 and had a non-NA response between 2004 and 2020.\n\n",
            "Breakdown of 2020 Bd+ and Bd- by 2004-2020 response:\n"
        )
        print(tab_bd_status_2020_statx_2020)
    sink()

    # calculate how many of the newly worsening species also worsened between 1980 and 2004
    # find species that worsened between 2004 and 2020
    sub_statx2004to2020 <- dfs[which(dfs$StatusChangeRL2004to2020 == 1),] 
    # note we could use Bd+ in 2020 here and it would make no difference since we end up
    # subsetting by only species that were Bd+ in 2004 anyway
    sub_statx2004to2020_bdpos2004 <- sub_statx2004to2020[which(sub_statx2004to2020$Bd2004 == 1),]
    nrow(sub_statx2004to2020_bdpos2004)
    # find species that worsened between 1980 and 2004
    sub_statx1980to2004 <- dfs[which(dfs$StatusChangeRL1980to2004 == 1),] 
    sub_statx1980to2004_bdpos2004 <- sub_statx1980to2004[which(sub_statx1980to2004$Bd2004 == 1),]
    nrow(sub_statx1980to2004_bdpos2004)
    # find overlap between the sets above to see which species have continued to worsen in category
    overlap <- intersect(sub_statx1980to2004_bdpos2004$Taxa, sub_statx2004to2020_bdpos2004$Taxa)
    length(overlap)

    # find species that are stable or improving in category
    recovering_or_stable_taxa <- setdiff(sub_statx1980to2004_bdpos2004$Taxa, sub_statx2004to2020_bdpos2004$Taxa)
    length(recovering_or_stable_taxa)
    recovering_or_stable <- dfs[which(dfs$Taxa %in% recovering_or_stable_taxa),]

    # subset by species that are recovering in terms of category change
    recovering <- data.frame()
    for (i in 1:nrow(recovering_or_stable)) {
        if (!is.na(recovering_or_stable$RL2020[i])) {
            if (recovering_or_stable$RL2004[i] != recovering_or_stable$RL2020[i]) {
                recovering <- rbind(recovering, recovering_or_stable[i,])
            }
        }
    }
    nrow(recovering)

    # subset by species that are stable in category
    stable <- data.frame()
    for (i in 1:nrow(recovering_or_stable)) {
        if (!is.na(recovering_or_stable$RL2020[i])) {
            if (recovering_or_stable$RL2004[i] == recovering_or_stable$RL2020[i]) {
                stable <- rbind(stable, recovering_or_stable[i,])
            }
        }
    }
    nrow(stable)

    # see how many species went extinct across time periods
    extinctions <- length(which(is.na(recovering_or_stable$IUCNcat2020)))
    # see how many species became data deficient across time periods
    # i.e. were categorised in 2004 but changed to DD in 2020
    data_deficient <- length(which(recovering_or_stable$IUCNcat2020 == "DD"))
    # calculate how many species either worsened in extant category or went extinct
    worsened <- length(overlap)+extinctions

    # store results
    sink(paste0(path_out, "descriptive_stats.txt"), append=TRUE)
        cat(
            "\n\n",
            "Of the", 
            tab_bd_status_2004_statx_2004[4], 
            "species that were Bd+ and worsened in category between 1980 and 2004,\n",
            worsened,
            "of these worsened further between 2004 and 2020, including",
            extinctions,
            "EX/EW.\n\n",
            "So of the",
            tab_bd_status_2004_statx_2020[4],
            "species that were already Bd+ and worsened in category between 2004 and 2020,\n",
            tab_bd_status_2004_statx_2020[4] - worsened,
            "of these species are new declines, whereas",
            worsened,
            "worsened further in category.\n\n",
            "This means that",
            tab_bd_status_2004_statx_2004[4] - worsened,
            "species are either stable or improved category between 2004 and 2020.\n\n",
            "Of these",
            tab_bd_status_2004_statx_2004[4] - worsened,
            "species,",
            nrow(stable),
            "are stable and",
            nrow(recovering),
            "are recovering.\n\n",
            "There are also",
            data_deficient,
            "species that were declared DD in 2020 (but not in 2004).\n\n",
            "This means that, of the species that worsened in category between 1980 and 2004,\n",
            paste0(round(worsened/tab_bd_status_2004_statx_2004[4]*100), "%"),
            "worsened further in category or went extinct,",
            paste0(round(nrow(stable)/tab_bd_status_2004_statx_2004[4]*100), "%"),
            "appear stable, and",
            paste0(round(nrow(recovering)/tab_bd_status_2004_statx_2004[4]*100), "%"),
            "appear to be recovering, with",
            paste0(round(data_deficient/tab_bd_status_2004_statx_2004[4]*100), "%"),
            "species now data deficient."
        )
    sink()
}




# then decide whether to use sampling biases data or not for this bit (in paper)
# currently not using sampling biases dataset
