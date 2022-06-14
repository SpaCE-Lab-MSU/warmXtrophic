# TITLE:          warmXtrophic goldenrod activity for K-12 classrooms
# AUTHORS:        Moriah Young, Kara Dobson
# COLLABORATORS:  Mark Hammond
# DATA INPUT:     Data imported as csv files from shared Google drive L0 folder
# DATA OUTPUT:    
# PROJECT:        warmXtrophic
# DATE:           June, 2022


# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)

# Read in data
#height <- read.csv("/Users/moriahyoung/Downloads/WarmX_goldenrod_activity__bamboo_stem_length.csv")
#density <- read.csv("/Users/moriahyoung/Downloads/WarmX_goldenrod_bamboo_stem_density_data.csv")
height <- read.csv("WarmX_goldenrod_activity__bamboo_stem_length.csv")
density <- read.csv("WarmX_goldenrod_bamboo_stem_density_data.csv")

# Clean data
height <- height[, -c(4,6)] # delete unnecessary columns
names(height)[4] <- "bamboo_height_cm" # rename column
names(height) <- tolower(names(height)) # column names to lower case

names(density) <- density[1,] #make the first row the column names
density <- density[-c(1),] # delete first row
density <- density[, -c(2)] # delete unnecessary column
names(density)[2] <- "bamboo_stem_count" # rename column

# Plot data

# Average length - warming vs. ambient

# Take average of plant height
avg_height <- height %>%
        group_by(treatment) %>%
        dplyr::summarize(avg_height = mean(bamboo_height_cm, na.rm = TRUE))

png("goldenrod_heights.png", units="in", width=6, height=5, res=300)
ggplot(avg_height, aes(x = treatment, y = avg_height, fill = treatment)) +
        geom_bar(stat = "identity") +
        labs(x = "Treatment", y = "Average height of goldenrod (cm)") +
        scale_fill_manual(values = c("#00aedb", "#d11141")) +
        scale_x_discrete(labels=c("Warmed" = "Warming",
                                  "Ambient" = "Ambient")) +
        theme_classic() +
        theme(legend.position = "none") +
        scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70), expand = c(0, 0))+
        coord_cartesian(ylim=c(0, 70))
dev.off()

# Average density - warming vs. ambient 

# Take average of number of plants (density)
density$bamboo_stem_count = as.numeric(as.character(density$bamboo_stem_count))
avg_density <- density %>%
        group_by(treatment) %>%
        dplyr::summarize(avg_density = mean(bamboo_stem_count, na.rm = TRUE))

png("goldenrod_density.png", units="in", width=6, height=5, res=300)
ggplot(avg_density, aes(x = treatment, y = avg_density, fill = treatment)) +
        geom_bar(stat = "identity") +
        labs(x = "Treatment", y = "Average number of goldenrods") +
        scale_fill_manual(values = c("#00aedb", "#d11141")) +
        scale_x_discrete(labels=c("warmed" = "Warming",
                                  "ambient" = "Ambient")) +
        theme_classic() +
        theme(legend.position = "none") +
        scale_y_continuous(breaks = c(0,2,4,6,8,10,12,14), expand = c(0, 0))+
        coord_cartesian(ylim=c(0, 14))
dev.off()

