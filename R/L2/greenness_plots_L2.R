# TITLE:          warmXtrohpic - Greenness
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Moriah Young, Kristin Wolford, Mark Hammond, Pat Bills
# DATA INPUT:     Data imported as csv files from shared Google drive L0 folder
# DATA OUTPUT:    Plots
# PROJECT:        warmXtrophic
# DATE:           July, 2021

# Clear all existing data
rm(list=ls())

# Load packages
library(tidyverse)

# Set working directory
L1_dir<-Sys.getenv("L1DIR")

# Read in data
green <- read.csv(file.path(L1_dir, "Greenness/greenness_L1.csv"))

# Take plot average
green2 <- green %>%
        group_by(plot, state) %>%
        summarize(greenness = mean(greenness, na.rm = TRUE))

# Boxplot
png("warmx_green.png", units="in", width=6, height=5, res=300)
ggplot(green2, aes(x = state, y = greenness, fill=state)) +
        #geom_jitter(shape=16, position=position_jitterdodge(), alpha = 0.6, aes(colour = state)) +
        scale_color_manual(values = c("ambient" = "olivedrab", "warmed" = "olivedrab")) +
        geom_boxplot(color = "black") +
        labs(x = "Treatment", y = "Leaf Greenness", face="bold") +
        scale_fill_manual(values = c("olivedrab", "olivedrab")) +
        scale_x_discrete(labels=c("ambient" = "Ambient",
                                  "warmed" = "Warmed")) +
        theme_bw() +
        theme(text = element_text(size=16)) +
        theme(axis.title.x = element_text(size=16, face="bold", colour = "black"),    
        axis.title.y = element_text(size=16, face="bold", colour = "black")) +
        theme(legend.position = "none")
#ggsave("warmx_green.png", width = 6, height = 5, device='png', dpi=300)
dev.off()
