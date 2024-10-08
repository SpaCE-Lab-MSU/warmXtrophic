[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.13362670.svg)](https://doi.org/10.5281/zenodo.13362670)

This project is licensed under the terms of the Creative Commons Attribution 4.0 license, CC-BY-4.0.

Open research: The data from this study are openly available in the Environmental Data Initiative data repository at https://doi.org/10.6073/pasta/a09100ccd11abc0fb43c3bdb27db93b6. 

Zarnetske, P., K.C. Dobson, M. Hammond, and M.L. Young. 2024. warmXtrophic: plant community responses to the individual and interactive effects of climate warming and herbivory across multiple years at Kellogg Biological Station Long-Term Ecological Research Sites (KBS LTER), Michigan, USA, and University of Michigan Biological Station (UMBS), Michigan, USA. ver 2. Environmental Data Initiative. https://doi.org/10.6073/pasta/ec1c1534994883f44e529610e9638305.

# warmXtrophic

## Introduction

This repository contains R scripts that organize, clean, analyze, and plot data from the long-term Warming X Trophic Interactions experiment at Kellogg Biological Station (KBS) and University of Michigan Biological Station (UMBS). A manuscript is currently in review summarizing the data from 2015-2021, co-led by Kara Dobson and Moriah Young, coauthors: Phoebe Zarnetske, Mark Hammond.

Young, M. L., Dobson, K. D., Hammond, M. D., Zarnetske, P. L. In review. Plant community responses to the individual and interactive effects of warming and herbivory across multiple years. Ecology. *Moriah and Kara are joint first authors

### Response variables include:

Phenology: greenup, flowering, flowering duration, seed set

Community composition: percent cover, diversity metrics (Shannon index, Simpson index, species richness)

Herbivory: probability of a leaf being eaten & the amount of leaf area eaten

Plant biomass: harvested at the end of the 2021 growing season

Leaf traits: Specific leaf area (SLA), N content, C content

## Workflow

L1 data must be downloaded prior to running the R script associated with that response variable. Each response variable (e.g., biomass, green-up, etc.) typically has its own R script for cleaning and analysis. Some traits are grouped for L2 figure making scripts (e.g., leaf traits are SLA, C, and N). For some L2 scripts, the L1 data must first be ran through its associated L1 script to create the data necessary for L2.

![EDI Workflow Image](docs/EDI_harmonization_procedure_general_updated.png)
Workflow image credit: https://edirepository.org/resources/designing-a-data-package

## Location of data

The L1 data that are processed in the scripts in this repository are published as an EDI package (https://doi.org/10.6073/pasta/ec1c1534994883f44e529610e9638305). In some scripts, the header may state that the data was input from Google Drive, which is an artifact of our data processing steps.

## Spatiotemporal extent

- Spatial extent: Two experimental sites in Hickory Corners (Kellogg Biological Station) and Pellston (University of Michigan Biological Station), MI, USA.
- Temporal extent: 2015-2021
    - 2015-2017: warming (via open top chambers), herbivore reduction (via insecticide, small mammal exclosures)
    - 2018-2021: warming (via open top chambers), herbivore reduction (via insecticide). In 2018 we removed the small mammal exclosures as they did not affect any response variables.
        - The total number of replicates for each treatment is 6 at each site, excluding the small mammal treatment.

## Usage

All analyses were conducted using R (R Core Team 2020)

### File Naming Conventions

- Data Files: File names typically contain the response variable, site, and data stage designation. For example, "UMBS_pairedsensors_L1.csv" would represent data from UMBS for the air temperature paired sensors as a clean, L1 version. Some data files may not have a site name (e.g., "final_flwr_sd_L1.csv"), meaning both KBS and UMBS are contained within that file. More meta-data file information is available in our EDI data repository.
- Scripts: Scripts are separated into two folders: L1 and L2. L1 files take raw data and clean it, thus resulting in our clean L1 data files available in EDI. The L2 files take the cleaned data and analyze/plot it. Scripts are named with their respective response variable and if the data is being analyzed or plotted (e.g., "herbivory_analyses_L2.R and herbivory_plots_L2.R").

## Scripts

### L1

The L1 scripts take raw data files and clean them into L1 data versions. The clean L1 data is already available on our EDI repository. We do not currently have raw, L0 data available in our EDI repository, but it may be published in a future version of the repo.

Scripts with "functions" in the file name (e.g., "biomass_functions.R") contain functions used in that response variables' cleaning script. Cleaning scripts are designated with "clean" in the file name (e.g., "biomass_clean_L1.R"). The "HOBO_pairedsensor_cleanup" and "HOBO_pendantdata_cleanup" scripts take the raw HOBO data and clean it, and those scripts are followed by the "HOBO_pairedsensor_merge" and "HOBO_pendantdata_merge" scripts, which then merge together all clean HOBO data.

A few L1 scripts take pre-cleaned L1 data and do a second transformation. These include the HOBO_data_removal.R script and the HOBO_GDD_L1.R script.

HOBO_data_removal.R: Takes the clean, L1 data files from the paired sensors cleaning scripts (e.g., "UMBS_pairedsensors_L1.csv") and remove outliers/inaccurate data. The output of this script makes a version of L1 data with some data removed (e.g., "UMBS_pairedsensors_dataremoved_L1.csv").

HOBO_GDD_L1.R: uses the clean, L1 data from the HOBO_data_removal.R script and calculates cumulative GDD and median, mean, and max temperatures for given timeframes. The HOBO_data_removal.R script must be ran prior to running this script.

### L2

Prior to running the analyses or plots for the response variables in the L2 scripts, a few scripts must first be ran:

phenology_dates_L2.R: This script takes the cleaned plant composition and phenology data and calculates the plot-level and species-level averages for green-up, flowering, flowering duration, and seed set. It outputs separate csv files for each variable, which are needed for further scripts on analyses and plotting.

plant_comp_data_wrangling_L2.R: This script takes the cleaned plant composition data and calculates the plot-level and species-level averages plant composition data, including for growth forms (forb and graminoid) and plant origin (native and exotic). It outputs separate csv files for each variable, which are needed for further scripts on analyses and plotting.

plant_comp_diversity_calculations_L2.R: This script takes the cleaned plant composition data and calculates species diversity metrics, including Shannon index, Simpson index, and species richness. It outputs a separate csv containing these calculated variables, which are needed for further scripts on analyses and plotting.

All other L2 files are labeled with either "analyses" or "plots" in the file name (e.g., "herbivory_analyses_L2.R and herbivory_plots_L2.R"). Also included in the file name is the response variable that is being analyzed or plotted.

## Contributors

PI: Phoebe Zarnetske

Collaborators: Mark Hammond, Moriah Young, Kara Dobson, Emily Parker

Prior collaborators/technicians: Nina Lany, Kileigh Welshofer, Kathryn Schmidt, Amy Wrobleski

## Contact

For inquiries related to the data and scripts, please contact Phoebe Zarnetske: @plz@msu.edu

### Funding

Moriah Young was supported by the National Science Foundation (NSF) Graduate Research Fellowship Program (DGE: 184-8739). Kara Dobson was supported by the Michigan State College of Natural Science and the NRT-IMPACTS program through NSF (DGE: 1828149). Support for this research was also provided by the NSF LTER Program (DEB: 2224712) at the Kellogg Biological Station and Michigan State University AgBioResearch.

