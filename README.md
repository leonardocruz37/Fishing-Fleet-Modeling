# Fishing-fleet-modeling
Code associated with the article ***'Fisheries track the future redistribution of marine species'***

The processed Global Fishing Watch data and EEZ boundaries shapefile are available at: doi.org/10.6084/m9.figshare.25112171

The environmental layers need to be downloaded from Bio-ORACLE (www.bio-oracle.org/downloads-to-email.php) and added to its respective folder, no need to rename any file. Benthic layers is from maximum depth. For all layers, the 'mean' version was used. 

The code is in order of execution and  restarting the R session in every step is necessary to avoid packages conflicts.

## Folder Structure

Project Fishing Fleet Modeling

├── code (in GitHub)

├── data (in Figshare repository)

├── environmental variables (donwnload from Bio-ORACLE)

│ ├── present

│ ├── ssp1_2100

│ ├── ssp4_2100

├── figures

├── output

│ ├── countries

│ ├── fishing gears

│ ├── Global
