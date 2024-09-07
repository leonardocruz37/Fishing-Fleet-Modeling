# Fishing-fleet-modeling
Code and additional supplementary figures (Figs. S20-S117, diagnostic plots) associated with the article ***'Fisheries track the future redistribution of marine species'*** [![DOI](https://img.shields.io/badge/DOI-10.1038%2Fs41558--024--02127--7-blue)](https://doi.org/10.1038/s41558-024-02127-7). The code is in order of execution and  restarting the R session in every step is necessary to avoid packages conflicts.

All the code and data necessary to reproduce the results is available at: 

[![DOI](https://img.shields.io/badge/DOI-10.6084/m9.figshare.25907905-blue)](https://doi.org/10.6084/m9.figshare.25907905)

This compressed file includes:

1) The processed Global Fishing Watch data (2013-2020) | Source: https://globalfishingwatch.org/
2) Exclusive Economic Zones boundaries shapefile version 12 | Source: https://marineregions.org/
3) Environmental layers | Source: https://www.bio-oracle.org/
4) List of the 82 countries and their regions according to the World Bank
5) Models output
6) Packages used that are not in CRAN (cannot be download through R) - just copy them to the folder where the other packages are installed. rgdal and maptools for figures and embarcadero for modelling (https://github.com/cjcarlson/embarcadero)
