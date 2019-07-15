# risksr_OSF
This repository contains data and analysis scripts for the project on *Mapping the Cognitive Processes Underlying Self-Reported Risk-Taking Propensity*

It contains the risksr_OSF R project and a collection of subfolders containing code ("cpp" with the C++ implementation of the value updating model and its fitting procedure; "r" with the data preparation and analysis scripts), data (containing anonymized data from the pilot study), and plots (generated with the r analysis scripts).

The R code files are numbered in the order in which they were executed (the data preparation scripts do not have to be run as the prepared data are already in the data folder).

To be able to run the simulation analyses, either first run the simulation code, or download the simulation_data.zip folder from the OSF repository, unpack it, and place it in the "data/simulation" folder of this repository.

For some analyses of Studies 1 and 2, either the files "data/study_X/contents_models.RData" are used. To do this, download the files from the OSF repository and place them in the appropriate folder, or run the respective code (see Codebook.pdf; the files are too large to push to OSF via GitHub which is why we have to use this procedure). 

The data files of the main studies are explained in the "Codebook.pdf".
