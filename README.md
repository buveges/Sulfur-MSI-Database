# Sulfur-MSI-Database
Github repo of sulfur isotope database with interactive figures and code associated with Uveges et al., 2023. (DOI to come)

The full database can be downloaded vie the "New_MIF_database.xlsx" file.

Interactive figures display the data from the database, and when clicking on a data point, will open up a new tab linking to a google scholar entry for the paper that sources that data point.


## Decription of code and other data associated with - Reconciling discrepant minor sulphur isotope records of the Great Oxidation Event
Authors:
Benjamin T. Uveges(a,1), Gareth Izon(a), Shuhei Ono(a), Nicolas J. Beukes(b), Roger E. Summons(a)

(a)-Department of Earth, Atmospheric and Planetary Sciences, Massachusetts Institute of Technology, 77 Massachusetts Avenue, Cambridge, MA 02139, USA.
(b)-DSI-NRF Centre of Excellence for Integrated Mineral and Energy Resource Analysis, Department of Geology, University of Johannesburg, P.O. Box 524, Auckland Park 2006, South Africa.
(1)To whom correspondence may be addressed: Benjamin T. Uveges – buveges (at) mit.edu


Description of the files used here:

Matlab files:

OSMV5.m – 
The base geochemical model employed by Luo et al. (2016) and modified herein. 
Luo, G. et al. Rapid oxygenation of Earth’s atmosphere 2.33 billion years ago. Sci. Adv. 2, e1600134 (2016).

Keff.m – 
a function utilized within the OSMV5 model

Sulfur_isotope_model_ForReview.m – 
Sulfur isotope model constructed for this study.

Boot_fun.m – 
Bootstrap sampling function utilized by the sulfur isotope model.

MIF_boot_sampl.csv – 
Time adjusted synthetic dataset that can be fed to isotope model.

Copy_of_MIF_boot_sample_cratons.csv – 
Craton adjusted synthetic dataset that can be fed to isotope model.

The output files listed below are passed to R for figure generation: 
Boot_out5.xlsx – 
weathered sulphur pool output of model using the TA database.

Boot_outCraton2.xlsx – 
weathered sulphur pool output of model using the CA database.

Boot_out_noweightSSA.xlsx – 
weathered sulphur pool output of model using the unadjusted SSA database.

Pool5.xlsx – 
Seawater sulphate pool output of model using the TA database.

pool_Craton2.xlsx –
Seawater sulphate pool output of model using the CA database

pool_noweightSSA.xlsx – 
Seawater sulphate pool output of model using the unadjusted SSA database.

Pilbara_out.xlsx – 
Pilbara Craton specific model output.


R Files:

NatComms_RcodeForReview.R – 
Tidied up compilation of the R code used in this study. Code is generally organized by the figure(s) that it generated.

New_MIF_database.xlsx – 
S-MIF database that was compiled for this study. No additions or analysis columns have been added, just the raw compiled data. 

Oxygen_history.png – 
Raster image that is used by R code to generate the full version of Fig. 1 in the main text.

Uveges TableSXv2.xlsx – 
Supplementary data file containing the new QSI data presented in this study.


How to run the attached code:

NOTE: There is no random "seed" set in any of the code here, meaning that for some analyses that employ randomized sampling or randomized methods, the outputs from run to run will vary slightly from those presented in the paper, and from each other, but not enough to change any conclusions drawn from the analyses.

Matlab:
For the Matlab model code, access to Matlab software is required, which is unfortunately not open source like R. All the functions and data files required to run the code are contained within the Matlab Code folder, so simply opening up the script and running it should work.

The OSMV5 model should be run first, followed by the sulphur isotope model simply by clicking the “run” button in Matlab. It should be noted, that because the Boot_fun function runs at each timestep of the isotope model, it will take a fair amount of time to run. Otherwise, the summary plots that pop up on completion display the relevant outputs.

R:
For the R code, an up-to-date version of R is required (Major version 4 minimum). The submitted code and analysis was run using minimum 4.1.1, within the 2022.07.1 Build 554 version of the RStudio IDE, but other IDEs should work as well.

To make review of this code easier, we have arranged the analyses into code blocks according to the figure(s) that they are associated with in the main text/supplement. For example, the cumulative mean ∆33S by publication date analysis code is located under the “Fig. 2” heading. Headings indicating a large collapsible code block are indicated by a single #, whereas smaller blocks are given subheadings of multiple # (ie. ##, ###, ####). Some comments are placed to the right of the specific code they describe.

Each analysis is run somewhat piece-meal. In other words, each block of code is run by itself by using command(control)+enter with the cursor clicked on the appropriate line. 
 
Within a given block, one can simply hold command/control and continue to hit enter to progress through each sub-block of the analysis.

The “Setup” block should be run first, but otherwise each block should be self-contained and can be run in any order. However, you will need to run each line of code, starting from the beginning of the block. We have tried to comment out the function/purpose of each operation to allow for easier review. We have made NOTES at code blocks that will take a fair bit of time to run.

There are several packages that will need to be installed and loaded to fully run all the code. All these packages are officially screened and published by CRAN and require very little storage space to install.
-	library(readxl) # For reading in excel files
-	library(tidyverse) # General R package family
-	library(patchwork) # For plotting combination figures
-	library(grid) # For plotting combination figures
-	library(MetBrewer) # Colour palette package
-	library(ggridges) # Specialized figure generation package
-	library(scales) # Used for scaling axes

