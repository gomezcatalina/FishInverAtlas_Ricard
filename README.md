

# Population Status & Important Habitat of Fish and Invertebrates in the Scotian Shelf Bioregion

Ricard and Shackell 2013 https://waves-vagues.dfo-mpo.gc.ca/Library/349264.pdf developed a reproducible report building from previous work and former atlases by 
1) providing a comprehensive suite of indices to assess population status and environmental preferences of 104 species and,
2) archiving code at DFO Maritimes Region. 
In 2020 Dan Ricard and Nancy Shackell shared code with Catalina Gomez to reproduce its outputs. 

This repo will update code assembled in Ricard and Shackell (2013) using new tools (e.g. mar.wrangling, csasdown) and developments.  

## Organisation of this repository
This git repository is made to be the base folder from which to run analyses related to the Maritimes Atlas.

The file "Atlas-main.R" is the top-level R script that contains the different data extractions, analyses and figure-making components of the Atlas.

The file "data-and-stats.R" contains the code developed to perform data extractions and statistical computations. Its main task is to populate the folder "Figures-Data" with data files that are to be used to generate figures.

The file "figures.R" contains the code developed to handle the generation of figures. The code associated with each figure is found in the folder "Figures-Rcode". The script uses data files from the "Figures-Data" folder and creates figures in the "Figures-Actual" folder.

The folder "Mapping" contains R scripts to generate maps that won't change over time (e.g. annotated strata map).

The folder "Report-generation" is the link between data extractions/analyses/figures and the generation of a Tech Report using csasdown.

The folder "report-EN" contains the csasdown folder tree for a Tech Report.
