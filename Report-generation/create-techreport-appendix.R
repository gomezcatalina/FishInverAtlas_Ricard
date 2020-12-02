##
## script to generate a single .Rmd file to be included in the TechReport
## - this is an alternative to the way I was doing the assembly of the Maritimes Atlas, and is tailored to work with csasdown
## - highly influenced by Sean Anderson's approach for his groundfish synoptic report

## generate a single .Rmd file containing all the bits and pieces required to generate the Appendix of the Atlas, 
## i.e. where all the figures appear: plot-pages.Rmd
library(tidyverse)

in.df <- readr::read_csv(file.path(here::here(), "species-list-for-report-APHIA.csv"))
taxo.final <- in.df
names(taxo.final)[1:3] <- c("species.code","comm.english","comm.fr")

## first class tickets
taxo.final <- taxo.final[taxo.final$type %in% c("L"),]
## 

temp <- lapply(taxo.final$species.code, function(x) {
  
  out <- list()
  ## figure files 
  spp_file1 <- paste0("SS",x,"_IDWMap-biomass.pdf")
  
  spp_file2 <- paste0("SS",x,"_Stratified-biomass.pdf")
  spp_file3 <- paste0("SS",x,"_Distribution-usingbiomass.pdf")
  spp_file4 <- paste0("SS",x,"_BDcorrelations.pdf")
  
  spp_file5 <- paste0("SS",x,"_LengthFreq-NAFO.pdf")
  
  spp_file6 <- paste0("SS",x,"_Condition4X4VW.pdf")
  
  spp_file7 <- paste0("SS",x,"_DepthDist.pdf")
  spp_file8 <- paste0("SS",x,"_TemperatureDist.pdf")
  spp_file9 <- paste0("SS",x,"_SalinityDist.pdf")
  
  spp_file10 <- paste0("SS",x,"_DDHSslopes.pdf")
  
  latin_name <- taxo.final$scientificname[taxo.final$species.code == x]
  english_name <- taxo.final$comm.english[taxo.final$species.code == x]
  french_name <- taxo.final$comm.fr[taxo.final$species.code == x]
  family_name <- taxo.final$family[taxo.final$species.code == x]
  worms_id <- taxo.final$AphiaID[taxo.final$species.code == x]
  worms_link <- taxo.final$url[taxo.final$species.code == x]
  
  i <- 1
  out[[i]] <- paste0("## ", english_name, " (", french_name, ") - species code ", x," {#sec:", x, "} \n")
  i <- 1
  out[[i]] <- paste0("\\index{", french_name, "} ",  "\\index{", english_name, "} ",  "\\index{", latin_name, "} \n")
  i <- i + 1
  out[[i]] <- paste0("\\index{", family_name, "!", latin_name, "} \n")
  i <- i + 1
  out[[i]] <- paste0(
    "Scientific name: [", latin_name, "](",worms_link,") \n \\newline")
  i <- i + 1
  #Figure 1
  out[[i]] <- "\\begin{minipage}{1.0\\textwidth}"
  i <- i + 1
  out[[i]] <- " \\begin{tabular}{c}"
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=5.5in]{../Figures-Actual/",
                     spp_file1, "} \\\\ ")
  i <- i + 1
  out[[i]] <- "\\end{tabular} "
  i <- i + 1
  out[[i]] <- paste0("\\captionof{figure}{Inverse distance weighted distribution of catch biomass (kg/tow) for ", english_name,".}")
  i <- i + 1
  out[[i]] <- "\\end{minipage} \n"
  i <- i + 1
  #end of Figure 1
  
  #Figures 2,3 and 4
  out[[i]] <- "\\begin{minipage}{1.0\\textwidth}"
  i <- i + 1
  out[[i]] <- " \\begin{tabular}{ccc}"
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=1.8in]{../Figures-Actual/",
                      spp_file2, "} & ")
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=1.8in]{../Figures-Actual/",
                     spp_file3, "} & ")
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=1.8in]{../Figures-Actual/",
                     spp_file4, "} \\\\ ")
  i <- i + 1
  out[[i]] <- "\\end{tabular} "
  i <- i + 1
  out[[i]] <- paste0("\\captionof{figure}{Stratified random estimates of biomass (kg/tow), D75 and D95 and the correlation between D75 and
biomass of ", english_name,".}")
  i <- i + 1
  out[[i]] <- "\\end{minipage} \n"
  i <- i + 1
  #end of Figure 2, 3 and 4
  
  #Figures 7,8 and 9
  out[[i]] <- "\\begin{minipage}{1.0\\textwidth}"
  i <- i + 1
  out[[i]] <- " \\begin{tabular}{ccc}"
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=1.8in]{../Figures-Actual/",
                     spp_file7, "} & ")
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=1.8in]{../Figures-Actual/",
                     spp_file8, "} & ")
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=1.8in]{../Figures-Actual/",
                     spp_file9, "} \\\\ ")
  i <- i + 1
  out[[i]] <- "\\end{tabular} "
  i <- i + 1
  out[[i]] <- paste0("\\captionof{figure}{Catch distribution by depth, temperature and salinity of ", english_name,".}")
  i <- i + 1
  out[[i]] <- "\\end{minipage} \n"
  i <- i + 1
  #end of Figure 7,8 and 9
  
  #Figure 5 
  out[[i]] <- "\\begin{minipage}{1.0\\textwidth}"
  i <- i + 1
  out[[i]] <- " \\begin{tabular}{c}"
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=5in]{../Figures-Actual/",
                     spp_file5, "} \\\\ ")
  i <- i + 1
  out[[i]] <- "\\end{tabular} "
  i <- i + 1
  out[[i]] <- paste0("\\captionof{figure}{Length frequency distribution in NAFO units 4X and 4VW for ", english_name,".}")
  i <- i + 1
  out[[i]] <- "\\end{minipage} \n"
  i <- i + 1
  #end of Figure 5 
  out[[i]] <- "\n"
  #Figure 6 
  out[[i]] <- "\\begin{minipage}{1.0\\textwidth}"
  i <- i + 1
  out[[i]] <- " \\begin{tabular}{c}"
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=5in]{../Figures-Actual/",
                     spp_file6, "} \\\\ ")
  i <- i + 1
  out[[i]] <- "\\end{tabular} "
  i <- i + 1
  out[[i]] <- paste0("\\captionof{figure}{Average fish condition in NAFO units 4X and 4VW for ", english_name,".}")
  i <- i + 1
  out[[i]] <- "\\end{minipage} \n"
  i <- i + 1
  #end of Figure 6
  
  #Figure 10
  out[[i]] <- "\\begin{minipage}{1.0\\textwidth}"
  i <- i + 1
  out[[i]] <- " \\begin{tabular}{c}"
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=5in]{../Figures-Actual/",
                     spp_file10, "} \\\\ ")
  i <- i + 1
  out[[i]] <- "\\end{tabular} "
  i <- i + 1
  out[[i]] <- paste0("\\captionof{figure}{DDHS slopes versus median stratum abundance. The last two digits of each stratum number is shown in
the figure for ", english_name,".}")
  i <- i + 1
  out[[i]] <- "\\end{minipage} \n"
  i <- i + 1
  #end of Figure 10
  out[[i]] <- "\n"
  
  
  #End of Figures:
  out[[i]] <- "\\clearpage\n"
  out
})

temp <- lapply(temp, function(x) paste(x, collapse = "\n"))
temp <- paste(temp, collapse = "\n")
temp <- c("# Appendix\n<!-- This page has been automatically generated: do not edit by hand -->\n", temp, "\\printindex \n")
if (!exists("N"))
  #writeLines(temp, con = file.path("report-EN", "plot-pages.Rmd"), useBytes=T)
  writeLines(temp, "plot-pages.Rmd", useBytes=T)
## French Tech Report



########################################################################################################