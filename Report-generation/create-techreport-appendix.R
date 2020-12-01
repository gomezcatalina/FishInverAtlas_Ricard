##
## script to generate a single .Rmd file to be included in the TechReport
## - this is an alternative to the way I was doing the assembly of the Maritimes Atlas, and is tailored to work with csasdown
## - highly influenced by Sean Anderson's approach for his groundfish synoptic report

## generate a single .Rmd file containing all the bits and pieces required to generate the Appendix of the Atlas, 
## i.e. where all the figures appear: plot-pages.Rmd
library(tidyverse)

in.df <- readr::read_csv("../species-list-for-report-APHIA.csv")
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
  #spp_file3 <- paste0("SS",x,"_Distribution-usingbiomass.pdf")
  #spp_file4 <- paste0("SS",x,"_BDcorrelations.pdf")
  
  #spp_file5 <- paste0("SS",x,"LengthFreq-NAFO.pdf")
  #spp_file6 <- paste0("SS",x,"Condition4X4VW.pdf")
  
  #spp_file7 <- paste0("SS",x,"DepthDist.pdf")
  #spp_file8 <- paste0("SS",x,"TemperatureDist.pdf")
  #spp_file9 <- paste0("SS",x,"SalinityDist.pdf")
  #spp_file10 <- paste0("SS",x,"DDHSslopes.pdf")
  
  latin_name <- taxo.final$scientificname[taxo.final$species.code == x]
  english_name <- taxo.final$comm.english[taxo.final$species.code == x]
  french_name <- taxo.final$comm.fr[taxo.final$species.code == x]
  worms_id <- taxo.final$AphiaID[taxo.final$species.code == x]
  worms_link <- taxo.final$url[taxo.final$species.code == x]
  
  i <- 1
  
  out[[i]] <- paste0("## ", english_name, " (", french_name, ") - species code ", x," {#sec:", x, "} \n")
  i <- i + 1
  
    out[[i]] <- paste0(
    "Scientific name: [", latin_name, "](",worms_link,") \n \\newline")
  i <- i + 1
  #Figure 1
  out[[i]] <- "\\begin{minipage}{1.0\\textwidth}"
  i <- i + 1
  out[[i]] <- " \\begin{tabular}{c}"
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=6in]{../Figures-Actual/",
                     spp_file1, "} \\\\ ")
  i <- i + 1
  out[[i]] <- "\\end{tabular} "
  i <- i + 1
  out[[i]] <- paste0("\\captionof{figure}{Inverse distance weighted distribution of catch biomass (kg/tow) for ", english_name,".}")
  i <- i + 1
  out[[i]] <- "\\end{minipage} \n"
  i <- i + 1
  #end of Figure 1
  
  #Figure 2,3 and 4
  out[[i]] <- "\\begin{minipage}{1.0\\textwidth}"
  i <- i + 1
  out[[i]] <- " \\begin{tabular}{c}"
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=1.8in]{../Figures-Actual/",
                     spp_file2, "} \\\\ ")
  i <- i + 1
  out[[i]] <- "\\end{tabular} "
  i <- i + 1
  out[[i]] <- paste0("\\captionof{figure}{Inverse distance weighted distribution of catch biomass (kg/tow) for ", english_name,".}")
  i <- i + 1
  out[[i]] <- "\\end{minipage} \n"
  i <- i + 1
  #end of Figure 2, 3 and 4
  
  #End of Figures:
  out[[i]] <- "\\clearpage\n"
  out
})

temp <- lapply(temp, function(x) paste(x, collapse = "\n"))
temp <- paste(temp, collapse = "\n")
temp <- c("# Appendix\n<!-- This page has been automatically generated: do not edit by hand -->\n", temp)
if (!exists("N"))
  #writeLines(temp, con = file.path("report-EN", "plot-pages.Rmd"), useBytes=T)
  writeLines(temp, "plot-pages.Rmd", useBytes=T)
## French Tech Report



########################################################################################################