##
## script to generate a single .Rmd file to be included in the TechReport
## - this is an alternative to the way I was doing the assembly of the Maritimes Atlas, and is tailored to work with csasdown
## - highly influenced by Sean Anderson's approach for his groundfish synoptic report
 
## generate a single .Rmd file containing all the bits and pieces required to generate the Appendix of the Atlas, 
## i.e. where all the figures appear: plot-pages.Rmd
library(tidyverse)

temp <- lapply(taxo.final$species.code, function(x) {

  out <- list()
  ## figure files 
  spp_file1 <- paste0("SS",x,"_IDWMap-biomass.pdf")
  
  latin_name <- taxo.final$scientificname[taxo.final$species.code == x]
  english_name <- taxo.final$comm.english[taxo.final$species.code == x]
  french_name <- taxo.final$comm.fr[taxo.final$species.code == x]
  worms_id <- taxo.final$aphia.id[taxo.final$species.code == x]
  worms_link <- taxo.final$aphia.url[taxo.final$species.code == x]

  i <- 1

    out[[i]] <- paste0("## ", english_name, " (", french_name, ") - species code ", x," {#sec:", x, "} \n")
  i <- i + 1
  out[[i]] <- paste0(

	"Scientific name: [", latin_name, "](",worms_link,") \n \\newline")
  i <- i + 1
  out[[i]] <- "\\begin{minipage}{0.9\\textwidth}"
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
  out[[i]] <- "\\end{tabular}"
  i <- i + 1
  out[[i]] <- "\\clearpage\n"
  out
})

temp <- lapply(temp, function(x) paste(x, collapse = "\n"))
temp <- paste(temp, collapse = "\n")
temp <- c("# Appendix\n<!-- This page has been automatically generated: do not edit by hand -->\n", temp)
if (!exists("N"))
  writeLines(temp, con = file.path("report-EN", "plot-pages.Rmd"), useBytes=T)
## French Tech Report



########################################################################################################
