##
## script to generate a single .Rmd file to be included in the TechReport
## - this is an alternative to the way I was doing the assembly of the Maritimes Atlas, and is tailored to work with csasdown
## - highly influenced by Sean Anderson's approach for his groundfish synoptic report
 
## generate a single .Rmd file containing all the bits and pieces required to generate the Appendix of the Atlas, 
## i.e. where all the figures appear: plot-pages.Rmd
library(tidyverse)

# First we need to create taxo.final using species-list-for-report.csv and info from Gulf-RV-Atlas-species-list.csv
# This step onle needs to be accomplished once when species-list-for-report.csv is updated
main.path <- here::here()
species.in <- read.csv(file.path(main.path, "species-list-for-report.csv"), stringsAsFactors=FALSE, encoding="UTF-8", quote = '"')
colnames(species.in) <- c("scientificname", "comm.english", "comm.fr", "species.code", "other", "class", "order", "family", "taxo.group")
species.in <- species.in %>% 
                        filter(taxo.group == "L") %>%
                        select(class, order, family, scientificname, species.code, taxo.group)
load("C:/RProjects/FishInverAtlas_Ricard/messy-closet/taxo-final-Gulf.Rdata") # loads taxo.final.Gulf that file was created D. Ricard using “taxonomic-classification-APHIA-ID.R”

taxo.final_Gulf <- taxo.final %>% 
                        select(phylum, class, order, family, scientificname, comm.english, comm.fr, aphia.id, aphia.url) %>%   
                        mutate(taxo.group="Gulf")

taxo.final <- species.in %>% left_join(taxo.final) %>% 
                         select(phylum, class, order, family, scientificname, comm.english, comm.fr, species.code, aphia.id, aphia.url) 

taxo.final$phylum <- factor(taxo.final$phylum, levels=c("Chordata","Mollusca","Arthropoda"), ordered=TRUE)
taxo.final$class <- factor(taxo.final$class, levels=c("Myxini","Actinopterygii","Elasmobranchii","Cephalopoda","Malacostraca"), ordered=TRUE)
oo1 <- order(taxo.final$phylum)
taxo.final <- taxo.final[oo1,]
oo2 <- order(taxo.final$class)
taxo.final <- taxo.final[oo2,]
getwd()
save(taxo.final, file = "taxo.final.RData")
load("taxo.final.RData")

temp <- lapply(taxo.final$species.code, function(x) {

  out <- list()
  ## figure files 
  spp_file1 <- paste0("SS",x,"IDWMap-biomass.pdf")
  
  spp_file2 <- paste0("SS",x,"Stratified-biomass.pdf")
  spp_file3 <- paste0("SS",x,"Distribution-usingbiomass.pdf")
  spp_file4 <- paste0("SS",x,"BDcorrelations.pdf")
  
  spp_file5 <- paste0("SS",x,"LengthFreq-NAFO.pdf")
  spp_file6 <- paste0("SS",x,"Condition4X4VW.pdf")
  
  spp_file7 <- paste0("SS",x,"DepthDist.pdf")
  spp_file8 <- paste0("SS",x,"TemperatureDist.pdf")
  spp_file9 <- paste0("SS",x,"SalinityDist.pdf")
  spp_file10 <- paste0("SS",x,"DDHSslopes.pdf")
  
  ##spp_file4 <- paste0("RV-4T-",x,"-RVkgpertowcutoff-to-",current.year,".pdf")

  latin_name <- taxo.final$scientificname[taxo.final$species.code == x]
  english_name <- taxo.final$comm.english[taxo.final$species.code == x]
  french_name <- taxo.final$comm.fr[taxo.final$species.code == x]
  worms_id <- taxo.final$aphia.id[taxo.final$species.code == x]
  worms_link <- taxo.final$aphia.url[taxo.final$species.code == x]

  i <- 1
#  out[[i]] <- "\\clearpage\n"
#  i <- i + 1
##  out[[i]] <- paste0("## ", english_name, " {#sec:", x, "}\n")
  out[[i]] <- paste0("## ", english_name, " (", french_name, ") - species code ", x," {#sec:", x, "} \n")
  i <- i + 1
  out[[i]] <- paste0(
#    "Order: ", taxo.final$order[taxo.final$species.code == x], " \n",
#    "Family: ", taxo.final$family[taxo.final$species.code == x], " \n",
	"Scientific name: [", latin_name, "](",worms_link,") \n \\newline")
  i <- i + 1
  out[[i]] <- "\\begin{minipage}{0.9\\textwidth}"
  i <- i + 1
  out[[i]] <- " \\begin{tabular}{ccc}"
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=1.8in]{../Figures-actual/",
                     spp_file6, "} & ")
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=1.8in]{../Figures-actual/",
                     spp_file2, "} & ")
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=1.8in]{../Figures-actual/",
                     spp_file3, "} \\\\")
  i <- i + 1
  out[[i]] <- "\\end{tabular} "
  i <- i + 1
  out[[i]] <- paste0("\\captionof{figure}{Catch biomass (kg/tow), distribution indices (DWAO, D75 and D95), and D75 as a function of catch biomass (kg/tow) for ", english_name, ".}")
  i <- i + 1
  out[[i]] <- "\\end{minipage} \n"
  i <- i + 1
  
     
    out[[i]] <- "\n"
  i <- i + 1

  out[[i]] <- "\\begin{tabular}{c}"
  i <- i + 1
  out[[i]] <- "\\begin{minipage}{1.0 \\textwidth}"
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=5.7in]{../Figures-actual/",
              spp_file1, "}\\\\")
  i <- i + 1
  out[[i]] <- paste0("\\captionof{figure}{Time series of log catch biomass (kg/tow) of ", english_name, " for each stratum.} ")
  i <- i + 1
  out[[i]] <- "\\end{minipage} \n"
  i <- i + 1
  out[[i]] <- "\\end{tabular}"
  i <- i + 1
  out[[i]] <- "\n"
  i <- i + 1
  
  out[[i]] <- "\\begin{tabular}{c}"
  i <- i + 1
  out[[i]] <- "\\begin{minipage}{1.0 \\textwidth}"
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=5.7in]{../Figures-actual/",
              spp_file4, "}\\\\")
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
  writeLines(temp, con = file.path("report-EN","plot-pages.Rmd"), useBytes=T)

## French Tech Report



########################################################################################################
## using the same data frame, generate a properly formatted table to present the different species
## taxonomic classes
cc <- unique(taxo.final$class)
## taxonomic orders for the current class
oo <- unique(taxo.final$order)
## taxonomic families
ff <- unique(taxo.final$family)

vars <- c("class","order","family","scientificname","comm.english","comm.fr","species.code","aphia.id")
oo <- order(taxo.final$class, taxo.final$order, taxo.final$family)
con<-file(file.path("report2020-EN","species-table2.csv"), encoding="UTF-8")
write.csv(taxo.final[oo,vars], con, row.names = FALSE)

con<-file(file.path("report2020-FR","species-table2.csv"), encoding="UTF-8")
write.csv(taxo.final[oo,vars], con, row.names = FALSE)



con <- file(file.path("report2020-EN","species-table.csv"), open = "wt", encoding = "UTF-8")
sink(con)
cat(c("Scientific name,","English name,","French name,","Species code,","AphiaID"))
cat("\n")

## triple nested loop
for(i in 1:length(cc)){ ## loop over taxonomic classes
  c.t <- as.character(cc[i])
  c.t2 <- paste(paste0("Class: ", c.t), paste(rep(NA,4), collapse=","), sep=",")
  cat(c.t2)
  cat("\n")
  this.oo <- unique(taxo.final[taxo.final$class==c.t,"order"])
  
  for(ii in 1:length(this.oo)){ ## loop over taxonomic orders
    o.t <- this.oo[ii]
    o.t2 <- paste(paste0("Order: ", o.t), paste(rep(NA,4), collapse=","), sep=",")
    cat(o.t2)
    cat("\n")
    
    this.ff <- unique(taxo.final[taxo.final$class==c.t & taxo.final$order==o.t,"family"])
    
    for(iii in 1:length(this.ff)){ ## loop over taxonomic families
      f.t <- this.ff[iii]
      f.t2 <- paste(paste0("Family: ", f.t), paste(rep(NA,4), collapse=","), sep=",")
      cat(f.t2)
      cat("\n")
      
      ss <- taxo.final[((taxo.final$class == c.t) & (taxo.final$order == o.t) & (taxo.final$family == f.t)),]
      
      for(iiii in 1:nrow(ss)){
        s.t <- ss[iiii,"scientificname"] 
        en.t <- ss[iiii,"comm.english"] 
        fr.t <- ss[iiii,"comm.fr"]
        code.t <- ss[iiii,"species.code"]
        # code.t <- paste0("[", code.t, "]", "(\\@ref(sec:",code.t,"))")
        aid.t <- ss[iiii,"aphia.id"]
        ali.t <- ss[iiii,"aphia.url"]
        aph.t <- aid.t # paste0("[", aid.t, "](", ali.t, ")")
        
        ll <- paste(s.t, en.t, fr.t, code.t, aph.t,  sep=",")
        cat(ll)
        cat("\n")
        
      }
      
      
    }## end loop over families
  }## end loop over orders
}## end loop over classes

cat("\n")
sink()
close(con)

