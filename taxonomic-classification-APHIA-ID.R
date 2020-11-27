
taxo.tree.fct <- function(aphia.id, spec.code){
  my.df <- wormsbyid(aphia.id)
  vars <- c("kingdom","phylum","class","order","family","scientificname")
  return(cbind(spec.code, my.df[,vars]))
}## end function


## read in the list of species which is the starting point for assembling the document
## start by making the table that contains the taxonomic classification, which will establish the order in which the different species will appear in the Atlas  -> now done in "create-techreport-appendix.R"

taxo.list.out <- lapply(1:nrow(species.L), function(ss){aa1<-species.in[ss,c("aphia.id")];aa2<-species.in[ss,c("species.code")];taxo.tree.fct(aa1,aa2)})
taxo.t <- do.call(rbind, taxo.list.out)
vars <- c("species.code","comm.english","comm.fr","aphia.id","taxo.group")
taxo.df.out <- merge(species.in[,vars], taxo.t, by.x="species.code", by.y="spec.code")

taxo.df.out$aphia.url <- paste("http://www.marinespecies.org/aphia.php?p=taxdetails&id=", taxo.df.out$aphia.id, sep="")

## order the taxonomic tree phylogenetically
##
taxo.final <- taxo.df.out[,c("phylum","class","order","family","scientificname","comm.english","comm.fr","species.code","aphia.id","aphia.url","taxo.group")]
taxo.final$phylum <- factor(taxo.final$phylum, levels=c("Chordata","Mollusca","Arthropoda","Echinodermata"), ordered=TRUE) ## this is used to order the table of species
taxo.final$class <- factor(taxo.final$class, levels=c("Ascidiacea","Myxini","Actinopterygii","Elasmobranchii","Cephalopoda","Malacostraca","Echinoidea","Asteroidea"), ordered=TRUE) ## this is used to order the table of species
oo1 <- order(taxo.final$phylum)
taxo.final <- taxo.final[oo1,]
oo2 <- order(taxo.final$class)
taxo.final <- taxo.final[oo2,]


## generate a table with list of species with associated taxonomic information -> now done in "create-techreport-appendix.R"
