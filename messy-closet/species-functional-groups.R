## assigning functional groups to species
## 
from.ices.paper <- read.csv("species-functional-groups.csv",header=TRUE)

## list from the atlas
from.atlas <- read.csv("../Report/species-list-final.csv",header=FALSE)

functional.groups.df <- merge(from.ices.paper, from.atlas, by.x='scientific.name', by.y='V1',all.x=TRUE, all.y=FALSE)
names(functional.groups.df) <- c("scientific.name","functional.group","common.name","english.name","french.name","spec.num","n.records","class","order","family","cat")

oo <- order(functional.groups.df$functional.group)
groups.df <- functional.groups.df[oo,]

## from WORMS
install.packages("SSOAP", repos = "http://www.omegahat.org/R")
library(SSOAP)
w = processWSDL("http://www.marinespecies.org/aphia.php?p=soap&wsdl=1")
iface = genSOAPClientInterface(, w) 
AphiaID = iface@functions$getAphiaID("Solea solea",1,('http://www.marinespecies.org/aphia.php?p=soap'))
print(AphiaID)
##

## try with my species
# iface@functions$getAphiaID(groups.df$scientific.name[1],1,('http://www.marinespecies.org/aphia.php?p=soap')) # this works fine
# apply to all species
my.aphia.ids <- lapply(1:length(groups.df$scientific.name), function(i){iface@functions$getAphiaID(groups.df$scientific.name[i],1,('http://www.marinespecies.org/aphia.php?p=soap'))})
groups.df$aphia <- unlist(my.aphia.ids)

