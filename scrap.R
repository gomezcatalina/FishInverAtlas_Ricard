## now order by phylogenetic classification
## order of classes
classes.order <- c("Myxini","Cephalaspidomorphi","Actinopterygii","Chondrichthyes","Cephalopoda","Malacostraca")
spec.xtable.df$taxoclass <- ordered(spec.xtable.df$CLASS_, levels=classes.order)

oo <- order(spec.xtable.df$taxoclass, spec.xtable.df$ORDER_, 
            spec.xtable.df$FAMILY_)
spec.xtable.df.final <- spec.xtable.df[oo,]

#spec.xtable.df.final <- na.omit(spec.xtable.df.final)

spec.xtable.df.final <- spec.xtable.df.final[spec.xtable.df.final$spec 
                                             %in% c(241,240,604,156,149,712,720,60,62,61,10,11,12,16,13,112,15,114,17,410,412,414,14,19,400,742,150,160,64,63,610,50,51,52,637,630,122,621,70,623,622,625,626,701,640,647,641,619,620,646,603,816,44,142,40,41,42,43,30,31,143,340,350,351,341,300,304,306,880,301,314,303,501,502,320,503,512,505,520,307,23,123,158,741,159,704,201,202,204,203,200,221,220,4512,4511,2511,2513,2532,2523,2550,2526,2527,2521,2211), ]

# write.table(spec.xtable.df.final[,c(1:9)],
#             file.path(path.Report, "species-list-final.csv"),
#             row.names=FALSE, col.names=FALSE, sep=",")

source(file.path(main.path, "messy-closet/write.unicode.csv.R")) 

# to allow french names to remain in the csv file i had to add the following lines of code as encoding="UTF-8" did not work

write.unicode.csv(spec.xtable.df.final[,c(1:9)],
                  file.path(main.path, "species-list-for-report.csv"))

<<<<<<< HEAD:summaries.R
AA <- read.csv(file.path(main.path,"species-list-for-report.csv"),
               header=TRUE)[ ,2:10]

write.table(AA[,c(1:9)],
            file.path(main.path, "species-list-for-report.csv"),
            row.names=FALSE, col.names=FALSE, sep=",")
=======
  # AA <- read.csv(file.path(path.Report,"species-list-final.csv"),
  #                        header=TRUE)[ ,2:10]
  
  # write.table(AA[,c(1:9)],
  #               file.path(path.Report, "species-list-final.csv"),
  #               row.names=FALSE, col.names=FALSE, sep=",")
  >>>>>>> 5b0445a96f4f29c098f1b4a0522c532ef09b29ab:FunctionsR/summaries.R


#fn.tex1 <- "Atlas_speciessummary_table1.tex"
#filename.tex1 <- file.path(path.Figures, fn.tex1)

#spec.xtable1 <- xtable(spec.xtable.df, digits=0)
#print.xtable(spec.xtable1, type='latex', file=filename.tex1, include.rownames=FALSE, size='scriptsize', booktabs=TRUE, tabular.environment="longtable", floating=FALSE)

## csv file 
#df.for.csv <- merge(df.summary, itis.all, by.x="spec", by.y="GIVEN_SPEC_CODE")
#df.for.csv$type <- ifelse(df.for.csv$nrecords <= 200 & df.for.csv$ORDER=='Decapoda', "SR", ifelse(df.for.csv$nrecords <= 200, "LR", ifelse(df.for.csv$ORDER=='Decapoda',"S","L") ))
#oo <- order(df.for.csv$nrecords, decreasing=TRUE)
#csv.df <- df.for.csv[oo,c(1,7,20,21,30,15,14)]

#write.csv(csv.df,file.path(path.R, "species-list.csv"), row.names=FALSE)

## make a better text file that will be used by the whole Atlas.
## what we are after is a single text file containing the following columns:
# Scientific name
# French name
# English name
# Species number
# Number of records
# Category

#spec.list <- read.csv(file.path(path.Report, "species-list.csv"),header=FALSE)

#spec.list <- csv.df

#spec.list.df <- merge(spec.list, spec.xtable.df, by="ACCEPTED_SCIENT_NAME", all.x=TRUE, all.y=FALSE)[,c(1,3,4,10,9,13,11,12)]
#names(spec.list.df) <- c("scientificname","englishname","frenchname","speciesnumber","numberofrecords","category","ordername","familyname")

#write.csv(spec.list.df,file.path(path.Report, "final-species-list.csv"), row.names=FALSE)








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

