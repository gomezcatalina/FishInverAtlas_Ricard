## script that handles a call for each species and generates the appropriate figures 
## 
## 
## description
## figure 1: a figure containing 9 maps of the survey area and showing the stratum-level stratified estimate of catch abundance
## figure 2: cumulative distributions showing habitat preference of a species (as per Perry and Smith (1994)), this figure shows the distributions for depth
## figure 3: cumulative distributions showing habitat preference of a species (as per Perry and Smith (1994)), this figure shows the distributions for bottom temperature
## figure 4: cumulative distributions showing habitat preference of a species (as per Perry and Smith (1994)), this figure shows the distributions for bottom salinity
## figure 5: time-series plots of distribution indices, includes the Gini index, D50, D75 and D95
## figure 6: time-series plots of stratified random estimates of catch abundance and catch biomass
## figure 7: length frequency distribution per 7-year period
## figure 8: length-weight relationship plot
## figure 9: timeseries of average fish condition
## figure 10: a figure containing 9 maps of the survey area and showing a geostatistical interpolation of catch abundance (inverse distance weighted)
## figure 11: same as figure 10, using catch biomass
## figure 12: density-dependent habitat selection plot, beta vs. stratum-level density
## figure 13: time-series plots of stratified random estimates of catch abundance (as per figure 6, but only abundance)
## figure 14: time-series plots of stratified random estimates of catch biomass (as per figure 6, but only biomass)
## figure 15: length frequency distribution per 7-year period, separately for NAFO 4X and NAFO 4VW (as per figure 7)
## figure 16: a single map showing the occurences of a species, used for rare species with few records 
## figure 17: correlation between abundance and distribution
## figure 18: time-series plots of distribution indices, includes the Gini index, D50, D75 and D95 (as per Figure 5, but using biomass)
## figure 19: correlation between biomass and distribution
## figure 20: timeseries of average fish condition, separately for NAFO 4X and NAFO 4VW (as per figure 9) and including the estimated parameters of the length-weight relationship
## figure 21: a figure containing 4 maps of the survey area and showing a geostatistical interpolation of catch abundance (inverse distance weighted), from 1999 for inverts 
## figure 22: same as figure 21, using catch biomass

figures<-function(fig=c(1:10), spec.num=NA,
		path.Data=NA, path.Map=NA,
		saveFormat="pdf", save=TRUE, 
		qual="low", power=10, 
    mature=T, pos.leg.fl="R", pos.leg.prof="R", pos.leg.ln="R", pos.ylabel=c(0,0)){

	##
##

	# load required libraries
	require(rgeos, quietly=TRUE, warn.conflicts = FALSE)
	require(PBSmapping, quietly=TRUE, warn.conflicts = FALSE)
	data(worldLLhigh)
	require(spatstat, quietly=TRUE, warn.conflicts = FALSE) 
	require(zoo, quietly=TRUE, warn.conflicts = FALSE) 	
	require(classInt, quietly=TRUE, warn.conflicts = FALSE) 
	require(RColorBrewer, quietly=TRUE, warn.conflicts = FALSE) 
	require(gstat, quietly=TRUE, warn.conflicts = FALSE) 
	require(maptools, quietly=TRUE, warn.conflicts = FALSE) 
	require(foreign, quietly=TRUE, warn.conflicts = FALSE) 
	require(lattice, quietly=TRUE, warn.conflicts = FALSE) 
	require(fields, quietly=TRUE, warn.conflicts = FALSE) 
	require(spam, quietly=TRUE, warn.conflicts = FALSE) 
	require(raster, quietly=TRUE, warn.conflicts = FALSE)
	require(rgdal, quietly=TRUE, warn.conflicts = FALSE)
	

	# base path
	path.Base1=path.ATLAS
	path.Base2=path.ATLAS
	# R functions path
	path.R=file.path(path.Base1, "FunctionsR")
	# data path
	path.Data=file.path(path.Base2,"Data")
	# mapping path
	path.Map=file.path(path.Base2, "Mapping")
	# path to store figures
	path.Figures=file.path(path.Base2, "Figure")	
	dir.create(path.Figures, showWarnings = FALSE)

	if(!file.exists(path.Base1)) stop(paste(path.Base1, "doesn't exist!\n"))
	if(!file.exists(path.Base2)) stop(paste(path.Base2, "doesn't exist!\n"))
	if(!file.exists(path.R)) stop(paste(path.R, "doesn't exist!\n"))
	if(!file.exists(path.Data)) stop(paste(path.Data, "doesn't exist!\n"))
	if(!file.exists(path.Map)) stop(paste(path.Map, "doesn't exist!\n"))

#--------------------------------------------------------------------------#
# source the R files associated with each figure
#--------------------------------------------------------------------------#
 
	source(file.path(path.R, "figure1.R"))
	source(file.path(path.R, "figure2.R"))
	source(file.path(path.R, "figure3.R"))
	source(file.path(path.R, "figure4.R"))
	source(file.path(path.R, "figure5.R"))
	source(file.path(path.R, "figure6.R"))
	source(file.path(path.R, "figure7.R"))
	source(file.path(path.R, "figure8.R"))
	source(file.path(path.R, "figure9.R"))
	source(file.path(path.R, "figure10.R"))
	source(file.path(path.R, "figure11.R")) 
	source(file.path(path.R, "figure12.R")) 
	source(file.path(path.R, "figure13.R")) 
	source(file.path(path.R, "figure14.R")) 
	source(file.path(path.R, "figure15.R")) 
	source(file.path(path.R, "figure16.R")) 
	source(file.path(path.R, "figure17.R")) 
	source(file.path(path.R, "figure18.R")) 
	source(file.path(path.R, "figure19.R")) 
	source(file.path(path.R, "figure20.R")) 
	source(file.path(path.R, "figure21.R")) 
	source(file.path(path.R, "figure22.R")) 
	source(file.path(path.R, "devOpen.R"))
	source(file.path(path.R, "devSave.R"))
	source(file.path(path.Map, "SUMMER-strata.R"))
	
#--------------------------------------------------------------------------#
# load all the necessary resources for mapping routines
#--------------------------------------------------------------------------#
		
#--------------------------------------------------------------------------#
# load the data associated with the species and start generating the figures
#--------------------------------------------------------------------------#
			
	# List the files that are in the Data directory
	fList = list.files(path.Data)
	# Find the files that are named based on the convention ALPHA*NUMERIC*_OTHER*.csv
	tmp = grep("[[:alpha:]]*[[:digit:]]['_'].*[.]csv$",fList)
	if(length(tmp)==0) stop("There are no appropriate data files in the Data directory\n")

	# Filter the list of files based on the file names that follow the convention
	fList=fList[tmp] 

	# Extract species number from the file names
	number = gsub("[[:alpha:]]*|['_'].*[.].*$", "", fList)
	number = as.numeric(levels(as.factor(number)))


	# if a species number is passed as an argument, use that number, otherwise process all the species that have data files
	if(!is.na(spec.num)) number=as.numeric(spec.num)
	
	# this is the sequential name of data files required by each figure, so there are some repetitions since some figures use the same data
	f.names =c("catch", "depthdist", "temperaturedist", "salinitydist", "distribution", "stratified", "lf", "lw", "lw", "catch", "catch", "DDHSslopes", "stratified", "stratified", "lf4x","catch","stratified","distribution-usingbiomass","stratified","lw4x","catch","catch")
	extension=".csv"
	
	for(j in number) {
		## load the data files
		paths=file.path(path.Data, paste("SS", j, "_", f.names, extension, sep=""))	
				for(i in fig){
			switch(i, 
				1 == { 	# tow-level catch data
					if(file.exists(paths[i])){
						# load the data from the csv file
						catch.dat <- read.csv(paths[i], header = TRUE)
						
						# name of graphics file
						devName <- "StrataMap"
						
						# open graphics device
						devOpen(devName, saveDir=path.Figures, fileName=paste("SS", j, "_", devName, sep=""), saveFormat=saveFormat, width=6, height=4.5)
						
						# graphics parameters
						par(mfrow=c(3,3), omi=c(0,0,0,0), mai=c(0,0,0,0), ps=12, cex=1, xpd=TRUE, lheight=0.9, xaxs="r", yaxs="r")
						
						# text parameters
						#cexF=list(title=1, axis=0.8,labels=0.9,legend=1, unit="in")
						cexF=list(title=1, axis=1.0,labels=1.0,legend=1, unit="in")

						
						# call figure function
						figure1.fct(dat.in=catch.dat, cex.in=cexF, pos.ylabel)
						
						# save the graphic device
						if(save==TRUE) devSave(devName)
						}
					},
				2 == { 	# depth distribution of catch
					if(file.exists(paths[i])){
						# load the data from the csv file
						depth.dist.dat <- read.csv(paths[i], header = TRUE)
						
						# name of graphics file
						devName <- "DepthDist"
						
						# open graphics device
						devOpen(devName, saveDir=path.Figures, fileName=paste("SS", j, "_", devName, sep=""), saveFormat=saveFormat, width=4, height=2+0.75)
						
						# graphics parameters
						#par(mfrow=c(1,1), omi=c(0,0,0,0), mai=c(0.35, 0.5+pos.ylabel[1], 0.1, 0.1), ps=8, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						par(mfrow=c(1,1), omi=c(0,0,0,0), mai=c(0.5, 0.75+pos.ylabel[1], 0.1, 0.1), ps=12, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						
						# text parameters
						#cexF=list(title=1, axis=0.8,labels=0.9,legend=1, unit="in")
						cexF=list(title=1, axis=1.0,labels=1.0,legend=1, unit="in")

						# call figure function
						figure2.fct(depth.dist.dat, cexF, pos.ylabel)
						
						# save the graphic device
						if(save==TRUE) devSave(devName)
						}
					},
				3 == { 	# temperature distribution of catch 
					if(file.exists(paths[i])){
						# load the data from the csv file
						temperature.dist.dat <- read.csv(paths[i], header = TRUE)
						# name of graphics file
						devName <- "TemperatureDist"
						
						# open graphics device
						devOpen(devName, saveDir=path.Figures, fileName=paste("SS", j, "_", devName, sep=""), saveFormat=saveFormat, width=4, height=2+0.75)
						
						# graphics parameters
						#par(mfrow=c(1,1), omi=c(0,0,0,0), mai=c(0.35, 0.5+pos.ylabel[1], 0.1, 0.1), ps=8, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						par(mfrow=c(1,1), omi=c(0,0,0,0), mai=c(0.5, 0.75+pos.ylabel[1], 0.1, 0.1), ps=12, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						
						# text parameters
						#cexF=list(title=1, axis=0.8,labels=0.9,legend=1, unit="in")
						cexF=list(title=1, axis=1.0,labels=1.0,legend=1, unit="in")

						# call figure function
						figure3.fct(temperature.dist.dat, cexF, pos.ylabel)
						
						# save the graphic device
						if(save==TRUE) devSave(devName)
						}
					},
				4 == { 	# salinity distribution of catch
					if(file.exists(paths[i])){
						# load the data from the csv file
						salinity.dist.dat <- read.csv(paths[i], header = TRUE)
						# name of graphics file
						devName <- "SalinityDist"
						
						# open graphics device
						devOpen(devName, saveDir=path.Figures, fileName=paste("SS", j, "_", devName, sep=""), saveFormat=saveFormat, width=4, height=2+0.75)
						
						# graphics parameters
						#par(mfrow=c(1,1), omi=c(0,0,0,0), mai=c(0.35, 0.5+pos.ylabel[1], 0.1, 0.1), ps=8, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						par(mfrow=c(1,1), omi=c(0,0,0,0), mai=c(0.5, 0.75+pos.ylabel[1], 0.1, 0.1), ps=12, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						
						# text parameters
						#cexF=list(title=1, axis=0.8,labels=0.9,legend=1, unit="in")
						cexF=list(title=1, axis=1.0,labels=1.0,legend=1, unit="in")
						
						# call figure function
						figure4.fct(salinity.dist.dat, cexF, pos.ylabel)
						
						# save the graphic device
						if(save==TRUE) devSave(devName)
						}
					},
				5 == { 	# distribution indices
					if(file.exists(paths[i])){
						# load the data from the csv file
						distribution.dat <- read.csv(paths[i], header = TRUE)
						
						# name of graphics file
						devName <- "Distribution"
						
						# open graphics device
						devOpen(devName, saveDir=path.Figures, fileName=paste("SS", j, "_", devName, sep=""), saveFormat=saveFormat, width=6, height=6)
						
						# graphics parameters
						#par(mfrow=c(1,3), omi=c(0,0,0,0), mai=c(0.35, 0.5+pos.ylabel[1], 0.1, 0.1), ps=8, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						#par(mfrow=c(1,1), omi=c(0,0,0,0), mai=c(0.35, 0.5+pos.ylabel[1], 0.1, 0.1), ps=12, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						par(mfrow=c(1,1), omi=c(0,0,0,0), mai=c(0.9, 1+pos.ylabel[1], 0.1, 1), ps=16, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")

						
						# text parameters
						#cexF=list(title=1, axis=0.8,labels=0.9,legend=1, unit="in")
						cexF=list(title=1, axis=1.0,labels=1.0,legend=1, unit="in")
						
						# call figure function
						#figure5.fct(distribution.dat, cexF, pos.ylabel, which.measure=c('areaocc','D','Gini'))
						figure5.fct(distribution.dat, cexF, pos.ylabel, which.measure=c('D'))
						# save the graphic device
						if(save==TRUE) devSave(devName)
						}
					},
				6 == { 	# stratified estimates
					if(file.exists(paths[i])){
						# load the data from the csv file
						stratified.dat <- read.csv(paths[i], header = TRUE)
						
						# name of graphics file
						devName <- "Stratified"
						
						# open graphics device
						devOpen(devName, saveDir=path.Figures, fileName=paste("SS", j, "_", devName, sep=""), saveFormat=saveFormat, width=12, height=4)
						
						# graphics parameters
						par(mfrow=c(1,2), omi=c(0,0,0,0), mai=c(0.35, 0.5+pos.ylabel[1], 0.1, 0.1), ps=12, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						
						# text parameters
						#cexF=list(title=1, axis=0.8,labels=0.9,legend=1, unit="in")
						cexF=list(title=1, axis=1.0,labels=1.0,legend=1, unit="in")
						
						# call figure function
						figure6.fct(stratified.dat, cexF, pos.ylabel)
						
						# save the graphic device
						if(save==TRUE) devSave(devName)
						}
					},
				7 == { 	# length frequencies
					if(file.exists(paths[i])){
						# load the data from the csv file
						lf.dat <- read.csv(paths[i], header = TRUE)
						
						# name of graphics file
						devName <- "LengthFreq"
						
						# open graphics device
						devOpen(devName, saveDir=path.Figures, fileName=paste("SS", j, "_", devName, sep=""), saveFormat=saveFormat, width=4, height=2+0.75)
						
						# graphics parameters
						# par(mfrow=c(1,1), omi=c(0,0,0,0), mai=c(0.35, 0.5+pos.ylabel[1], 0.1, 0.1), ps=12, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						par(mfrow=c(1,1), omi=c(0,0,0,0), mai=c(0.35, 0.7+pos.ylabel[1], 0.1, 0.1), ps=10, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						
						# text parameters
						#cexF=list(title=1, axis=0.8,labels=0.9,legend=1, unit="in")
						cexF=list(title=1, axis=1.0,labels=1.0,legend=1, unit="in")
						
						# call figure function
						figure7.fct(lf.dat, cexF, pos.ylabel)
						
						# save the graphic device
						if(save==TRUE) devSave(devName)
						}
					},
				8 == { 	# length-weight data
					if(file.exists(paths[i])){
						# load the data from the csv file
						lw.dat <- read.csv(paths[i], header = TRUE)

						# name of graphics file
						devName <- "LengthWeight"
						
						# open graphics device
						    #0.7+pos.ylabel: distance y axis to left margin. 
						devOpen(devName, saveDir=path.Figures, fileName=paste("SS", j, "_", devName, sep=""), saveFormat=saveFormat, width=4, height=2+0.75)
						
						# graphics parameters
						par(mfrow=c(1,1), omi=c(0,0,0,0), mai=c(0.35, 0.7+pos.ylabel[1], 0.1, 0.1), ps=10, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						
						# text parameters
						#cexF=list(title=1, axis=0.8,labels=0.9,legend=1, unit="in")
						cexF=list(title=1, axis=0.9,labels=0.9,legend=0.8, unit="in")
						
						# call figure function
						figure8.fct(lw.dat, cexF, pos.ylabel)
						
						# save the graphic device
						if(save==TRUE) devSave(devName)
						}
					},
				9 == { 	# fish condition
					if(file.exists(paths[i])){
						# load the data from the csv file
						lw.dat <- read.csv(paths[i], header = TRUE)

						# name of graphics file
						devName <- "Condition"
						
						# open graphics device
						devOpen(devName, saveDir=path.Figures, fileName=paste("SS", j, "_", devName, sep=""), saveFormat=saveFormat, width=4, height=2+0.75)
						
						# graphics parameters
						#par(mfrow=c(1,1), omi=c(0,0,0,0), mai=c(0.35, 0.5+pos.ylabel[1], 0.1, 0.1), ps=12, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						par(mfrow=c(1,1), omi=c(0,0,0,0), mai=c(0.75, 0.8+pos.ylabel[1], 0.1, 0.1), ps=16, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						
						# text parameters
						#cexF=list(title=1, axis=0.8,labels=0.9,legend=1, unit="in")
						cexF=list(title=1, axis=1.0,labels=1.0,legend=1, unit="in")
						
						# call figure function
						figure9.fct(lw.dat, cexF, pos.ylabel)
						
						# save the graphic device
						if(save==TRUE) devSave(devName)
						}
					},
				10 == { 	# IDW map, abundance
					if(file.exists(paths[i])){
						# load the data from the csv file
						catch.dat <- read.csv(paths[i], header = TRUE)

						# name of graphics file
						devName <- "IDWMap-abundance"
						
						# open graphics device
						devOpen(devName, saveDir=path.Figures, fileName=paste("SS", j, "_", devName, sep=""), saveFormat=saveFormat, width=11, height=8.5)
						
						# graphics parameters
						#par(mfrow=c(3,3), omi=c(0,0,0,0), mai=c(0.35, 0.5+pos.ylabel[1], 0.1, 0.1), ps=16, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						par(mfrow=c(3,3), omi=c(0,0,0,0), mai=c(0.35, 0.5+pos.ylabel[1], 0.1, 0.1), ps=12, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						
						# text parameters
						#cexF=list(title=1, axis=0.8,labels=0.9,legend=1, unit="in")
						cexF=list(title=1, axis=1.0,labels=1.0,legend=1, unit="in")
						
						# call figure function
						figure10.fct(catch.dat, cexF, pos.ylabel)
						
						# save the graphic device
						if(save==TRUE) devSave(devName)
						}
					},
				11 == { 	# IDW map, biomass
					if(file.exists(paths[i])){
						# load the data from the csv file
						catch.dat <- read.csv(paths[i], header = TRUE)

						# name of graphics file
						devName <- "IDWMap-biomass"
						
						# open graphics device
						devOpen(devName, saveDir=path.Figures, fileName=paste("SS", j, "_", devName, sep=""), saveFormat=saveFormat, width=11, height=8.5)
						
						# graphics parameters
						#par(mfrow=c(3,3), omi=c(0,0,0,0), mai=c(0.35, 0.5+pos.ylabel[1], 0.1, 0.1), ps=8, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						#par(mfrow=c(3,3), omi=c(0,0,0,0), mai=c(0.35, 0.5+pos.ylabel[1], 0.1, 0.1), ps=16, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						par(mfrow=c(3,3), omi=c(0,0,0,0), mai=c(0.35, 0.5+pos.ylabel[1], 0.1, 0.1), ps=12, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						
						# text parameters
						#cexF=list(title=1, axis=0.8,labels=0.9,legend=1, unit="in")
						cexF=list(title=1, axis=1.0,labels=1.0,legend=1, unit="in")
						
						# call figure function
						figure11.fct(catch.dat, cexF, pos.ylabel)
						
						# save the graphic device
						if(save==TRUE) devSave(devName)
						}
					},
				12 == { 	# DDHS map
					if(file.exists(paths[i])){
						# load the data from the csv file
						catch.dat <- read.csv(paths[i], header = TRUE)

						# name of graphics file
						devName <- "DDHSslopes"
						
						# open graphics device
						devOpen(devName, saveDir=path.Figures, fileName=paste("SS", j, "_", devName, sep=""), saveFormat=saveFormat, width=8, height=4)
						
						# graphics parameters
						#par(mfrow=c(1,1), omi=c(0,0,0,0), mai=c(0.35, 0.5+pos.ylabel[1], 0.1, 0.1), ps=8, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						#par(mfrow=c(1,1), omi=c(0,0,0,0), mai=c(0.5, 0.75+pos.ylabel[1], 0.1, 0.1), ps=12, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						par(mfrow=c(1,1), omi=c(0,0,0,0), mai=c(0.75, 1.2+pos.ylabel[1], 0.1, 0.1), ps=12, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						# text parameters
						#cexF=list(title=1, axis=0.8,labels=0.9,legend=1, unit="in")
						cexF=list(title=1, axis=1.0,labels=1.0,legend=1, unit="in")
						
						# call figure function
						figure12.fct(catch.dat, cexF, pos.ylabel, stratum.measure="mediantop25")
						
						# save the graphic device
						if(save==TRUE) devSave(devName)
						}
					},
				13 == { 	# stratified estimates
					if(file.exists(paths[i])){
						# load the data from the csv file
						stratified.dat <- read.csv(paths[i], header = TRUE)
						
						# name of graphics file
						devName <- "Stratified-abundance"
						
						# open graphics device
						devOpen(devName, saveDir=path.Figures, fileName=paste("SS", j, "_", devName, sep=""), saveFormat=saveFormat, width=6, height=6)
						
						# graphics parameters
						#par(mfrow=c(1,1), omi=c(0,0,0,0), mai=c(0.35, 0.5+pos.ylabel[1], 0.1, 0.1), ps=12, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						par(mfrow=c(1,1), omi=c(0,0,0,0), mai=c(0.9, 1+pos.ylabel[1], 0.1, 0.1), ps=16, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						
						# text parameters
						#cexF=list(title=1, axis=0.8,labels=0.9,legend=1, unit="in")
						cexF=list(title=1, axis=1.0,labels=1.0,legend=1, unit="in")
						
						# call figure function
						figure13.fct(stratified.dat, cexF, pos.ylabel)
						
						# save the graphic device
						if(save==TRUE) devSave(devName)
						}
					},
				14 == { 	# stratified estimates
					if(file.exists(paths[i])){
						# load the data from the csv file
						stratified.dat <- read.csv(paths[i], header = TRUE)
						
						# name of graphics file
						devName <- "Stratified-biomass"
						
						# open graphics device
						devOpen(devName, saveDir=path.Figures, fileName=paste("SS", j, "_", devName, sep=""), saveFormat=saveFormat, width=6, height=6)
						
						# graphics parameters
						#par(mfrow=c(1,1), omi=c(0,0,0,0), mai=c(0.35, 0.5+pos.ylabel[1], 0.1, 0.1), ps=8, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						#par(mfrow=c(1,1), omi=c(0,0,0,0), mai=c(0.35, 0.5+pos.ylabel[1], 0.1, 0.1), ps=12, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						par(mfrow=c(1,1), omi=c(0,0,0,0), mai=c(0.9, 1+pos.ylabel[1], 0.1, 0.1), ps=16, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						
						# text parameters
						#cexF=list(title=1, axis=0.8,labels=0.9,legend=1, unit="in")
						cexF=list(title=1, axis=1.0,labels=1.0,legend=1, unit="in")
						
						# call figure function
						figure14.fct(stratified.dat, cexF, pos.ylabel)
						
						# save the graphic device
						if(save==TRUE) devSave(devName)
						}
					},
				15 == { 	# length frequencies
					if(file.exists(paths[i])){
						# load the data from the csv file
						
						lf4x.dat <- read.csv(file.path(path.Data, paste("SS", j, "_", "lf4x", extension, sep="")), header = TRUE)
						lf4vw.dat <- read.csv(file.path(path.Data, paste("SS", j, "_", "lf4vw", extension, sep="")), header = TRUE)
						lfnafo.list <- list(nafo4x=lf4x.dat, nafo4vw=lf4vw.dat)
						
						# name of graphics file
						devName <- "LengthFreq-NAFO"
						
						# open graphics device
						devOpen(devName, saveDir=path.Figures, fileName=paste("SS", j, "_", devName, sep=""), saveFormat=saveFormat, width=12, height=6)
						
						# graphics parameters
						#par(mfrow=c(1,2), omi=c(0,0,0,0), mai=c(0.35, 0.5+pos.ylabel[1], 0.1, 0.2), ps=8, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						#par(mfrow=c(1,2), omi=c(0,0,0,0), mai=c(0.35, 0.5+pos.ylabel[1], 0.1, 0.2), ps=12, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						par(mfrow=c(1,2), omi=c(0,0,0,0), mai=c(0.75, 1.2+pos.ylabel[1], 0.1, 0.1), ps=16, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						
						# text parameters
						#cexF=list(title=1, axis=0.8,labels=0.9,legend=1, unit="in")
						cexF=list(title=1, axis=1.0,labels=1.0,legend=1, unit="in")
						
						# call figure function
						figure15.fct(lfnafo.list, cexF, pos.ylabel)
						
						# save the graphic device
						if(save==TRUE) devSave(devName)
						}
					},
			16 == { 	# single map, tow locations
					if(file.exists(paths[i])){
						# load the data from the csv file
						catch.dat <- read.csv(paths[i], header = TRUE)

						# name of graphics file
						devName <- "Map-tows"
						
						# open graphics device
						devOpen(devName, saveDir=path.Figures, fileName=paste("SS", j, "_", devName, sep=""), saveFormat=saveFormat, width=12, height=8)
						
						# graphics parameters
						par(mfrow=c(1,1), omi=c(0.5,0.2,0.5,0.2), mai=c(0.1, 0.1, 0.1, 0.1), ps=12, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						
						# text parameters
						#cexF=list(title=1, axis=0.8,labels=0.9,legend=1, unit="in")
						cexF=list(title=1, axis=1.0,labels=1.0,legend=1, unit="in")
						
						# call figure function
						figure16.fct(catch.dat, cexF, pos.ylabel)
						
						# save the graphic device
						if(save==TRUE) devSave(devName)
						}
					},	   		
			17 == { 	# distribution vs. abundance
					if(file.exists(paths[i])){
						# load the data from the csv file
						strat.dat <- read.csv(paths[i], header = TRUE)
						dist.dat <- read.csv(file.path(path.Data, paste("SS", j, "_distribution.csv", sep="")), header = TRUE)
						
						list.dat <- list(strat=strat.dat, dist=dist.dat)

						# name of graphics file
						devName <- "NDcorrelations"
						
						# open graphics device
						devOpen(devName, saveDir=path.Figures, fileName=paste("SS", j, "_", devName, sep=""), saveFormat=saveFormat, width=6, height=6)
						
						# graphics parameters
						#par(mfrow=c(1,3), omi=c(0,0,0,0), mai=c(0.35, 0.5+pos.ylabel[1], 0.1, 0.1), ps=8, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						#par(mfrow=c(1,1), omi=c(0,0,0,0), mai=c(0.35, 0.5+pos.ylabel[1], 0.1, 0.1), ps=12, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						#par(mfrow=c(1,1), omi=c(0,0,0,0), mai=c(0.35, 0.5+pos.ylabel[1], 0.1, 0.1), ps=12, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						par(mfrow=c(1,1), omi=c(0,0,0,0), mai=c(0.9, 1+pos.ylabel[1], 0.1, 0.1), ps=16, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						
						# text parameters
						#cexF=list(title=1, axis=0.8,labels=0.9,legend=1, unit="in")
						cexF=list(title=1, axis=1.0,labels=1.0,legend=1, unit="in")
						
						# call figure function
						#figure17.fct(list.dat, cexF, pos.ylabel, which.measure=c('areaocc','D','Gini'))
						figure17.fct(list.dat, cexF, pos.ylabel, which.measure=c('D'))
						
						# save the graphic device
						if(save==TRUE) devSave(devName)
						}
					},
				18 == { 	# distribution indices using biomass
					if(file.exists(paths[i])){
						# load the data from the csv file
						distribution.dat <- read.csv(paths[i], header = TRUE)
						
						# name of graphics file
						devName <- "Distribution-usingbiomass"
						
						# open graphics device
						devOpen(devName, saveDir=path.Figures, fileName=paste("SS", j, "_", devName, sep=""), saveFormat=saveFormat, width=6, height=6)
						
						# graphics parameters
						#par(mfrow=c(1,3), omi=c(0,0,0,0), mai=c(0.35, 0.5+pos.ylabel[1], 0.1, 0.1), ps=8, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						#par(mfrow=c(1,1), omi=c(0,0,0,0), mai=c(0.35, 0.5+pos.ylabel[1], 0.1, 0.1), ps=8, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						#par(mfrow=c(1,1), omi=c(0,0,0,0), mai=c(0.35, 0.5+pos.ylabel[1], 0.1, 0.1), ps=12, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						par(mfrow=c(1,1), omi=c(0,0,0,0), mai=c(0.9, 1+pos.ylabel[1], 0.1, 1), ps=16, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						
						# text parameters
						#cexF=list(title=1, axis=0.8,labels=0.9,legend=1, unit="in")
						cexF=list(title=1, axis=1.0,labels=1.0,legend=1, unit="in")
						
						# call figure function
						#figure18.fct(distribution.dat, cexF, pos.ylabel)
						figure18.fct(distribution.dat, cexF, pos.ylabel, which.measure=c('D'))
						# save the graphic device
						if(save==TRUE) devSave(devName)
						}
					},
			19 == { 	# distribution vs. biomass
					if(file.exists(paths[i])){
						# load the data from the csv file
						strat.dat <- read.csv(paths[i], header = TRUE)
						dist.dat <- read.csv(file.path(path.Data, paste("SS", j, "_distribution-usingbiomass.csv", sep="")), header = TRUE)
						
						list.dat <- list(strat=strat.dat, dist=dist.dat)

						# name of graphics file
						devName <- "BDcorrelations"
						
						# open graphics device
						devOpen(devName, saveDir=path.Figures, fileName=paste("SS", j, "_", devName, sep=""), saveFormat=saveFormat, width=6, height=6)
						
						# graphics parameters
						#par(mfrow=c(1,3), omi=c(0,0,0,0), mai=c(0.35, 0.5+pos.ylabel[1], 0.1, 0.1), ps=8, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						#par(mfrow=c(1,1), omi=c(0,0,0,0), mai=c(0.35, 0.5+pos.ylabel[1], 0.1, 0.1), ps=12, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						par(mfrow=c(1,1), omi=c(0,0,0,0), mai=c(0.9, 1+pos.ylabel[1], 0.1, 0.1), ps=16, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						
						# text parameters
						#cexF=list(title=1, axis=0.8,labels=0.9,legend=1, unit="in")
						cexF=list(title=1, axis=1.0,labels=1.0,legend=1, unit="in")
						
						# call figure function
						#figure17.fct(list.dat, cexF, pos.ylabel)
						figure19.fct(list.dat, cexF, pos.ylabel, which.measure=c('D'))
						# save the graphic device
						if(save==TRUE) devSave(devName)
						}
					},
				20 == { 	# fish condition
					if(file.exists(paths[i])){
						# load the data from the csv file
						lw.dat4x <- read.csv(paths[i], header = TRUE)
						lw.dat4vw <- read.csv(file.path(path.Data, paste("SS", j, "_lw4vw.csv", sep="")), header = TRUE)
						lw.list <- list(nafo4x=lw.dat4x, nafo4vw=lw.dat4vw)

						# name of graphics file
						devName <- "Condition4X4VW"
						
						# open graphics device
						devOpen(devName, saveDir=path.Figures, fileName=paste("SS", j, "_", devName, sep=""), saveFormat=saveFormat, width=12, height=6)
						
						# graphics parameters
						#par(mfrow=c(1,2), omi=c(0,0,0,0), mai=c(0.35, 0.5+pos.ylabel[1], 0.1, 0.1), ps=8, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						#par(mfrow=c(1,2), omi=c(0,0,0,0), mai=c(0.35, 0.5+pos.ylabel[1], 0.1, 0.1), ps=12, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						par(mfrow=c(1,2), omi=c(0,0,0,0), mai=c(0.9, 1.2+pos.ylabel[1], 0.1, 0.1), ps=16, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						
						# text parameters
						#cexF=list(title=1, axis=0.8,labels=0.9,legend=1, unit="in")
						cexF=list(title=1, axis=1.0,labels=1.0,legend=1, unit="in")
						
						# call figure function
						figure20.fct(lw.list, cexF, pos.ylabel)
						
						# save the graphic device
						if(save==TRUE) devSave(devName)
						}
					},
				21 == { 	# IDW map, abundance, for species recorded since 1999
					if(file.exists(paths[i])){
						# load the data from the csv file
						catch.dat <- read.csv(paths[i], header = TRUE)

						# name of graphics file
						devName <- "IDWMap-abundance"
						
						# open graphics device
						devOpen(devName, saveDir=path.Figures, fileName=paste("SS", j, "_", devName, sep=""), saveFormat=saveFormat, width=11, height=8.5)
						
						# graphics parameters
						par(mfrow=c(2,2), omi=c(0,0,0,0), mai=c(0.35, 0.5+pos.ylabel[1], 0.1, 0.1), ps=12, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						
						# text parameters
						#cexF=list(title=1, axis=0.8,labels=0.9,legend=1, unit="in")
						cexF=list(title=1, axis=1.0,labels=1.0,legend=1, unit="in")
						
						# call figure function
						figure21.fct(catch.dat, cexF, pos.ylabel)
						
						# save the graphic device
						if(save==TRUE) devSave(devName)
						}
					},
				22 == { 	# IDW map, biomass, for species recorded since 1999
					if(file.exists(paths[i])){
						# load the data from the csv file
						catch.dat <- read.csv(paths[i], header = TRUE)

						# name of graphics file
						devName <- "IDWMap-biomass"
						
						# open graphics device
						devOpen(devName, saveDir=path.Figures, fileName=paste("SS", j, "_", devName, sep=""), saveFormat=saveFormat, width=11, height=8.5)
						
						# graphics parameters
						par(mfrow=c(2,2), omi=c(0,0,0,0), mai=c(0.35, 0.5+pos.ylabel[1], 0.1, 0.1), ps=12, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
						
						# text parameters
						#cexF=list(title=1, axis=0.8,labels=0.9,legend=1, unit="in")
						cexF=list(title=1, axis=1.0,labels=1.0,legend=1, unit="in")
						
						# call figure function
						figure22.fct(catch.dat, cexF, pos.ylabel)
						
						# save the graphic device
						if(save==TRUE) devSave(devName)
						}
					},
					NULL == {
	   					stop("This figure number does not exist!\n")
	   		}
			) # end switch
		} # end of loop over figures
	}# end of loop over species


} # end function figures
	
