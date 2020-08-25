#--------------------------------------------------------------------------#
## main R script to generate the figures required in the DFO Maritimes groundfish survey atlas
## Daniel Ricard, started 2012-07-20 relying extensively on Hugo Bourdages' work on the NGSL atlas
## Catalina Gomez and Gordana Lazin updated scripts in July 2020 and updated extractions and data up to 2020
options(echo=FALSE)

print(paste("Script Atlas.R started: ", Sys.time()))

# required libraries, and install if necessary
  necessary <- c("PBSmapping","spatstat","zoo","classInt","RColorBrewer","gstat","maptools",
                  "foreign","fields","spam","rgeos", "RODBC", 
                 "xtable", "MASS", "xlsx", "raster", "rgdal",
                 "here")
  installed <- necessary %in% installed.packages()[, 'Package']
  if (length(necessary[!installed]) >=1) install.packages(necessary[!installed], repos='http://mirror.its.dal.ca/cran/')
  ## load required packages
  

	# lapply(necessary, function(i){citation(i)}) ## citations for the different packages used
  
  
  ## since we are likely working from a git repository, use the "here" library to set the paths relative to the root path of the git repo
  main.path <- here::here()
  
  report.path <- file.path(main.path, "Report-generation")
  
  figdata.path <- file.path(main.path, "Figures-data") ## where to store data for figures
  figcode.path <- file.path(main.path, "Figures-Rcode") ## where to find code for figures
  fig.path <- file.path(main.path, "Figures-actual") ## where to store figures
  mapping.path <- file.path(main.path, "Mapping")## mapping folder for maps that won't change over time
  

  source(file.path(main.path, "chan.R"))
  
  ## generate the list of species that will be used to control what data extractions and figures will be generated
  ## this script will produce a file called "species-list-for-report.csv" which will determine the species that we will include, and what level of analysis they will receive
  ## this takes a while, so once it has run successfully, comment out and rely on the file "species-list-for-report.csv"
  source(file.path(main.path, "summaries.R"))
  
  
  ## generate maps that won't change over time, e.g. strata maps
  source(file.path(mapping.path, "Maritimes-SUMMER-strata-map.R")) ## strata map, nothing there yet
	
  # source the code that defines the data extraction functions
	source(file.path(main.path, "data-and-stats.R"))
  
  # source the code that appropriately calls the function for each figure
  source(file.path(main.path, "figures.R"))

## actual function calls for species-level analyses
## first call is to generate the data files for each species
## second call is to generate the figures for each species
  
  # 2012-09-20: I'm turning the species list into the authoritative list by which the data extraction and the generation of figures in R is done. 
  # 2020-07-08: Note that every year the number of records will increase and the species available may also change.
  spec.list <- read.csv(file.path(main.path, "species-list-for-report.csv"),header=FALSE) # this list is itself generated from the above "summaries.R", which requires database connection and connection to WORMS to get AphiaID

  ## test for cod
  ## data.extract(4, 10)
  ## figures(fig=6, spec.num=10)
  
    
  species.L <- spec.list[spec.list$V9=='L',]$V4 # long timeseries
  species.S <- spec.list[spec.list$V9=='S',]$V4 # short timeseries
  species.LR <- spec.list[spec.list$V9=='LR',]$V4 # long timeseries rare
  species.SR <- spec.list[spec.list$V9=='SR',]$V4 # short timeseries rare
  species.I <- spec.list[spec.list$V9=='I',]$V4 # intermediate species

  # GROUP : well identified species with no DDHS fitting problems
  species.numbers <- species.L # c(10,11,12,13,14,16,23,40,41,42,43,60,300,4511,320,220,640,400,200,201,202,203,204,50,30,304,62,160,70,304,112,15,31,241) #,114) 

  
  # extract all the data and plot all the figures for GROUP
  print(paste("Starting data extracts, L species: ", Sys.time()))
  # The following extraction is done in folder 'Data'
  # Make sure that folder contains "DFO-strata-statistics.csv"
  	sapply(species.numbers, function(i){data.extract(extract.num=c(1,2,3,5,6,7), spec.num=i)})
  	print(paste("End data extract, starting figures, L species: ", Sys.time()))
  	
  	# The following extraction is done in folder 'Figures'
  	# Figures 10,11,20,21 also create shape files in the folder 'FGP' (Federal Geospatial Platform')
    sapply(species.numbers, function(i){figures(spec.num=i, fig=c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,17,18,19,20))})
	# sapply(species.L, function(i){figures(spec.num=i, fig=c(15,20))})
	# sapply(species.numbers, function(i){figures(spec.num=i, fig=c(19))})
	print(paste("End figures, L species: ", Sys.time()))
  
  # GROUP : well identified species with DDHS fitting problems
  #species.numbers <- c(610,143)
  #sapply(species.numbers, function(i){data.extract(extract.num=c(1,2,3,4,6,7), spec.num=i)})
  #sapply(species.numbers, function(i){figures(spec.num=i, fig=c(2,3,4,5,6,7,8,9,10,11,12,13,14,17,18,19,20))})
	
  # GROUP : recorded since 1999, invertebrates
  species.numbers <- species.S # c(2526,2550,2511,2211,2527,2521,2513,4321,340,2523)
	print(paste("Starting data extracts, S species: ", Sys.time()))
	sapply(species.numbers, function(i){data.extract(extract.num=c(8,9,10), spec.num=i)}) # extract only catch data
    print(paste("End data extract, starting figures, S species: ", Sys.time()))
	sapply(species.numbers, function(i){figures(spec.num=i, fig=c(13,14,16,5,18,17,19,21,22))}) # 
    print(paste("End figures, S species: ", Sys.time()))
	
  # GROUP : rare species
  species.numbers <- c(species.LR, species.SR) # c(341,742,637,816,741,240,159,52,4514,4512,414,604,520,500,621,412,51,303,626,4320,704,505,720,512,158,221,307,603,503,17,149,646,351,314,63,142,620,122,156,323,619,641,301,630,150,200,712,19,502,501,625,642,143) #,114, 2532,
	print(paste("Starting data extracts, LR species: ", Sys.time()))
	sapply(species.numbers, function(i){data.extract(extract.num=c(1), spec.num=i)}) # extract only catch data
	print(paste("End data extract, starting figures, LR species: ", Sys.time()))
	sapply(species.numbers, function(i){figures(spec.num=i, fig=c(16))}) # plot only tow locations
	print(paste("End figures, LR species: ", Sys.time()))
  
  # GROUP : intermediate species where abundance should be used instead of biomass
  species.numbers <- species.I # c(114, 647, 64, 410, 123, 623, 622, 61, 306, 701, 350, 880, 44)
	print(paste("Starting data extracts, I species: ", Sys.time()))
	sapply(species.numbers, function(i){data.extract(extract.num=c(1,3,4), spec.num=i)}) # extract catch data and generate distribution indices
	print(paste("End data extract, starting figures, I species: ", Sys.time()))
	sapply(species.numbers, function(i){figures(spec.num=i, fig=c(5,6,10,13,14,17,19))}) 
	print(paste("End figures, I species: ", Sys.time()))

  #sapply(species.numbers, function(i){figures(spec.num=i, fig=c(13,14))})
  #sapply(species.numbers, function(i){figures(spec.num=i, fig=c(5))})
  #
  
	## close ODBC connection
	odbcClose(chan)
	
print(paste("Script Atlas.R finished: ", Sys.time()))
q("no")

	## now run analyses for multiple species and trophic groups
	## identification of "core habitats"
	# habitat.suitability(species.num=species.numbers)

	## blah blah
