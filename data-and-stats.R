## main function to perform data extraction and generate data files to be used in the different figures and maps of the groundfish survey atlas
##
## this function write the appropriate text files that are to be used by the plotting routines and also generate a single Excel data file for each species
## Modification history:
## 2012-09-10: to accomodate the fact that not all analyses can be conducted for each species, I'm adding a switch to handle which data files are to be produced
## description
## extract 1: catch abundance and biomass data
## extract 2: depth, temperature and salinity distribution 
## extract 3: distribution indices
## extract 4: yearly stratified random estimates of abundance and weight
## extract 5: density-dependent habitat selection
## extract 6: length frequencies
## extract 7: length-weight relationship
## extract 8: catch abundance and biomass data from 1999 onwards, for inverts
## extract 9: yearly stratified random estimates of abundance and weight from 1999 onwards, for inverts
## extract 10:  distribution indices from 1999 onwards, for inverts


data.extract <- function(extract.num=c(1:10), spec.num, writexls=FALSE) {

	# load required libraries
	require(RODBC, quietly=TRUE, warn.conflicts = FALSE)
	require(xlsx, quietly=TRUE, warn.conflicts = FALSE)
	require(xtable, quietly=TRUE, warn.conflicts = FALSE)
	require(MASS, quietly=TRUE, warn.conflicts = FALSE)
#	require(gdata, quietly=TRUE, warn.conflicts = FALSE)

	# base path
	path.Base1=main.path
	path.Base2=main.path
	# R functions path
	path.R=file.path(path.Base1, "Data-extraction")
	# data path
	path.Data=file.path(path.Base2,"Figures-data")

	if(!file.exists(path.Base1)) stop(paste(path.Base1, "doesn't exist!\n"))
	if(!file.exists(path.Base2)) stop(paste(path.Base2, "doesn't exist!\n"))
	if(!file.exists(path.R)) stop(paste(path.R, "doesn't exist!\n"))
	if(!file.exists(path.Data)) stop(paste(path.Data, "doesn't exist!\n"))

#--------------------------------------------------------------------------#
# source the R files associated with each query and/or analysis
#--------------------------------------------------------------------------#
	source(file.path(path.R, "data-extract-catch.R")) # extract the tow-level data
	source(file.path(path.R, "data-extract-catch-short.R")) # extract the tow-level data, 1999 onwards
	source(file.path(path.R, "generate-cumul-dist.R")) # uses the tow-level data to generate the cumulative distribution of catches and environmental variables
	source(file.path(path.R, "compute-stratified.R")) # uses the tow-level data to generate yearly stratified random estimates
	source(file.path(path.R, "compute-distribution.R")) # uses the abundance tow-level data to generate yearly distribution indices
	source(file.path(path.R, "compute-distribution-usingbiomass.R")) # uses the biomass tow-level data to generate yearly distribution indices
	source(file.path(path.R, "length-frequency.R")) # extract length frequency information
	source(file.path(path.R, "length-weight.R")) # extract length-weight information

	for(i in extract.num){
	switch(i, 
		1 == { 	# catch abundance and biomass data
		## catch abundance and biomass data
		catch.df <- extract.catch.fct(spec.num)
		fn <- paste("SS",spec.num,"_catch.csv",sep="")
		write.csv(catch.df, file.path(path.Data, fn), row.names=FALSE)
			},
		2 == { 	#
		## depth distribution
		dat.fn <- paste("SS",spec.num,"_catch.csv",sep="")
		#depth.dist.list <- generate.cumul.dist(extract.catch.fct(spec.num), "depth")
		depth.dist.list <- generate.cumul.dist(read.csv(file.path(path.Data, dat.fn)), "depth")
		depth.dist.df <- depth.dist.list[[1]]
		depth.dist.xt <- depth.dist.list[[2]]
		fn <- paste("SS",spec.num,"_depthdist.csv",sep="")
		fn.xt <- paste("SS",spec.num,"_depthdist.tex",sep="")
		write.csv(depth.dist.df, file.path(path.Data, fn), row.names=FALSE)
		print.xtable(xtable(depth.dist.xt), type='latex', file=file.path(path.Data, fn.xt),floating=FALSE)

		## temperature distribution
		#temperature.dist.list <- generate.cumul.dist(extract.catch.fct(spec.num), "temperature")
		temperature.dist.list <- generate.cumul.dist(read.csv(file.path(path.Data, dat.fn)), "temperature")
		temperature.dist.df <- temperature.dist.list[[1]]
		temperature.dist.xt <- temperature.dist.list[[2]]
		fn <- paste("SS",spec.num,"_temperaturedist.csv",sep="")
		fn.xt <- paste("SS",spec.num,"_temperaturedist.tex",sep="")
		write.csv(temperature.dist.df, file.path(path.Data, fn), row.names=FALSE)
		print.xtable(xtable(temperature.dist.xt), type='latex', file=file.path(path.Data, fn.xt),floating=FALSE)

		## salinity distribution
		#salinity.dist.list <- generate.cumul.dist(extract.catch.fct(spec.num), "salinity")
		salinity.dist.list <- generate.cumul.dist(read.csv(file.path(path.Data, dat.fn)), "salinity")
		salinity.dist.df <- salinity.dist.list[[1]]
		salinity.dist.xt <- salinity.dist.list[[2]]
		fn <- paste("SS",spec.num,"_salinitydist.csv",sep="")
		fn.xt <- paste("SS",spec.num,"_salinitydist.tex",sep="")
		write.csv(salinity.dist.df, file.path(path.Data, fn), row.names=FALSE)
		print.xtable(xtable(salinity.dist.xt), type='latex', file=file.path(path.Data, fn.xt),floating=FALSE)
	
		# single xtable with all three
		fn.xt <- paste("SS",spec.num,"_alldist.tex",sep="")
		my.df<-data.frame(Freq=depth.dist.xt[,1],Depth=depth.dist.xt[,2], Temp=temperature.dist.xt[,2], Sal=salinity.dist.xt[,2])
		print.xtable(xtable(my.df,digits=c(0,0,0,1,2)), type='latex', file=file.path(path.Data, fn.xt), include.rownames=FALSE, floating=FALSE)
			},
		3 == { 	#
			## distribution indices
		distribution.df <- distribution.fct(extract.catch.fct(spec.num))
		fn <- paste("SS",spec.num,"_distribution.csv",sep="")
		write.csv(distribution.df, file.path(path.Data, fn), row.names=FALSE)
		
		## distribution indices using biomass
		distribution.df <- distribution.usingbiomass.fct(extract.catch.fct(spec.num))
		fn <- paste("SS",spec.num,"_distribution-usingbiomass.csv",sep="")
		write.csv(distribution.df, file.path(path.Data, fn), row.names=FALSE)

			},
		4 == { 	#
			## yearly stratified random estimates of abundance and weight
		strat.list <- stratified.fct(extract.catch.fct(spec.num), DDHS=FALSE)
		stratified.df <- strat.list[[1]]
		fn <- paste("SS",spec.num,"_stratified.csv",sep="")
		write.csv(stratified.df, file.path(path.Data, fn), row.names=FALSE)
			},
		5 == { 	#
			## yearly stratified random estimates of abundance and weight and DDHS
		strat.list <- stratified.fct(extract.catch.fct(spec.num), DDHS=TRUE)
		stratified.df <- strat.list[[1]]
		fn <- paste("SS",spec.num,"_stratified.csv",sep="")
		write.csv(stratified.df, file.path(path.Data, fn), row.names=FALSE)
		# DDHS
		DDHS.list <- strat.list[[2]]
		DDHS.df <- data.frame(par.name=names(coef(DDHS.list[[1]])), lm1.est = as.numeric(coef(DDHS.list[[1]])), glm.poisson.est = as.numeric(coef(DDHS.list[[2]])), glm.nb.est = as.numeric(coef(DDHS.list[[3]])) )
		fn <- paste("SS",spec.num,"_DDHS.csv",sep="")
		write.csv(DDHS.df, file.path(path.Data, fn), row.names=FALSE)
	
		
		# slope estimates and average stratum abundance
		ll <- length(as.numeric(coef(DDHS.list[[3]])))
		ii <- c(2,seq((ll/2)+2,ll))
	
		slopes.df <- data.frame(stratum=names(DDHS.list[[4]]), mean.n=as.numeric(DDHS.list[[4]]), slope.glm.poisson = (coef(DDHS.list[[2]])[ii]), slope.glm.poisson.stderr = (summary(DDHS.list[[2]])$coefficients[,2][ii]), slope.glm.nb = (coef(DDHS.list[[3]])[ii]), strat.quan95=DDHS.list[[7]], strat.quan75=DDHS.list[[6]], strat.median.top25=DDHS.list[[8]])
	
		fn <- paste("SS",spec.num,"_DDHSslopes.csv",sep="")
		write.csv(slopes.df, file.path(path.Data, fn), row.names=FALSE)

			},
		6 == { 	#
			## length frequencies
	lf.list <- lf.fct(spec.num)
	lf.df.all <- lf.list[[1]]
	fn <- paste("SS",spec.num,"_lf.csv",sep="")
	write.csv(lf.df.all, file.path(path.Data, fn), row.names=FALSE)
	lf.df.4vw <- lf.list[[2]]
	fn <- paste("SS",spec.num,"_lf4vw.csv",sep="")
	write.csv(lf.df.4vw, file.path(path.Data, fn), row.names=FALSE)
	lf.df.4x <- lf.list[[3]]
	fn <- paste("SS",spec.num,"_lf4x.csv",sep="")
	write.csv(lf.df.4x, file.path(path.Data, fn), row.names=FALSE)

			},
		7 == { 	#
			## length-weight relationship
	lw.list <- lw.fct(spec.num)
	lw.df <- lw.list[[1]]
	fn <- paste("SS",spec.num,"_lw.csv",sep="")
	write.csv(lw.df, file.path(path.Data, fn), row.names=FALSE)
	
	lw.df.nafo4x <- lw.list[[2]]
	fn <- paste("SS",spec.num,"_lw4x.csv",sep="")
	write.csv(lw.df.nafo4x, file.path(path.Data, fn), row.names=FALSE)

	lw.df.nafo4vw <- lw.list[[3]]
	fn <- paste("SS",spec.num,"_lw4vw.csv",sep="")
	write.csv(lw.df.nafo4vw, file.path(path.Data, fn), row.names=FALSE)

			},
		8 == { 	#
			## catch abundance and biomass data from 1999 onwards, for inverts
		catch.df <- extract.catch.short.fct(spec.num)
		fn <- paste("SS",spec.num,"_catch.csv",sep="")
		write.csv(catch.df, file.path(path.Data, fn), row.names=FALSE)

			},
		9 == { 	#
			## yearly stratified random estimates of abundance and weight
		strat.list <- stratified.fct(extract.catch.short.fct(spec.num), DDHS=FALSE)
		stratified.df <- strat.list[[1]]
		fn <- paste("SS",spec.num,"_stratified.csv",sep="")
		write.csv(stratified.df, file.path(path.Data, fn), row.names=FALSE)
			},
		10 == { 	#
			## distribution indices
		distribution.df <- distribution.fct(extract.catch.short.fct(spec.num))
		fn <- paste("SS",spec.num,"_distribution.csv",sep="")
		write.csv(distribution.df, file.path(path.Data, fn), row.names=FALSE)
		
		## distribution indices using biomass
		distribution.df <- distribution.usingbiomass.fct(extract.catch.short.fct(spec.num))
		fn <- paste("SS",spec.num,"_distribution-usingbiomass.csv",sep="")
		write.csv(distribution.df, file.path(path.Data, fn), row.names=FALSE)

			},
   		NULL == {
				stop("This figure number does not exist!\n")
				}
			) # end switch
		
	
	}
	
	
	if(writexls)# write a massive single spreadsheet with each data frame as a worksheet
	{
	excel.fn <- paste("SS",spec.num,".xlsx",sep="")
	write.xlsx2(catch.df, file.path(path.Data, excel.fn), sheetName="catch")
	write.xlsx2(depth.dist.df, file.path(path.Data, excel.fn), sheetName="depth", append=TRUE)
	write.xlsx2(temperature.dist.df, file.path(path.Data, excel.fn), sheetName="temperature", append=TRUE)
	write.xlsx2(salinity.dist.df, file.path(path.Data, excel.fn), sheetName="salinity", append=TRUE)
	write.xlsx2(distribution.df, file.path(path.Data, excel.fn), sheetName="distribution", append=TRUE)
	write.xlsx2(stratified.df, file.path(path.Data, excel.fn), sheetName="stratified", append=TRUE)
	write.xlsx2(lf.df, file.path(path.Data, excel.fn), sheetName="lf", append=TRUE)
	write.xlsx2(lw.df, file.path(path.Data, excel.fn), sheetName="lw", append=TRUE)	
	}
	
}
