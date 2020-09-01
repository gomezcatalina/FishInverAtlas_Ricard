## mapping functions and data requirements such as polygons and masks, this file also creates a survey map with strata polygons and NAFO divisions
	# base path
	path.Base1=main.path
	path.Base2=main.path
	# R functions path
	path.R=file.path(path.Base1, "Figures-Rcode")
	# data path
	path.Data=file.path(path.Base2,"Data")
	# mapping path
	path.Map=file.path(path.Base2, "Mapping")
	# path to store figures
	path.Figures=file.path(path.Base2, "Figures-actual")	

## DFO SUMMER
info<-sqlQuery(chan,paste("select substr(featureid,18,3),featureseq, xp,yp from mflib.mwacon_mapobjects where featureid like 'GF_SUMMER_STRATA%' "))
					names(info)<-c("PID","POS","X","Y")
					info.o<-with(info,info[order(PID,POS),])
					pp<-fixBound(info.o,tol=0.01000001)
					ab<-closePolys(pp)
					dfo.summer<-ab
					attr(dfo.summer,"projection")<-"LL"
					dfo.summer$X <- dfo.summer$X + 360
					
rm(info, info.o, ab, pp)

dfo.summer$label <- dfo.summer$PID
out.polys.dfo <- as.PolySet(data.frame(PID=rep(seq(443,445),each=5), POS=rep(seq(1,5),3), X=rep(c(301.5,302.5,302.5,301.5,301.5),3) , Y=c(43,43,42.8,42.8,43,42.6,42.6,42.4,42.4,42.6,42.2,42.2,42,42,42.2), label=rep(seq(443,445),each=5)))

dfo.summer.temp <- dfo.summer
dfo.summer.temp[dfo.summer.temp$PID==444,]$PID <- -99
dfo.summer.temp[dfo.summer.temp$PID==-99,]$label <- '443,444,445'

dfo.summer2 <- rbind(dfo.summer.temp, out.polys.dfo)
oo <- order(dfo.summer2$PID)
dfo.summer2 <- dfo.summer2[oo,]

					
	require(PBSmapping, quietly=TRUE, warn.conflicts = FALSE)
	data(worldLLhigh)
	
	source(file.path(path.R, "devOpen.R"))
	source(file.path(path.R, "devSave.R"))

	# name of graphics file
	devName <- "SurveyMap_Strata"
	
	# open graphics device
	devOpen(devName, saveDir=path.Figures, fileName=paste("SS", "_", devName, sep=""), saveFormat="eps", width=4, height=2+0.75)
	
	# graphics parameters
	#par(mfrow=c(1,1), omi=c(0,0,0,0), mai=c(0.35, 0.5+pos.ylabel[1], 0.1, 0.1), ps=8, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
	par(mfrow=c(1,1), omi=c(0,0,0,0), 
	    ps=8, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
	# text parameters
	cexF=list(title=1, axis=0.8,labels=0.9,legend=1, unit="in")
	
plotMap(worldLLhigh, xlim=c(291.5,303.5), ylim=c(41.5,48), col="#FFF7BC",plt=c(0.1,0.9,0.1,0.9),border='black',axes=TRUE,tckLab=FALSE,xlab="",ylab="")
addPolys(dfo.summer2)
addLabels(data.frame(PID=unique(dfo.summer2$PID),label=unique(dfo.summer2$label)), placement="CENTROID", polys=dfo.summer2, cex=0.8)

#text(301.1, 42.9, "443", cex=0.5)
#text(301.1, 42.5, "444", cex=0.5)
#text(301.1, 42.1, "445", cex=0.5)
lines(c(301.3, 301.7), c(45.2,43.1),lwd=0.5)

devSave(devName)


	# name of graphics file
	devName <- "SurveyMap_NAFO"
	
	# open graphics device
	devOpen(devName, saveDir=path.Figures, fileName=paste("SS", "_", devName, sep=""), saveFormat="eps", width=4, height=2+0.75)
	
	# graphics parameters
	par(mfrow=c(1,1), omi=c(0,0,0,0), mai=c(0.35, 0.5+pos.ylabel[1], 0.1, 0.1), ps=8, cex=1, xpd=TRUE, lheight=0.5, xaxs="r", yaxs="r")
	
	# text parameters
	cexF=list(title=1, axis=0.8,labels=0.9,legend=1, unit="in")
	
plotMap(worldLLhigh, xlim=c(291.5,303.5), ylim=c(41.5,48), col='white',plt=c(0.1,0.9,0.1,0.9),border='black',axes=TRUE,tckLab=FALSE,xlab="",ylab="")
text(296.5,45, "Nova Scotia")
text(293,46.5, "New Brunswick")
text(297,46.25, "PEI")
text(302,46.5, "Laurentian Channel")
text(300,45, "Eastern Scotian Shelf")
text(296,43.5, "Western Scotian Shelf")
text(293.5,44.75, "Bay of Fundy")
text(292.5,42, "Georges Bank")
text(293.5,42.5, "Fundian Channel")


devSave(devName)
