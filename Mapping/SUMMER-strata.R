##
## strata shapefile obtained from SABS

require(PBSmapping, quietly=TRUE, warn.conflicts = FALSE)
data(worldLLhigh)

# SS.strata <- importShapefile("/Mapping/ScotianShelfStrataUTM83F.shp", readDBF=TRUE, projection="UTM", zone=19)
fn<-file.path(main.path, "/Mapping/ScotianShelfStrataUTM83F.shp")
SS.strata <- importShapefile(fn, readDBF=TRUE, projection="UTM", zone=19)

SS.strata.LL <- convUL(SS.strata, km=FALSE)
SS.strata.LL$X <- SS.strata.LL$X 

SS.strata.LL2 <- SS.strata.LL
SS.strata.LL2$X <- SS.strata.LL2$X + 360


my.pids <- unique(SS.strata.LL$PID)
strata.list <- lapply(my.pids, function(i){subset(SS.strata.LL, PID == i)})


fn <- "SUMMER-strata-multi-SABS.dat"
sink(fn)
for(i in 1:length(my.pids)){
ss <- strata.list[[i]]
ss.num <- ss$PID[1]
cat(paste("> ", ss.num, "\n", sep=""), file=fn, append=TRUE)
cat(paste(ss$X,",",ss$Y,"\n",sep=""), file=fn, append=TRUE)
}
sink()

## mask as the union of all strata polygons
SUMMER.strata.mask <- joinPolys(SS.strata.LL, operation='UNION')

SUMMER.strata.mask$X <- SUMMER.strata.mask$X + 360
SUMMER.strata.mask.fix <- fixBound(SUMMER.strata.mask, tol=0.1)


fn<-file.path(main.path, "/Mapping/ScotianShelfStrataUTM83F_dissolve.shp")
SS.strata.mask <- importShapefile(fn, readDBF=TRUE, projection="UTM", zone=19)

# SS.strata.mask <- importShapefile("C:/Documents and Settings/RicardD/My Documents/Dropbox/ATLAS_poissons_SS/Mapping/ScotianShelfStrataUTM83F_dissolve.shp", readDBF=TRUE, projection="UTM", zone=19)


SS.strata.mask.LL <- convUL(SS.strata.mask, km=FALSE)
SS.strata.mask.LL$X <- SS.strata.mask.LL$X + 360

#plotMap(worldLLhigh, xlim=c(290,303), ylim=c(41,48))
#addPolys(SS.strata.mask.LL, col=grey(0.9))

out.polys.dfo <- as.PolySet(data.frame(PID=rep(seq(443,445),each=5), SID=rep(1,15), POS=rep(seq(1,5),3), X=rep(c(300.43,301.43,301.43,300.43,300.43),3) , Y=c(45.72,45.72,45.52,45.52,45.72,45.42,45.42,45.22,45.22,45.42,45.12,45.12,44.92,44.92,45.12)))

my.xlim <- c(291.25,303.75)
my.ylim <- c(41,47.5)

#plotMap(worldLLhigh, my.xlim, my.ylim, col=grey(0.9),plt=c(0.05,0.95,0.05,0.95),border=grey(0.7),axes=TRUE,tckLab=TRUE,xlab="",ylab="")
#addPolys(out.polys.dfo)
#addPolys(SS.strata.LL2)


## manual fix to replace Stratum 444 with the above strata 443, 444 and 445
SS.strata.DRfixed <- SS.strata.LL2

## replace PID with the actual strata numbers
for(i in 1:length(unique(SS.strata.DRfixed$PID))){
s.num <- subset(attr(SS.strata.DRfixed, "PolyData"), PID==i)$Stratum1
SS.strata.DRfixed[which(SS.strata.DRfixed$PID==i),]$PID <- as.numeric(as.character(s.num))
}

SS.strata.DRfixed.temp <- rbind(subset(SS.strata.DRfixed, PID!=444), out.polys.dfo)

oo<-order(SS.strata.DRfixed.temp$PID, SS.strata.DRfixed.temp$POS)

SS.strata.DRfixed.final <- SS.strata.DRfixed.temp[oo,]

# write.csv(data.frame(pid=out.polys.dfo$PID,x=out.polys.dfo$X, y=out.polys.dfo$Y), "out-polys.csv")