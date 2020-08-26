## single map with tow locations

figure16.fct <- function(dat.in, cex.in, pos.ylabel=c(0,0)) {
# fn <- file.path(path.ATLAS, "/Data/SS10_catch.csv")
# catch.dat <- read.csv(fn, header=TRUE)

my.xlim <- c(291.25,303.75)
my.ylim <- c(41,47.5)

xx.lon<-pretty(my.xlim,n=5)
yy.lat<-pretty(my.ylim, n=4)

rr <- range(dat.in$YEAR)
yrs.labels <- paste(rr[1],rr[2],sep="-")

my.cols.palette <- c('white','#FEF0D9', '#FDCC8A', '#FC8D59', '#E34A33', '#B30000')

i<-1
yy <- strsplit(yrs.labels[i],"-")
tt <- subset(dat.in, YEAR >=as.numeric(yy[[1]][1]) & YEAR <= as.numeric(yy[[1]][2]) )
tt$occ <- ifelse(tt$totno.corr==0,0,1)
pr.occ <- round(mean(tt$occ), digits=3)


plotMap(worldLLhigh, my.xlim, my.ylim, col=grey(0.9),plt=c(0,1.0,0,1.0),border=grey(0.7),axes=FALSE,tckLab=FALSE,xlab="",ylab="")
#plotMap(worldLLhigh, my.xlim, my.ylim, col=grey(0.9),plt=c(0,1.0,0,1.0),border=grey(0.7),axes=TRUE,tckLab=TRUE,xlab="",ylab="")
text(293.2,46.4,yrs.labels[i],cex=1.5)
text(293.2,45.9,paste("P(occ) = ",pr.occ,sep=""),cex=1.25)
text(293.2,45.6,paste("n = ",sum(tt$occ),sep=""),cex=1.25)

ttt <- subset(tt, occ==1)
points(360+ttt$lon, ttt$lat, type='p', pch=3, col='red')

par(las=1)
axis(side=1, at=xx.lon[2:7], labels=paste(360-xx.lon[2:7],"\u{B0}O",sep=""))
axis(side=3, at=xx.lon[2:7], labels=paste(360-xx.lon[2:7],"\u{B0}W",sep=""))
par(las=2)
axis(side=2, at=yy.lat, labels=paste(yy.lat,"\u{B0}N",sep=""))
axis(side=4, at=yy.lat, labels=paste(yy.lat,"\u{B0}N",sep=""))

box()


} # end function
