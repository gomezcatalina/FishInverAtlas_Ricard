devOpen<-function(devName=NA, saveDir=NA, fileName=NA, saveFormat=c("eps","pdf","png","tiff","bmp"), width=5, height=5){
	#--------------------------------------------------------------------------#
	#	Fonction auxiliaire : Ouverture de fenêtres graphiques
	#
	#	devName: un vecteur de numéro de device dev.cur() ayant les noms de fenêtres
	#	saveDir: chemin ou le fichier sera sauvé
	#	fileName: nom du fichier sauvé
	#	width: largeur de la fenêtre crée
	#	height: hauteur de la fenêtre crée
	#--------------------------------------------------------------------------#
	if(is.na(devName)) stop("La fen\u{EA}tre graphique do\u{EE} avoir un nom!\n")
	
	devNumber<-0
	new=FALSE
	replace=FALSE
	
	lll=dev.list()
	devList=NULL
	
	# Aucune fenêtre graphique d'ouverte
	if(is.null(lll)) {
		new=TRUE
	}else{
		# Non il n'existe pas
		if(!exists(".devList")){
			#print(".devList n'existe pas!\n")
			new=TRUE
		}else{ # Il existe
			#  Capture la liste
			devList<-get(".devList", inherit=TRUE)
			# Vérifie si elle est conforme 
			#print(".devList existe!\n")
			if(ncol(devList)!=7){
				new=TRUE
				#print(".devList est non conforme!\n")
			}
		}
	}	
	a=0
	b=0
	# Comparaison entre les fenêtres ouverte et le contenue de la liste.
	if(!is.null(lll) & new==FALSE){
		for(i in 1:nrow(devList)){
			tmp=which(lll==devList[i,1])
			# Pas de match
			if(length(tmp)==0){
				a=1
			}else{
				if(b==0) keep=i
				else keep=c(keep,i)
				b=1
			}
		}
		if(b==1 & a==1){
			#print("Des fenetres ouvertes correspondents a devList mais il y a des trous!\n")
			devList=devList[keep,]
		}else if(b==1 & a==0){
			#print("Des fenetres ouvertes correspondents a devList!\n")
			devList=devList[keep,]
		}else if(b==0 & a==1){
			#print("Aucune des fenetres ouvertes ne correspond a devList!\n")
			new=TRUE
		}else{ 
			#print("RIEN!\n")
		}
		
	}
	
	# Retire les doublons si présent de devList (devName, devNumber)
	if(!is.null(lll) & new==FALSE){
		tmp=as.numeric(levels(as.factor(devList[,1])))
		for(i in tmp){
			dup=which(devList[,1]==i)
			len.dup=length(dup)
			if(len.dup>=2){ 
				devList=devList[-dup[2:len.dup],]
				#print("Duplicata de devNumber plus que un trouve!\n")
			}
		}
		tmp=levels(as.factor(devList[,2]))
		for(i in tmp){
			dup=which(devList[,2]==i)
			len.dup=length(dup)
			if(len.dup>=2 ){
				devList=devList[-dup[2:len.dup],]
 				#print("Duplicata de devNames, plus que un de trouve!\n")
			}
		}
	}
		
	devNumber=0
	# Est-ce que le graphique existe déjà dans devList
	if(new==FALSE){
		tmp=which(devList[,2]==devName)
		if(length(tmp)==1){ 
			#print("Le graphique existe!\n")
			devNumber=devList[tmp,1]
			devList[tmp, ] <- data.frame(devNumber, devName, I(saveDir), I(fileName), I(width), I(height), I(saveFormat))
			replace=TRUE
		}
	}
	
	if(devNumber==0){
		#print("Ouverture nouvelle fenetre!\n")
		if(.Platform$OS.type=="windows"){
			windows(width=width, height=height)
		}else if(.Platform$OS.type=="unix"){
			quartz(title=as.character(devName),width=width, height=height)
		}else{ 
			warning("La .Platform$OS.type n'est pas reconnue")
		}
		devNumber=dev.cur()		
	}else{
		#print("Selection fenetre!\n")
		dev.set(devNumber)
	}

	#Loading the graphic number
	if(new==TRUE){
		#print("Creation de devList!\n")
		devList<-data.frame(devNumber, devName, I(saveDir), I(fileName), I(width), I(height), I(saveFormat))
		devList[1,1]<-dev.cur()
		devList[1,2]<-devName
		devList[1,3]<-saveDir
		devList[1,4]<-fileName
		devList[1,5]<-width
		devList[1,6]<-height
		devList[1,7]<-saveFormat
	}else{
		if(replace==FALSE){
			#print("Ajout a devList!\n")
			devList<-rbind(devList, data.frame(devNumber, devName, saveDir, fileName, width, height, saveFormat))
		}
	}	
	assign(".devList", devList, env=.GlobalEnv)
	invisible()
}