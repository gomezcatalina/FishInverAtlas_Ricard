devSave<-
function(which.devName=NULL, dir=NULL, fileName=NULL, saveFormat=c("eps","pdf","png","tiff","bmp","wmf","emf"), quiet=FALSE){
	
	#--------------------------------------------------------------------------#
	#	Fonction auxiliaire : Sauver les fenêtres graphiques ouverte avec devOpen
	#	Dir is the directory
	#	fileName is name of the file without the extension
	#	quiet print the location of the saved plot
	#--------------------------------------------------------------------------#
	
	# Est-ce que la liste existe
	if(!exists(".devList")){
		stop(".devList ne peut \u{EA}tre trouv\u{E9} dans .GlobalEnv, devOpen doit \u{EA}tre utilis\u{E9} avant!")
	}else{
		# La liste existe
		devList<-get(".devList", inherit=TRUE)
		# Est-ce la bonne liste, vérification
		if(ncol(devList)!=7){
			# This is not the right .devList
			stop("Le format de .devList n'est pas correct, .devlist est effac\u{E9}!")
			rm(.devList)
		} 
	}
	# Si aucun nom est donné, le graphique actif est sauvé, trouve le numéro de ligne dans la liste
	if(is.null(which.devName)){ 
		which.dev<-dev.cur()
		lineNumber<-which(devList[,1]==which.dev)
		if(length(lineNumber)!=0) which.devName<-devList[lineNumber,2]
	}else{
		# Quel element de devList possède le nom ...
		lineNumber<-which(devList[,2]==which.devName)
		if(length(lineNumber)==0) stop("Ce nom de graphique n'existe pas dans .devList")
	}
		
	# Le graphique est dans .devList, sélection du graphique
	dev.set(devList[lineNumber,1])
	
	devName<-devList[lineNumber,2] 
	saveDir<-devList[lineNumber,3] 
	fileName<-devList[lineNumber,4]
	width<-devList[lineNumber,5]
	height<-devList[lineNumber,6]
	saveFormat<-devList[lineNumber,7]

	if(is.null(saveDir)) stop("devSave requier un chemin de dossier pour sauver le graphique!")
	if(is.null(fileName)) stop("devSave requier un nom de fichier pour sauver le graphique!")
	if(is.null(saveFormat)) stop("devSave requier un nom de format de fichier pour sauver le graphique!")
	if(is.null(width)) stop("devSave requier une largueur pour sauver le graphique!")
	if(is.null(height)) stop("devSave requier une hauteur pour sauver le graphique!")
	
	# Vérification de l'accès au dossier
	if(file.access(saveDir)!=0) stop("Le chemin de dossier donn\u{E9} n'existe pas!\n")
	
	fpath <- file.path(saveDir, paste(fileName,".",saveFormat,sep=""))
	if (.Device == "X11") {
		stop("Non test\u{E9} avec X11!\n")
	}
	else if (.Device == "quartz") {
		switch(match.arg(saveFormat), 
			eps = {
				dev.copy2eps(file = fpath)
	    	}, pdf = {
				dev.copy2pdf(file = fpath)
	    	}, png = {
				dev.print(png, file=fpath, width=width, height=height, unit="in", res=300)    	
	    	}, tiff = {
				dev.print(tiff, file=fpath, width=width, height=height, unit="in", res=300)
	    	}, bmp = {
				dev.print(bmp, file=fpath, width=width, height=height, unit="in", res=300)
	    	}, wmf = {
	    		print("Les figures ne peuvent etre sauv\uE9 en .wmf sous mac. \n") 
			}, emf = {
	    		print("Les figures ne peuvent etre sauv\uE9 en .emf sous mac. \n")
			}
	   
		)
	}
	else if (.Platform$OS.type == "windows") {
		switch(match.arg(saveFormat), 
			wmf = {
				savePlot(filename = fpath, type = "wmf", device = dev.cur(), restoreConsole = TRUE)
	    	}, eps = {
				setEPS()
				savePlot(filename = fpath, type = "eps", device = dev.cur(), restoreConsole = TRUE)
	    	}, pdf = {
				savePlot(filename = fpath, type = "pdf", device = dev.cur(), restoreConsole = TRUE)
	    	}, png = {
				savePlot(filename = fpath, type = "png", device = dev.cur(), restoreConsole = TRUE)
	    	}, tiff = {
				savePlot(filename = fpath, type = "tiff", device = dev.cur(), restoreConsole = TRUE)
	    	}, emf = {
				savePlot(filename = fpath, type = "emf", device = dev.cur(), restoreConsole = TRUE)
	    	}, bmp = {
				savePlot(filename = fpath, type = "pdf", device = dev.cur(), restoreConsole = TRUE)
	    	}
		)
	}
	else {
		warning("L'outil graphique n'est pas reconnu!\n")
	}
	if(quiet==FALSE){
		cat("\nPlot ", fileName, ".", saveFormat, " est sauv\u{E9} dans:\n\t", fpath, "\n", sep="")
	}
}
