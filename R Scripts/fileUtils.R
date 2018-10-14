# TODO: Add 
# 
# Author: dj2watso
###############################################################################


extractAllCSVs=function(listOfDirs, ListOfCSV, n=10){
	rootDir = pwd()
	allData = lapply(listOfDirs,
			function(dir){
				#setwd(dir) #switch to directory
				print(dir)
				dat = lapply(ListOfCSV, function(file){
							file = paste(dir, "/", file, sep="")
							print(file)
							extractCSV(file) #pull data from csv file
						})	
				#cd(rootDir) #switch back to rootDir
				dat
			})
	print(dim(allData))
	closeAllConnections()
	return(allData)
}
extractCSV=function(filename, n=10){
	#print(paste(filename))
	rawData = read.csv(filename, header = FALSE,na.strings = c("", "NA"),sep=',')
	if(debug)
		browser()
	App = rawData[2,1]
	Package = rawData[2,2]
	Time = rawData[2,3]
	Duration = rawData[2,4]
	colNames = rawData[3,1:8]
	#print(colNames)
	#browser("", debug)
	for(i in 4:nrow(rawData)){			#find end of data
		if(all(is.na(rawData[i,]))){
			break
		}
	}
	rowNames = as.character(unlist(seq(from = 1, to = i-1, by = 1)))
	#print(dim(rowNames))
	
	dataPoints = as.matrix(rawData[c(6:i-1), 1:8]) #trim
	storage.mode(dataPoints) <- "numeric"
	
	
	#apply attributes
	attr(dataPoints, 'app') = App
	attr(dataPoints, 'package') = Package
	attr(dataPoints, 'time') = Time
	attr(dataPoints, 'duration') = Duration
	#colNames = colNames[,-9]
	#print(colNames)
	
	
	dataPoints = filterRows(dataPoints)
	print("dim:")
	print(dim(dataPoints))
	#print(typeof(dataPoints))
	#browser("", debug)
	colnames(dataPoints) = as.character(unlist(colNames,TRUE))
	
	rownames(dataPoints) <- 1:nrow(dataPoints)
	computeAttr(dataPoints)
	return(splitInstances(dataPoints,n))
}

extractTrace=function(filename,name = filename){
	dataFile = file(filename)
	rawData = readLines(dataFile, -1)
	
	rawData = strsplit(rawData, ",|:")
	return(rawData)
	
	delim = vector("numeric", 30)
	d = 1
	
	for(i in 1:length(rawData)){
		if(length(rawData[[i]])==1){
			delim[d] = i
			d=d+1
		}
	}
}








