# TODO: Add comment
# 
# Author: dj2watso
###############################################################################

library(e1071)
library("tm")
library("magrittr")
library("klaR")
library("caret")
library("plyr")
library("class")
library("pracma")
library('ggplot2')
library('forecast')
library('tseries')
library('lmtest')

wait <- function()
{
	cat ("Press [enter] to continue")
	line <- readline()
}

insertRow = function(numMat, index, newRow){
	return(rbind(numMat[1:(index-1),],newRow,numMat[index:nrow(numMat),]))	
}


refresh = function(){
	source("Rstuff/fileUtils.R")
	source("Rstuff/manifest.R")
	source("Rstuff/dataUtils.R")
	source("Rstuff/plottingUtils.R")
	source("Rstuff/postClassification.R")
	gc()
}

cdh = function(){
	cd("C:/Users/dj2watso/eclipse-workspace/appTesting")
}


memory.limit(5000)

lv = function(){
	
	return(.Last.value)
}

st = function(){
	return(traceback())
}

num = function(x){
	return(as.numeric(x))
}

filterRows =function(final){
	row.is.na = apply(final, 1, function(x){any(is.na(x))})
	print(sum(row.is.na))
	return(final[!row.is.na,])
}

library(pryr)
checkMemory=function(){
	for(object in ls(envir=.GlobalEnv)){
		print(paste(deparse(substitute(object)),object.size(object)))
	}
}

top <- function(x, n){
	tail( order(x), n )
}

bp = function(){
	if(debug)
		browser()
}

rf = refresh

getName = function(x)
{
	deparse(substitute(x))
}

trimToSmaller=function(x,y){
	xl=length(x)
	yl=length(y)
	if(xl<yl)
		return(c(x,y[1:length(x)]))
	else
		return(list(x[1:length(y)],y))
}



library(e1071)
library("tm")
library("magrittr")
library("klaR")
library("caret")
library("plyr")
library("class")
library(pracma)




#add matrices elementwise
add <- function(x) Reduce("+", x)

#adds ternary operator cause why not :P

`?` <- function(x, y)
	eval(
			sapply(
					strsplit(
							deparse(substitute(y)), 
							":"
					), 
					function(e) parse(text = e)
			)[[2 - as.logical(x)]])

setwd(wd)
