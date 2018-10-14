# TODO: Add comment
# 
# Author: dj2watso
###############################################################################


clean = function(corpus){
	corpus.clean <- corpus %>%
			tm_map(content_transformer(tolower)) %>% 
			tm_map(removePunctuation) %>%
			tm_map(removeNumbers) %>%
			tm_map(removeWords, stopwords(kind="en")) %>%
			tm_map(stripWhitespace) #%>%
			#tm_map(stemDocument)
	return(corpus.clean)
}

getDTM=function(corpus){
	corpus = Corpus(VectorSource(corpus))
	corpus.clean = clean(corpus)
	
	dtmShort = DocumentTermMatrix(corpus.clean)
	#browser()
	return(dtmShort)
}

convert_count <- function(x) {
	y <- ifelse(x > 0, 1,0)
	y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
	y
}

convertMemsafe = function(testdat){
	for(i in 1:nrow(testdat$termMatrix)){
		testdat$termMatrix[i,] = sapply(testdat$termMatrix[i,], convert_count)
	}
}

trainClassifier = function(class, dtm){
	dtmShort = 	data.frame(as.matrix(dtm), stringsAsFactors=TRUE)
	dtmShort = apply(dtmShort, 2, convert_count)
	print(c(dim(dtmShort), length(class)))
	#browser()
	classifier = train(dtmShort,class, 'nb',trControl=trainControl(method='cv',number=10))
	return(classifier)
}

loadFromcsv=function(file, lines, sskip){
	header <- names(read.csv(file, nrows=1, header=TRUE))
	lines = read.csv(file, nrows=lines, skip=sskip)
	names(lines) = header
	return(lines)
}

prepare=function(csvData,trainDTM=NULL, termfreq=5, includebody=TRUE){
	if(includebody){
		csvData$text = paste(csvData$card,csvData$body)
	}else{
		csvData$text = csvData$card
	}
	#csvData$termMatrix = getDTM(csvData$text)
	#csvData$termMatrix = apply(csvData$termMatrix, 2, convert_count)	
	return(csvData)
}

classify = function(classifier, testcorpus, trainedDTM, termfreq=5){
	#dtm = as.matrix(getDTM(testcorpus, trainedDTM))
	#dtmShort = apply(dtm, 2, convert_count)
	print("test Data formatted")
	print(dim(testcorpus))
	return(predict(classifier$finalModel, testcorpus))
}

evaluate = function(class, corpus, testProp){
	
	gc(FALSE)
	class = as.factor(class)
	testSize = floor(length(class)*testProp)
	testSet = sample(1:length(class), testSize, FALSE)
	
	trainingCorpus = corpus[-testSet]
	trainingClass = class[-testSet]
	
	testCorpus = corpus[testSet]
	testingClass = class[testSet]
	
	
	
	train.dtm = getDTM(trainingCorpus)
	test.dtm = getDTM(testCorpus, train.dtm)
	
	train.dtm = apply(train.dtm, 2, convert_count)
	test.dtm= apply(test.dtm, 2, convert_count)
	
	print("train/test built")
	
	nbc = trainClassifier(trainingClass, train.dtm)
	print("classifier trained")
	
	predict(nbc, test.dtm)
}

sepByClass = function(trainingText){
	classes = levels(trainingText$class)
	dtms = list("list", length(classes))
	for(i in 1:length(classes)){
		cClass = classes[i]
		dtms[[i]] = trainingText$text[which(trainingText$class == cClass)]
		attr(dtms[[i]],"className") = cClass
	}
	names(dtms) = classes
	return(dtms)
}

computeTfIdf = function(trainingData){
	corpusDTM = as.matrix(getDTM(trainingData$text))
	
	
	appearances = colSums(corpusDTM != 0)
	classGroups = sepByClass(trainingData)
	classDTM = lapply(classGroups,function(x){as.matrix(getDTM(x))})
	bigN = nrow(trainingData)
	idf = bigN/appearances
	#browser()
	
	classes = levels(trainingData$class)
	dn = list(classes, names(idf))
	
	tfIdf = matrix(rep(0,length(classGroups)*length(idf)), nrow= length(classGroups),ncol=length(idf), dimnames=dn)
	
	for(i in 1:length(classGroups)){
		counts = colSums(classDTM[[i]])
		tf = counts/sum(counts)
		
		#browser()
		for(term in colnames(classDTM[[i]])){
			tfIdf[i,term] = tf[term]*idf[term]*log(nrow(classDTM[[i]]))
			#browser()
		}
	}
	return(tfIdf)
}

classIDF = function(DTMRow, scores = FALSE, dfMat = tfIdf, retN = 1){
	#DTMRow: single row matrix with colnames
	#scores: return scores?
	#dfMat: specific tfIDf matrix or use default?
	score = rep(0, nrow(dfMat))
	for(i in 1:nrow(dfMat)){
		#browser()
		for(term in colnames(DTMRow)){
			#print(term)
			if(term %in% colnames(dfMat)){
				if(debug)
					browser()
				score[i] = score[i]+(dfMat[i,term]*DTMRow[,term])
			}
		}
	}
	#print(score)
	maxScore = which(score==max(score))
	#browser()
	if(max(score)==0)
		return("no matching terms")
	if(scores)
		return(score)
	if(retN>1)
		return(rownames(dfMat)[rev(top(score,retN))])
	return((rownames(dfMat)[maxScore])[1])
}

LeaveOneOutTest = function(trainingData, fraction = .1, confMatrix=FALSE, n=0){
	
	loo = function(i){
		dfMat = computeTfIdf(trainingData[-i,])
		if(n>0)
		{
			dfMat = topNDescriptors(dfMat, n)
		}
		#browser()
		classIDF(as.matrix(getDTM(trainingData$text[i])),FALSE, dfMat)
	}
	samp = sort(sample(1:nrow(trainingData), nrow(trainingData)*fraction,replace=FALSE ))
	if(debug)
		browser()
	res = unlist(lapply(samp, loo))
	#browser()
	if(confMatrix){
		return(confusionMatrix(res,trainingData$class[samp]))
	}
	sum(equals(res, trainingData$class[samp]))/length(samp)
	
	#histogram the precision results across classes
}

ClassifyForID = function(id, dataSet = testingtext, dfMat = tfIdf, n=3){
	post = which(dataSet$post_id == id)
	post = dataSet$text[post]
	
	classIDF(as.matrix(getDTM(post)), dfMat = dfMat, retN = n)
}

predictForCSV = function(data, dfMat = tfIdf, n=3){	
	ids = data$post_id
	#browser()
	classes = lapply(ids, function(id){ClassifyForID(id, data,dfMat,n)})
	classes = matrix(unlist(classes),nrow(data),n,byrow=TRUE)

	data$pred = classes[,1]
	
	#data$termMatrix=NULL
	
	write.csv(data,"classResultsFull.csv")
	
	return(data)
	
}
