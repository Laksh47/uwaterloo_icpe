# TODO: Add comment
# 
# Author: dj2watso
###############################################################################


ma <- function(arr, n=15){	#convert vector to moving average
	res = arr
	#print(typeof(arr))
	for(i in n:length(arr)){
		#browser()
		res[i] = mean(as.numeric(arr[(i-n):i]))
	}
	res
}

maCols = function(x, n=15){	#convert vector or matrix to moving average form
	#browser("",debug)
	res = mat.or.vec(nrow(x),ncol(x))
	for(i in seq(2,ncol(x),2)){
		res[,i] = ma(unlist(x[,i]), n)
	}
	for(i in seq(1,ncol(x),2)){
		res[,i] = x[,i]
	}
	res = res[n:nrow(res),]
	colnames(res) = colnames(x)
	return(res)
}

sampleVariance=function(lst,smooth, metric = "CPU Load (Normalized) [%]"){
	aggregator=getAggregator(smooth, TRUE, n=10)
	if(debug)
		browser()
	lst = aggregator(lst)	
	d = length(lst)
	n = nrow(lst[[1]])
	timeInd = which(colnames(lst[[1]])==metric)-1
	#if(smooth)
		#lst = lapply(lst, maCols, 10)
	#browser
	
	#compute vectors of variance
	res = vector("numeric",n)
	
	for(i in 1:n){
		sumDif = 0
		pointMean = lst[[d]][i,metric]
		for(inst in lst[-d]){
			val = inst[i,metric]
			if(!is.na(val)){
				sumDif=sumDif+(val-pointMean)^2
			}	
		}
		#browser()
		res[i] = sumDif/(d-1)
	}
	
	return(res)
}

splitInstances = function(x, n){ 
	#given a profile consisting of multiple runs (x) separated by screen shut-off, number of runs(n)
	#list of mat consisting of each instance with junk at beginning, end, and between runs culled
	#x must have column labeled "Screen State"
	
	l = vector("list", n)
	#print(colnames(x))
	if(debug)
		browser()
	scrnst = x[,'Screen State']
	i=1
	#The first screen shut off indicates the beginning of the experiment
	while(scrnst[i]==1){i=i+1}
	
	minLen = Inf
	#extract exactly n instances
	for(inst in 1:n){
		#skip to screen turning back on
		while(scrnst[i]==0){i=i+1}
		start = i
		
		#skip to screen turning back off as this indicates the end of the experiment
		
		while(scrnst[i]==1){
			i=i+1
			if(debug && i>length(scrnst))
				browser(condition = i>length(scrnst))
		}
		end = i
		
		#extract instance
		instance =  x[start:end,]
		
		#normalize timestamps. First record is 0.
		starttime = as.numeric(instance[1,1])
		#print(starttime)
		browser()
		for(col in seq(1,ncol(x),2)){
			instance[,col]=as.numeric(instance[,col])-starttime
		}
		
		#keep track of minimal length for normalizing
		if(end-start<minLen){
			print(c(start, end, end-start))
			if(debug)
				browser()
			minLen = end-start
		}
		#print(c(end, start))
		
		l[[inst]] = instance
	}
	if(debug)
		browser()
	for(inst in 1:n){
		if(length(l[[inst]])>minLen){
			l[[inst]] = l[[inst]][1:minLen,]
		}
	}
	
	return(l)
}

getAggregator = function(rollMean = FALSE, rollMeanBefore=TRUE, n = 10){
	
	
	toRet = function(lst){
		#print("bop")
		if(rollMeanBefore && rollMean){
			for(i in 1:length(lst)){
				#print(c("rollin",i))
				lst[[i]] = maCols(lst[[i]], n)
			}
		}
		
		#print("beep")
		sum = add(lst)
		#print("boop")
		if(debug)
			browser()
		#print(paste(dim(sum), length(lst)))
		mean = sum/length(lst)
		lst = c(lst,list(mean))
		if(!rollMeanBefore && rollMean){
			for(i in 1:length(lst)){
				lst[[i]] = maCols(lst[[i]], n)
			}
		}
		return(lst)
	}
	return(toRet)
}

getMean = function(instances, rollmean=FALSE){
	#browser("",debug)
	aggr = getAggregator(rollmean, TRUE, length(instances))
	return(aggr(instances)[[length(instances)+1]])
}

computeAttr = function(instance){
	attr(instance, "means") = colMeans(instance)
	attr(instance, "medians") = apply(instance, 2, median)
	attr(instance, "maximum") = apply(instance, 2, max)
	attr(instance, "minimum") = apply(instance, 2, min)
	#print(attributes(instance))
	return(instance)
}

splitTrace = function(trace){
	nums = trace[2]
	nums = apply(nums, 1, function(x){gsub("[^0-9\\.]","",x)})
	nums = as.numeric(nums)
	touches = sum(is.na(nums))
	ct = 1
	#print(touches)
	ends = matrix(0, touches, 2)
	ends[1,1] = 0
	for(i in 2:length(nums)){
		if(is.na(nums[i])){
			ends[ct,2] = nums[i-1]
			if(ct<touches){
				ct = ct+1
				ends[ct,1] = nums[i+1]
			}	
		}
	}
	ends[touches, 2] = nums[length(nums)]
	return(ends)
}

regularizeData = function(obs1, obs2, metric="CPU Load (Normalized) [%]"){
	
	timeInd = (which(colnames(obs1)==metric)-1)
	#()
	len1 = max(obs1[,timeInd])
	len2 = max(obs2[,timeInd])
	lenMax = max(len1,len2)
	
	if(len2>len1)
		obs1[,timeInd]=obs1[,timeInd]*(len2/len1)
	else
		obs2[,timeInd]=obs2[,timeInd]*(len1/len2)
	
	row1 = 1
	row2 = 1
	#browser()
	while(row1<=nrow(obs1) & row2<=nrow(obs2))
	{
		tval1 = obs1[row1, timeInd]
		tval2 = obs2[row2, timeInd]
		rowsInserted = 0
		numc = ncol(obs1)
		if(tval1-tval2 > 100)
		{
			
			obs2 = insertRow(obs2, row2, numeric(numc))
			for(i in 1:numc){
				obs2[row2, i] = NA
			}
			row2 = row2+2
			
		#	browser()
		#	print(paste("obs2",row1,row2 ))
		}
		else if(tval2-tval1 > 100)
		{
			obs1 = insertRow(obs1, row1, numeric(numc))
			for(i in 1:numc){
				obs1[row1, i] = NA
			}
			row1 = row1+2
		#	print(paste("obs1",row1, row2))
		}
		else
		{
			row1 = row1+1
			row2 = row2+1
		}
		
	}
#	browser()
	return(list(obs1,obs2))
}

obsToTs = function(observation, timeInterval, metric="CPU Load (Normalized) [%]"){
	#observation = getMean(observations)
#	browser()
	#trim data to be univariate about metric and within timeInterval
	timeInd = (which(colnames(observation)==metric)-1)
	
	time = observation[,timeInd]
	interval = which(time>timeInterval[1] & time<timeInterval[2])
	
	obs = observation[min(interval):max(interval), timeInd+1]
	#browser()
	return(tsclean(as.ts(obs,start=1,frequency=10)))
}

compareObs=function(obs1, obs2, timeInterval, ord = NULL, metric = "CPU Load (Normalized) [%]")
{
	#timeInterval in seconds
	timeInterval=timeInterval*1000
	if(regularize){
		regularized = regularizeData(obs1, obs2, metric)
		obs1 = regularized[[1]]
		obs2 = regularized[[2]]
	}
	
	obs1 = obsToTs(obs1, timeInterval, metric)
	plot(obs1, ylim=c(0,60), col=green)
	wait()
	obs2 = obsToTs(obs2, timeInterval, metric)
	lines(obs2, col=blue)
	#browser()
	wait()
	
	deltaTs = obs1-obs2
	plot(deltaTs)
	wait()
	tsdisplay(deltaTs)
	wait()
	#browser()
	if(is.null(ord)){
		adf=(adf.test(deltaTs, alternative="stationary"))
		print(adf)
		#plot(pacf(deltaTs))
		#arima = auto.arima(deltaTs ,max.p=10, max.q=10, include.mean=TRUE, stationary=TRUE, allowdrift=FALSE, stepwise=FALSE)
		arima = auto.arima(deltaTs)
	}
	else{
		temp = deltaTs
		if(ord[2]>0){
			for(i in 1:ord[2])
				temp=diff(temp)
			tsdisplay(temp)
			wait()
		}
		
		adf=(adf.test(temp, alternative="stationary"))
		print(adf)
		#plot(pacf(temp))
		arima = arima(deltaTs,order=ord, include.mean=TRUE, method="ML")
	}
	#print(arima)
	wait()
	#print(arima)
	tsdisplay(residuals(arima))
	#browser()
	print(coeftest(arima))
	return(arima)
}

testForInterval = function(phoneNum, appNum, interval, order=NULL)
{
	obs1 = allDataMeans[[phoneNum]][[appNum[1]]]
	obs2 = allDataMeans[[phoneNum]][[appNum[2]]]
	compareObs(obs1, obs2, interval, order)
}
coeftest = function(x, vcov. = NULL, df = NULL, ...) 
{
	coef0 <- if ("stats4" %in% loadedNamespaces()) 
				stats4::coef
			else coef
	vcov0 <- if ("stats4" %in% loadedNamespaces()) 
				stats4::vcov
			else vcov
	est <- coef0(x)
	if (is.null(vcov.)) 
		se <- vcov0(x)
	else {
		if (is.function(vcov.)) 
			se <- vcov.(x, ...)
		else se <- vcov.
	}
	se <- sqrt(diag(se))
	if (!is.null(names(est)) && !is.null(names(se))) {
		if (length(unique(names(est))) == length(names(est)) && 
				length(unique(names(se))) == length(names(se))) {
			anames <- names(est)[names(est) %in% names(se)]
			est <- est[anames]
			se <- se[anames]
		}
	}
	tval <- as.vector(est)/se
	if (is.null(df)) {
		df <- try(df.residual(x), silent = TRUE)
		if (inherits(df, "try-error")) 
			df <- NULL
	}
	if (is.null(df)) 
		df <- 0
	if (is.finite(df) && df > 0) {
		pval <- 2 * pt(abs(tval), df = df, lower.tail = FALSE)
		cnames <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
		mthd <- "t"
	}
	else {
		pval <- 2 * pnorm(abs(tval), lower.tail = FALSE)
		cnames <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
		mthd <- "z"
	}
	rval <- cbind(est, se, tval, pval)
	#browser()
	colnames(rval) <- cnames
	class(rval) <- "coeftest"
	attr(rval, "method") <- paste(mthd, "test of coefficients")
	return(rval)
}