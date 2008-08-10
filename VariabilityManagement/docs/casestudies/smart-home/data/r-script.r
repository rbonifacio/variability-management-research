sqr <- function (x) {
  x*x
}

sigma2 <- function(v) {
 sum <- 0
 for (i in 1:length(v)) {
  sum <- sum + sqr ( (v[i] - mean(v)) )
 }
 sum / (length (v) -1)
}

sigma <- function (v) {
 sqrt (sigma2 (v))
}

pnormal <- function (n,s,y0)
 (y0-n)/s
 

tanglingScenario <- function(inputData, scenario) {
 dist <- inputData[scenario]
 result <- 0
 for (i in 1:length(dist[[1]])) {
    if( dist[i,1] > 0) result <- result + 1
 }
 result
} 


featureIndex <- function(inputData, feature) {
	nFeatures <- length(inputData[[1]])
	fIndex <- 0
	for( i in 1:nFeatures ) {
		if( inputData[i,1] == feature ) fIndex <- i
	}
	fIndex
}

dedication <- function(inputData, feature, scenario) {
	stepsOnScenario <- sum(inputData[scenario])
	fIndex <- featureIndex(inputData, feature)
	stepsOnScenarioRelatedToFeature <- inputData[scenario][fIndex,1]
	
	(stepsOnScenarioRelatedToFeature / stepsOnScenario)
}

concentration <- function(inputData, feature, scenario) {
	nFeatures <- length(inputData[[1]])
	nScenarios <- length(inputData)
	fIndex <- featureIndex(inputData, feature)
	
	stepsRelatedToFeature <- 0 
	for( j in 2:nScenarios ) {
		stepsRelatedToFeature <- stepsRelatedToFeature + inputData[fIndex, j]	}
	stepsOnScenario <- inputData[scenario][fIndex,1]
	result <- stepsOnScenario / stepsRelatedToFeature
}

degreeOfScattering <- function(inputData, feature) {
	nFeatures <- length(inputData[[1]])
	nScenarios <- length(inputData) - 1
	fIndex <- featureIndex(inputData, feature)	
	
	# total number of steps related to a feature
	stepsRelatedToFeature <- 0 
	for( j in 2:(nScenarios + 1) ) {
		stepsRelatedToFeature <- stepsRelatedToFeature + inputData[fIndex, j]	}
	
	#
	sum <- 0
	for ( k in 2:(nScenarios+1) ) {
		concentration <- (inputData[fIndex, k] / stepsRelatedToFeature)
		sum <- sum + sqr (concentration - (1/nScenarios))
	}
	
	1 - (nScenarios * sum / (nScenarios-1))
	
}

degreeOfFoccus <- function(inputData, scenario)  {
	s <- length(inputData[[1]])
	stepsOfScenario <- sum(inputData[scenario])
	
	sum <- 0
	
	for( i in 1:s ) {
		dedication <- ( inputData[scenario][i,1] / stepsOfScenario )
		sum <- sum + sqr(dedication - 1/s)		
	}
	s * sum / (s-1)
}



