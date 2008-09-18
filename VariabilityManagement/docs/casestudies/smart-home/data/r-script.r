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
 

featureIndex <- function(inputData, feature) {
	nFeatures <- length(inputData[[1]])
	fIndex <- 0
	for( i in 1:nFeatures ) {
		if( inputData[i,1] == feature ) fIndex <- i
	}
	fIndex
}

featureDiffusionOverStep <- function(inputData, feature) {
	fIndex <- featureIndex(inputData, feature)
    nScenarios <- length(inputData)-1
 
    result <- 0
    
    for( i in 2:(nScenarios+1)) {
    	result <- result + inputData[fIndex,i]
    } 
 	result
}

totalFeatureDiffusionOverStep <- function(inputData) {
	nFeatures <- length(inputData[[1]])
	fds <- c(1:nFeatures)

	for(i in 1:nFeatures) {
		featureName <- inputData[i, 1]
		fds[i] <- featureDiffusionOverStep (inputData, featureName)
	}   
	fds
}

featureDiffusionOverScenario <- function(inputData, feature) {
	fIndex <- featureIndex(inputData, feature)
      nScenarios <- length(inputData)-1
 
      result <- 0
      for( i in 2:(nScenarios+1)) {
          if( inputData[fIndex,i] > 0) result <- result + 1
      } 
 
      result
}

totalFeatureDiffusionOverScenario <- function(inputData) {
	nFeatures <- length(inputData[[1]])
	fds <- c(1:nFeatures)

	for(i in 1:nFeatures) {
		featureName <- inputData[i, 1]
		fds[i] <- featureDiffusionOverScenario (inputData, featureName)
	}   
	fds
}

numberOfFeaturesPerScenario <- function(inputData, scenario) {
 	dist <- inputData[scenario]
	 result <- 0
	 for (i in 1:length(dist[[1]])) {
    		if( dist[i,1] > 0) result <- result + 1
	 }
	 result
} 

totalNumberOfFeaturesPerScenarios <- function(inputData) {
	numberOfScenarios <- length(inputData)-1
	nameOfScenarios <- names(inputData)
	nfs <- c(1:(numberOfScenarios))
	
	for( i in 2:(numberOfScenarios+1) ) {
		scenarioName <- nameOfScenarios[i]
		nfs[i-1] <- numberOfFeaturesPerScenario(inputData, scenarioName)		
	} 	
	nfs
}


stepsRelatedToFeature <- function(inputData, feature) {
	nScenarios <- length(inputData)
	fIndex <- featureIndex(inputData, feature)

	# total number of steps related to a feature
	stepsRelatedToFeature <- 0 
	for( j in 2:(nScenarios) ) {
		stepsRelatedToFeature <- stepsRelatedToFeature + inputData[fIndex, j]	
	}

	stepsRelatedToFeature
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
	
	stepsRelatedToFeature <- stepsRelatedToFeature(inputData, feature) 

	stepsOnScenario <- inputData[scenario][fIndex,1]
	result <- stepsOnScenario / stepsRelatedToFeature
}

degreeOfScattering <- function(inputData, feature) {
	nScenarios <- length(inputData) - 1
	fIndex <- featureIndex(inputData, feature)	
	
	# total number of steps related to a feature
	nSteps <- stepsRelatedToFeature(inputData, feature)
	
	#
	sum <- 0.000
	for ( k in 1:(nScenarios) ) {
		c <- (inputData[fIndex, (k+1)] / nSteps)
		sum <- sum + sqr (c - (1/nScenarios))
	}
	1 - ((nScenarios * sum) / (nScenarios-1))	
}

totalDegreeOfScattering <- function(inputData) {
	nFeatures <- length(inputData[[1]])
	dos <- c(1:nFeatures)

	for(i in 1:nFeatures) {
		featureName <- inputData[i, 1]
		dos[i] <- degreeOfScattering(inputData, featureName)
	}   
	dos
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

avarageDegreeOfFocus <- function(inputData) {
	numberOfScenarios <- length(inputData)
	nameOfScenarios <- names(inputData)
	sum <- 0
	
	for( i in 2:numberOfScenarios ) {
		scenarioName <- nameOfScenarios[i]
		sum <- sum + degreeOfFoccus(inputData, scenarioName)		
	} 	
	(sum / numberOfScenarios)
}

totalDegreeOfFocus <- function(inputData) {
	numberOfScenarios <- length(inputData)-1
	nameOfScenarios <- names(inputData)
	dof <- c(1:(numberOfScenarios))
	
	for( i in 2:(numberOfScenarios+1) ) {
		scenarioName <- nameOfScenarios[i]
		dof[i-1] <- degreeOfFoccus(inputData, scenarioName)		
	} 	
	dof
}



