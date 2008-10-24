columnMean <- function(inputData, column) {
	mean(inputData[column])
}

rowMean <- function(inputData, row) {

	columns = length(inputData) - 1
        total <- 0

	for( i in 2:(columns+1)) {
		total <- total + sum(inputData[row, i])
	}

	total / columns
}

squareMean <- function(inputData) {

	columns = length(inputData) - 1
	rows <- length(inputData[,1])	
	total <- 0	
 
	for( i in 2:(columns+1)) {
		total <- total + sum(inputData[i])
	}
	
	total / (rows*columns)

}

columnEffect <- function(inputData, column) {
	sm <- squareMean(inputdata)
	cm <- columnMean(inputData, column)

	cm - sm	
}

rowEffect <- function(inputData, row) {
	sm <- squareMean(inputdata)
	rm <- rowMean(inputData, row)

	cm - rm
}





