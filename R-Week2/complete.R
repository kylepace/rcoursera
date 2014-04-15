getFileString <- function(fileNo) {
	asChar <- as.character(fileNo)
	charCount <- nchar(asChar)			
	if (charCount == 1) {
		paste("00", as.character(fileNo), ".csv", sep = "")
	} else if (charCount == 2) {
		paste("0", as.character(fileNo), ".csv", sep = "")
	} else {
		paste(as.character(fileNo), ".csv", sep = "")
	}
}

complete <- function(directory, id = 1:332) {
	for (file in id) {
		fileName <- paste0(directory, '/', getFileString(file))
		dt <- read.csv(fileName)
		completes <- dt[complete.cases(dt), ]
		if (exists("totalNobs")) {
			totalNobs <- rbind(totalNobs, data.frame(id = file, nobs = nrow(completes)))
		} else {
			totalNobs <-data.frame(id = file, nobs = nrow(completes))
		}
	}
	
	totalNobs
	## Return a data frame of the form:
	## id nobs
	## 1  117
	## 2  1041
	## ...
	## where 'id' is the monitor ID number and 'nobs' is the
	## number of complete cases
}