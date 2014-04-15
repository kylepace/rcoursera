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

pollutantmean <- function(directory, pollutant, id = 1:332) {
	for (file in id) {
		fileName <- paste0(directory, '/', getFileString(file))
		if (exists("monitors")) {
			monitors <- rbindlist(list(monitors, read.csv(fileName)))
		} else {
			monitors <- read.csv(fileName)
		}
	}

	onlyPollutants <- subset(monitors, , select = c(pollutant))
	mean(onlyPollutants[complete.cases(onlyPollutants), ])
}