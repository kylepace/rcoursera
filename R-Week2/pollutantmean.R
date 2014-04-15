pollutantmean <- function(directory, pollutant, id = 1:332) {
	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files
	for (file in list.files(directory)) {
		fileName <- paste0(directory, '/', file)
		if (exists("monitors")) {
			monitors <- rbindlist(list(monitors, read.csv(fileName)))
		} else {
			monitors <- read.csv(fileName)
		}
	}

	onlyPollutants <- subset(monitors, , select = c(pollutant, "ID"))
	noNa <- onlyPollutants[complete.cases(onlyPollutants),]
	subSetted <- subset(noNa, ID %in% id, select = pollutant)
	mean(subSetted[[pollutant]])
}