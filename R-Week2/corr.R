corr <- function(directory, threshold = 0) {
	for (file in list.files(directory)) {
		dt <- read.csv(paste0(directory, '/', file))
		completes <- dt[complete.cases(dt), c(2, 3)]
		if (nrow(completes) > threshold) {
			if (exists("cors")) {
				cors <- c(cors, cor(completes[["nitrate"]], y = completes[["sulfate"]]))
			} else {
				cors <- cor(y = completes[["nitrate"]], x = completes[["sulfate"]])
			}
		}
	}
	
	if (!exists("cors")) {
		cors <- c()
	}
	cors
}