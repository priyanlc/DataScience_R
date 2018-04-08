
pollutantmean<-function(directory=getwd(),pollutant,id=1:332){

	filenames <- sprintf("%03d.csv", id)
	#print(class(filenames))
	#print(filenames)

	filenames <- paste(directory, filenames, sep="/")

	#print(class(filenames))

	

	ldf <- lapply(filenames, read.csv)
	print(class(ldf))

	df=ldply(ldf)
	print(class(df))
	print(unclass(df))

	# df is your list of data.frames
	mean(df[, pollutant], na.rm = TRUE)	

}

