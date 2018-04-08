best<-function(state,disease){
	# read outcome data
	outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	
	# hospital name is column 2
	# state is 7
	# I know the columns 
	# 1.Heart Attack motality- 11 
	# 2.Number of heart attack parients -15
	
	#Check if the state is valid, process outcome and have all the states in a vector
	#Check the state entered against this vector
	
	# Have the motality reasons in a vector
	# Check the motality reason  entered against this vector
	
	# depending on the motality reason, locate the correct column
	# extract the data to a data frame 
	# columns to be extracted, motality rate + hospital + state
	# Extract all the rows for the state
	# within this subdata frame, extract the rows with the minumum motality rates
	# sort this data struncture by alpha
	# return this data structure

####################################
	
	# extract the states to a data stucture
	all_states <-outcome[,7]
	
	# set the motality reasons
	motality_reasons<-c('Heart Attack','Heart Failure','Pneumonia')
	#create the list of unique states
	unique_states<-unique(all_states)
	is_state_valid<-state%in% unique_states
	
	is_motality_reason_valid<-disease%in%motality_reasons
	
	if(!is_state_valid) stop('Invalid state')
	
	if(!is_motality_reason_valid) stop('Invalid outcome')
	
	# depending on the outcome select the column
	# Column names 
	heart_attack<-'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'
	heart_failure<-'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
	Pneumonia<-'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
	hospital_name<-'Hospital.Name'
	stateC<-'State'
	df2<-outcome[,c(hospital_name, stateC, heart_attack, heart_failure, Pneumonia)]
	
	if(disease=='Heart Attack')
	{
		df3<-df2[,c('Hospital.Name', 'State', 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack')]
		df3_state<- df3[df3$State==state,]
		df3_num <- df3_state[!is.na(as.numeric(as.character(df3_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))),]
		df3_result<-df3_num[which(df3_num$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == min(df3_num$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)), ]
		df3_result
	
	} else if (disease=='Heart Failure')
	{
		df3<-df2[,c('Hospital.Name', 'State', 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure')]
		df3_state<- df3[df3$State==state,]
		df3_num <- df3_state[!is.na(as.numeric(as.character(df3_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))),]
		df3_result<-df3_num[which(df3_num$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == min(df3_num$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)), ]
	 	df3_result
		
	}else if (disease =='Pneumonia')
	{
		df3<-df2[,c('Hospital.Name', 'State', 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia')]
		df3_state<- df3[df3$State==state,]
		df3_num <- df3_state[!is.na(as.numeric(as.character(df3_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))),]
		df3_result<-df3_num[which(df3_num$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == min(df3_num$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)), ]
	 	df3_result
		
	}
	 
}