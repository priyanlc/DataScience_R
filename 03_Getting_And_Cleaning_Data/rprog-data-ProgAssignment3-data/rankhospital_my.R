rankhospital <- function(state, disease, num = "best") { 
	
	## Read outcome data 
	## Check that state and outcome are valid 
	## Return hospital name in that state with the given rank 
	## 30-day death rate 
	
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	
	# extract the states to a data stucture
	all_states <-data[,7]
	
	# set the motality reasons
	motality_reasons<-c('Heart Attack','Heart Failure','Pneumonia')
	#create the list of unique states
	unique_states<-unique(all_states)
	is_state_valid<-state%in% unique_states
	
	is_motality_reason_valid<-disease%in%motality_reasons
	
	if(!is_state_valid) stop('Invalid state')
	
	if(!is_motality_reason_valid) stop('Invalid outcome')
	
	heart_attack<-'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'
	heart_failure<-'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
	Pneumonia<-'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
	hospital_name<-'Hospital.Name'
	stateC<-'State'
	
	df2<-data[,c(hospital_name, stateC, heart_attack, heart_failure, Pneumonia)]
	
	if(disease=='Heart Attack')
	{
		df3<-df2[,c('Hospital.Name', 'State', 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack')]
		df3_state<- df3[df3$State==state,]
		df3_num <- df3_state[!is.na(as.numeric(as.character(df3_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))),]
	    
	    if (num=='best')
	    {
	    	num_rows=1
	    }else if (num=='worst')
	    {
	    	num_rows=nrow(df3_num)
	    	
	    }else if ( f <- function(x) is.numeric(x) & !is.na(x))
	    {
	    	num_rows= num
	    }
	    
	    
	    df3_result =df3_num[with(df3_num, ave(-heart_attack, hospital_name, FUN = order)) %in% c(1, 2), ]
		 df3_result
	
	} else if (disease=='Heart Failure')
	{
		df3<-df2[,c('Hospital.Name', 'State', 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure')]
		df3_state<- df3[df3$State==state,]
		df3_num <- df3_state[!is.na(as.numeric(as.character(df3_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))),]
		df3_result<-df3_num[which(df3_num$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == min(df3_num$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)), ]
	 	df3_result
		
	}

	
	}
	