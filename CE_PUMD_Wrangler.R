#################################################################
#################################################################
# R Script Name: CE_PUMD_Wrangler                               #
# Purpose: Create an easy to use functional based R script that #
#          would facilitate the use of the CE PUMD for R users  #
#                                                               #
#################################################################
#################################################################

###########################
### R Script Parameters ###
###########################

# set the directory in which the data are located
interview_dir <- "~/Downloads/intrvw14"
diary_dir <- "~/Downloads/diary14"
stub_dir <- "~/Downloads/documentation14/documentation14/Programs"

# year of the CE PUMD
year <- 2014

#########################################
### Script Priming Functions and code ###
#########################################

# Function that fixes the directory path string to R's prefered way to read it
# Parameters: directory path to be fixed
renamePath <- function(directory){
  setwd(directory)
  return(getwd())
}

# Fixing the directory path to an R readable format if not already
interview_dir <- renamePath(interview_dir)
diary_dir <- renamePath(diary_dir)
stub_dir <- renamePath(stub_dir)

# Function that installs and librarys as many packages as you want in one sweep
# Parameters: string of package(s) name(s)
library.packages <- function(...){
  packages <- unlist(list(...))
  for (x in 1:length(packages)) {
    if(!(as.character(packages[x]) %in% installed.packages())) {
      cat("Couldn't find the package in installed packages.\n")
      cat("Installing ", packages[x],"...\n", sep="")
      install.packages(as.character(packages[x]), dependencies = TRUE)
    }
    cat("Librarying ",packages[x],".\n", sep = "")
    try(library(as.character(packages[x]), character.only = TRUE))
  }
}

# loading in all of the necessary packages
library.packages("data.table","dplyr","stringr","dtplyr")

# Subsetting the year to be the last two digits
subyear <- str_sub(as.character(year),3,4)

#############################
### Reading Data Function ###
#############################

# Function that reads and returns a combined Data table of the files you want
# Parameters:
## fileName: string, the characters that start the file name of which you want to read into R
## variables: string vector, a vector of variable names you specifically want from a file
## shortentedYear: integer, the last two digits of the PUMD year
readCEData <- function(fileName, variables = NULL, shortenedYear = subyear){
  if(any(grep(fileName,list.files(paste0(interview_dir,"/interview",shortenedYear))))){
    setwd(paste0(interview_dir,"/interview",shortenedYear))
  } else if(any(grep(fileName,list.files(paste0(interview_dir,"/expn",shortenedYear))))){
    setwd(paste0(interview_dir,"/expn",shortenedYear))
  } else if(any(grep(fileName,list.files(paste0(interview_dir,"/para",shortenedYear))))){
    setwd(paste0(interview_dir,"/para",shortenedYear))
  } else if(any(grep(fileName,list.files(diary_dir)))){
    setwd(diary_dir)
  }else{
    return(cat("The file name you have entered does not match any files within the CE data directories you have provided"))
  }
  if(getwd() == paste0(interview_dir,"/para",shortenedYear)){
    if(is.null(variables)){
      dataframe <- fread(paste0(fileName,as.numeric(shortenedYear)-1, shortenedYear,".csv"), na.strings = c(".", "", "NA"))
    } else {
      dataframe <- fread(paste0(fileName,as.numeric(shortenedYear)-1, shortenedYear,".csv"), select = variables, na.strings = c(".", "", "NA"))
    }
  } else if(getwd() == paste0(interview_dir,"/expn",shortenedYear)){
    if(is.null(variables)){
      dataframe <- fread(paste0(fileName,shortenedYear,".csv"), na.strings = c(".", "", "NA"))
    } else {
      dataframe <- fread(paste0(fileName,shortenedYear,".csv"), select = variables, na.strings = c(".", "", "NA"))
    }
  } else if(getwd() == diary_dir){
    if(is.null(variables)){
      dataframe <- bind_rows(
        fread(paste0(fileName,shortenedYear,"1.csv"), na.strings = c(".", "", "NA")),
        fread(paste0(fileName,shortenedYear,"2.csv"), na.strings = c(".", "", "NA")),
        fread(paste0(fileName,shortenedYear,"3.csv"), na.strings = c(".", "", "NA")),
        fread(paste0(fileName,shortenedYear,"4.csv"), na.strings = c(".", "", "NA"))
      ) %>% mutate(NEWID = as.character(NEWID))  
    } else {
      dataframe <- bind_rows(
        fread(paste0(fileName,shortenedYear,"1.csv"), select = variables, na.strings = c(".", "", "NA")),
        fread(paste0(fileName,shortenedYear,"2.csv"), select = variables, na.strings = c(".", "", "NA")),
        fread(paste0(fileName,shortenedYear,"3.csv"), select = variables, na.strings = c(".", "", "NA")),
        fread(paste0(fileName,shortenedYear,"4.csv"), select = variables, na.strings = c(".", "", "NA"))
      ) %>% mutate(NEWID = as.character(NEWID))
    }    
  } else {
    if(is.null(variables)){
      dataframe <- bind_rows(
        fread(paste0(fileName,shortenedYear,"1x.csv"), na.strings = c(".", "", "NA")),
        fread(paste0(fileName,shortenedYear,"2.csv"), na.strings = c(".", "", "NA")),
        fread(paste0(fileName,shortenedYear,"3.csv"), na.strings = c(".", "", "NA")),
        fread(paste0(fileName,shortenedYear,"4.csv"), na.strings = c(".", "", "NA")),
        fread(paste0(fileName,as.numeric(shortenedYear)+1,"1.csv"), na.strings = c(".", "", "NA"))
      ) %>% mutate(NEWID = as.character(NEWID))  
    } else {
      dataframe <- bind_rows(
        fread(paste0(fileName,shortenedYear,"1x.csv"), select = variables, na.strings = c(".", "", "NA")),
        fread(paste0(fileName,shortenedYear,"2.csv"), select = variables, na.strings = c(".", "", "NA")),
        fread(paste0(fileName,shortenedYear,"3.csv"), select = variables, na.strings = c(".", "", "NA")),
        fread(paste0(fileName,shortenedYear,"4.csv"), select = variables, na.strings = c(".", "", "NA")),
        fread(paste0(fileName,as.numeric(shortenedYear)+1,"1.csv"), select = variables, na.strings = c(".", "", "NA"))
      ) %>% mutate(NEWID = as.character(NEWID))
    }
  }
  return(dataframe)
}

####################################################
### Converting the interview and diary stub file ###
############ into .csv tables  function ############
####################################################

convertStubFiles <- function(dir = stub_dir){
  setwd(dir)
  
  for(s in c("IStub", "DStub", "IntStub")) {
    
    # Read in the original stub file, keep only the first line of every entry,
    # and substitute 2 tabs for 7 spaces
    sp <- paste0(s, year, ".txt")
    sf <- readLines(sp)
    sf <- gsub("\t\t" , "       " , sf)
    
    # Put the stub file in a temporary file
    tf <- tempfile()
    writeLines(sf, tf)
    
    # Read in the cleaner version of the stub file in fixed-width format
    stub <- read.fwf(
      tf, width = c(1, -2, 1, -2, 60, -3, 6, -7, 1, -5, 7),
      col.names = c("type", "level", "title", "UCC", "survey", "group")
    )
    
    # Convert the stub file to a data frame and strip whitespace
    stub[, names(Filter(is.factor, stub))] <-
      data.frame(lapply(stub[, names(Filter(is.factor, stub))], as.character),
                 stringsAsFactors = FALSE)
    
    trim.leading <- function(x) sub("^\\s+", "", x)
    trim.trailing <- function(x) sub("\\s+$", "", x)
    
    stub <- data.frame(lapply(stub, trim.leading), stringsAsFactors = FALSE)
    stub <- data.frame(lapply(stub, trim.trailing), stringsAsFactors = FALSE)
    
    # Concatenate the titles that run beyond 1 line into their respective first
    # lines
    for (i in seq(length(stub$type))) {
      if (stub$type[i] %in% "2") {
        l1_row <- max(which(stub$type[1:i] %in% "1"))
        stub$title[l1_row] <- paste(stub$title[l1_row], stub$title[i])
      }
    }
    
    stub <- stub[stub$type %in% c("1", "*"), ]
    
    # Make all the variable names lower character
    names(stub) <- tolower(names(stub))
    
    # copy the stub file into the global environment
    assign(s, stub, envir = .GlobalEnv)
  }
}

##################################################
### Subsetting to chosen Demographic Functions ###
##################################################

# Function that returns a dataframe subset by age
# Parameters:
## dataframe: dataframe, the data frame of bls microdata you wish to subset
## minAge: integer, the lower value of the age range you wish to subset
## maxAge: integer, the higher value of the age range you wish to subset
## reference: boolean, determing if you wish to subset for NEWID by reference person only
### if false the function will subset the dataframe to CUs that contain anyone in the age Range
subsetByAge <- function(dataframe, minAge, maxAge, reference = TRUE){
  dataframe <- as.data.frame(dataframe)
  rownames(dataframe) <- NULL
  ageRange <- seq(minAge,maxAge)
  if(reference){
    #paying attention to the reference person only
    dataframe <- dataframe[which(dataframe$AGE_REF %in% ageRange),]
  }else{
    #paying attention to every member of the CU
    dataframe <- dataframe[which(dataframe$AGE_REF %in% ageRange | dataframe$AGE2 %in% ageRange),]
  }
  return(dataframe)
}

# Function that returns a vector of income classes according to your incomeBreakpoints
# Parameters:
## dataframe: dataframe, the data frame that contains the income classes that you wish to change
## incomeBreakpoints: double, a vector of doubles that are the income breakpoints you wish to create income classes from
## incomeClassColumnName: string, the name of the Income class column in the dataframe
## specilaIBTColumnName: string, the name of the income before taxes column if not one of the standard column names
changeIncomeClasses <- function(dataframe, incomeBreakpoints, incomeClassColumnName = "INCLASS", specialIBTColumnName = NULL){
  dataframe <- as.data.frame(dataframe)
  rownames(dataframe) <- NULL
  incomeBreakpoints <- c(-Inf,incomeBreakpoints,Inf)
  incomeBreakpoints <- sort(unique(incomeBreakpoints))
  if(is.null(specialIBTColumnName)){
    if("FINCBTXM" %in% colnames(dataframe)){
      for(x in 1:(length(incomeBreakpoints)-1)){
        dataframe[which(dataframe$FINCBTXM >= incomeBreakpoints[x] & dataframe$FINCBTXM < incomeBreakpoints[x+1]), incomeClassColumnName] <- x
      }
    } else if("FINCBEFX" %in% colnames(dataframe)){
      for(x in 1:(length(incomeBreakpoints)-1)){
        dataframe[which(dataframe$FINCBEFX >= incomeBreakpoints[x] & dataframe$FINCBEFX < incomeBreakpoints[x+1]), incomeClassColumnName] <- x
      }
    } else {
      return(cat("Can not find the Income before taxes column. Try entering a special column name or using a different dataframe.\n"))
    }
  } else {
    if(specialIBTColumnName %in% colnames(dataframe)){
      for(x in 1:(length(incomeBreakpoints)-1)){
        dataframe[which(dataframe[,columnName] >= incomeBreakpoints[x] & dataframe[,columnName] < incomeBreakpoints[x+1]), incomeClassColumnName] <- x
      }
    } else {
      return(cat("Could not find the specialIBTColumnName you entered. Try using the default column name or a different dataframe.\n"))
    }
  }
  return(dataframe)
}

# Function that returns a dataframe of observations that are only within your chosen age range
# Parameters:
## dataframe: data frame, the data frame of which you want to subset
## minIncome: double, the low end of the age range you wish to subset by
## maxIncome: double, the high end of the age range you wish to subset by
## specilColumnName: string, the name of the column that contians income for the data frame
subsetByIncome <- function(dataframe, minIncome, maxIncome, specialColumnName = NULL){
  dataframe <- as.data.frame(dataframe)
  rownames(dataframe) <- NULL
  # Determining if the there is a special ColumnName
  if(!is.null(specialColumnName)){
    if(!(specialColumnName %in% colnames(dataframe))){
      cat("The special column name is not a column name in the data frame.\n")
    } else {
      dataframe <- dataframe[which(dataframe[,specialColumnName] >= minIncome & dataframe[,specialColumnName] < maxIncome)]
    }
  } else {
    if("FINCBEFX" %in% colnames(dataframe)){    # checking if the dataframe is a diary based 
      dataframe <- dataframe[which(dataframe$FINCBEFX >= minIncome & dataframe$FINCBEFX < maxIncome),]
    }else if("FINCBTXM" %in% colnames(dataframe)){    # checking if the dataframe is interview based
      dataframe <- dataframe[which(dataframe$FINCBTXM >= minIncome & dataframe$FINCBTXM < maxIncome),]
    } else{
      cat("The standard column names were not found please input a specialColumnName next time.\n")
    }
  }
  return(dataframe)
}

# Function that returns a data frame of which the CU size is equal to the sizeOfCU
# Parameters:
## dataframe: data frame, the data frameof which you want to subset
## sizeOfCU: vector, a vector containing integers of family size that you wish to subset
## specialColumnName: string, the name of the column that contains the size of the cu variable
subsetByCUSize <- function(dataframe, sizeOfCU, specialColumnName = NULL){
  dataframe <- as.data.frame(dataframe)
  rownames(dataframe) <- NULL
  if(!is.null(specialColumnName)){
    if(!(specialColumnName %in% colnames(dataframe))){
      cat("The special column name is not a column name in the data frame.\n")
    } else {
      dataframe <- dataframe[which(dataframe[,specialColumnName] %in% sizeOfCU),]
    }
  } else {
    if ("FAM_SIZE" %in% colnames(dataframe)) {
      dataframe <- dataframe[which(dataframe$FAM_SIZE %in% sizeOfCU), ]
    } else{
      cat("The standard column names were not found please input a specialColumnName next time.\n")
    }
  }
  return(dataframe)
}

# Function that returns a data frame of which the Housing Region is Urban or Rural
# Parameters:
## dataframe: data frame, the data frameof which you want to subset
## housingType: integer, either 1 or 2 based on the housing type you want 
### 1 = Urban
### 2 = Rural
subsetByRegionType <- function(dataframe, housingType, specialColumnName = NULL){
  dataframe <- as.data.frame(dataframe)
  rownames(dataframe) <-NULL
  if(!is.null(specialColumnName)){
    if(!(specialColumnName %in% colnames(dataframe))){
      cat("The special column name is not a column name in the data frame.\n")
    } else {
      dataframe <- dataframe[which(dataframe[,specialColumnName] == housingType),]
    }
  } else {
    if(!("BLS_URBN" %in% colnames(dataframe))){
      cat("The standard column names were not found please input a specialColumnName next time.\n")
    } else {
      dataframe <- dataframe[which(dataframe$BLS_URBN == housingType),]
    }
  }
  return(dataframe)
}

# Function that returns a data frame of which the Housing Region is Urban or Rural
# Parameters:
## dataframe: data frame, the data frameof which you want to subset
## specialColumnName: string, the name of the column that contains the size of the cu variable
## housingRegion: integer vector, based on the region type you want 
### 1 = Northeast
### 2 = Midwest
### 3 = South
### 4 = West
subsetByHousingRegion <- function(dataframe, housingRegion, specialColumnName = NULL){
  dataframe <- as.data.frame(dataframe)
  rownames(dataframe) <-NULL
  if(!is.null(specialColumnName)){
    if(!(specialColumnName %in% colnames(dataframe))){
      cat("The special column name is not a column name in the data frame.\n")
    } else {
      dataframe <- dataframe[which(dataframe[,specialColumnName] %in% housingRegion),]
    }
  } else {
    if(!("REGION" %in% colnames(dataframe))){
      cat("The standard column names were not found please input a specialColumnName next time.\n")
    } else {
      dataframe <- dataframe[which(dataframe$REGION %in% housingRegion),]
    }
  }
  return(dataframe)
}

# Function that returns a dataframe subset by earning composition
# Parameters
## dataframe: data frame, the data frame of which you want to subset
## specialColumnName: string, the name of the column that contains the size of the cu variable
## earningComp: integer vector, based on the earning composition you want
### 1 = Reference person only
### 2 = Reference person and spouse
### 3 = Reference person, spouse and others
### 4 = Reference person and others
### 5 = Spouse only
### 6 = Spouse and others
### 7 = Others only
subsetByEarningComp <- function(dataframe, earningComp, specialColumnName = NULL){
  dataframe <- as.data.frame(dataframe)
  rownames(dataframe) <-NULL
  if(!is.null(specialColumnName)){
    if(!(specialColumnName %in% colnames(dataframe))){
      cat("The special column name is not a column name in the data frame.\n")
    } else {
      dataframe <- dataframe[which(dataframe[,specialColumnName] %in% earningComp),]
    }
  } else {
    if(!("EARNCOMP" %in% colnames(dataframe))){
      cat("The standard column names were not found please input a specialColumnName next time.\n")
    } else {
      dataframe <- dataframe[which(dataframe$EARNCOMP %in% earningComp),]
    }
  }
  return(dataframe)
}

# Function that returns a dataframe subset by Race
# Parameters
## dataframe: data frame, the data frame of which you want to subset
## reference: boolean, determines if you want to subset for the refernce person's race only
## race: integer vector, based on the races you want to subset for 
### 1 White
### 2 African American, or Black
### 3 American Indian, or Alaskan Native
### 4 Asian
### 5 Native Hawaiian or Other Pacific Islander
### 6 Multi-race
subsetByRace <- function(dataframe, race, reference = TRUE) {
  dataframe <- as.data.frame(dataframe)
  rownames(dataframe) <- NULL
  if (reference) {
    if (!("REF_RACE" %in% colnames(dataframe))) {
      cat(
        "The standard column name were not found please make sure you have a \"REF_RACE\" column in the dataframe.\n"
      )
    } else {
      dataframe <- dataframe[which(dataframe[,"REF_RACE"] %in% race),]
    }
  } else {
    if (!("REF_RACE" %in% colnames(dataframe)) | !("RACE2" %in% colnames(dataframe))) {
      cat(
        "The standard column names were not found please make sure you have a \"HISP_REF\" column and a \"HISP2\" column in the dataframe.\n"
      )
    } else{
      dataframe <- dataframe[which(dataframe[,"REF_RACE"] %in% race | dataframe[,"RACE2"] %in% race),]
    }
  }
  return(dataframe)
}

# Function that returns a data frame subset for being Hispanic or not
# Parameters
## dataframe: data frame, the data frame of which you want to subset
## Hispanic: boolean, determines if you want to subset for Hispanic people or not
## Reference: boolean, determines if you want to subset my Hispanic for the reference person only
subsetByHispanic <- function(dataframe, Hispanic = TRUE, reference = TRUE){
  dataframe <- as.data.frame(dataframe)
  rownames(dataframe) <- NULL
  if (reference) {
    if (!("HISP_REF" %in% colnames(dataframe))) {
      cat(
        "The standard column name were not found please make sure you have a \"HISP_REF\" column in the dataframe.\n"
      )
    } else {
      if (Hispanic) {
        dataframe <- dataframe[which(dataframe[, "HISP_REF"] == 1), ]
      } else{
        dataframe <- dataframe[which(dataframe[, "HISP_REF"] == 2), ]
      }
    }
  } else {
    if (!("HISP_REF" %in% colnames(dataframe)) | !("HISP2" %in% colnames(dataframe))) {
      cat(
        "The standard column names were not found please make sure you have a \"HISP_REF\" column and a \"HISP2\" column in the dataframe.\n"
      )
    } else{
      if (Hispanic) {
        dataframe <-
          dataframe[which(dataframe[, "HISP_REF"] == 1 | dataframe[, "HISP2"] == 1), ]
      } else{
        dataframe <-
          dataframe[which(dataframe[, "HISP_REF"] == 0 | dataframe[, "HISP2"] == 2), ]
      }
    }
  }
  return(dataframe)
}

# Function that returns a data frame subset by PSU
# Parameters
## dataframe: data frame, the data frame of which you want to subset
## psuCode: integer vector, the code associated with the primary sampling unit/units *see diary or interveiw dictionary for codes*
## specialColumnName: string, the name of the column that contains the PSU variable
subsetByPSU <- function(dataframe, psuCode, specialColumnName = NULL){
  dataframe <- as.data.frame(dataframe)
  rownames(dataframe) <-NULL
  if(!is.null(specialColumnName)){
    if(!(specialColumnName %in% colnames(dataframe))){
      cat("The special column name is not a column name in the data frame.\n")
    } else {
      dataframe <- dataframe[which(dataframe[,specialColumnName] %in% psuCode),]
    }
  } else {
    if(!("PSU" %in% colnames(dataframe))){
      cat("The standard column names were not found please input a specialColumnName next time.\n")
    } else {
      dataframe <- dataframe[which(dataframe$PSU %in% psuCode),]
    }
  }
  return(dataframe)
}

# Function that returns a data frame subset by Why the reference person did not work during the past 12 months
# Parameters
## dataframe: data frame, the data frame of which you want to subset
## whoNoWork: vector, the corresponding number fo rthe reason the reference person did not work
### 1 Retired
### 2 Taking care of home/CU
### 3 Going to school
### 4 Ill, disabled, unable to work
### 5 Unable to find work
### 6 Doing something else
### NA The reference person did work during the past 12 months
subsetByWhyNoWork <- function(dataframe, whyNoWork){
  dataframe <- as.data.frame(dataframe)
  rownames(dataframe) <- NULL
  if ("INCNONW1" %in% colnames(dataframe)) {
    dataframe <- dataframe[which(dataframe$INCNONW1 %in% whyNoWork), ]
  } else if("WHYNWRK1" %in% colnames(dataframe)){
    dataframe <- dataframe[which(dataframe$WHYNWRK1%in% whyNoWork), ]
  } else {
    return(cat("You are missing the correctly named why no work column."))
  }
  return(dataframe)
}

# Function that returns a data frame subset by the number of vehicles a CU owns
# Parameters
## dataframe: data frame, the data frame of which you want to subset
## numberOfVehicles: integer vector, the number of vehicles you wish to subset for
## specialColumnName: string, the name of the column that contains the Number of Vehicles variable
subsetByNumVehichles <- function(dataframe, numberOfVehicles, specialColumnName = NULL){
  dataframe <- as.data.frame(dataframe)
  rownames(dataframe) <-NULL
  if(!is.null(specialColumnName)){
    if(!(specialColumnName %in% colnames(dataframe))){
      cat("The special column name is not a column name in the data frame.\n")
    } else {
      dataframe <- dataframe[which(dataframe[,specialColumnName] %in% numberOfVehicles),]
    }
  } else {
    if(!("VEHQ" %in% colnames(dataframe))){
      cat("The standard column names were not found please input a specialColumnName next time.\n")
    } else {
      dataframe <- dataframe[which(dataframe$VEHQ %in% numberOfVehicles),]
    }
  }
  return(dataframe)
}

# Function that returns a data frame subset by the housing tenure
# Parameters
## dataframe: data frame, the data frame of which you want to subset
## tenure: integer vector, the housing tenure type(s) you wish to subset for
### 1 Owned with mortgage
### 2 Owned without mortgage
### 3 Owned mortgage not reported
### 4 Rented
### 5 Occupied without payment of cash rent
### 6 Student housing
## specialColumnName: string, the name of the column that contains the Number of Vehicles variable
subsetByHousingTenure <- function(dataframe, tenure, specialColumnName = NULL){
  dataframe <- as.data.frame(dataframe)
  rownames(dataframe) <-NULL
  if(!is.null(specialColumnName)){
    if(!(specialColumnName %in% colnames(dataframe))){
      cat("The special column name is not a column name in the data frame.\n")
    } else {
      dataframe <- dataframe[which(dataframe[,specialColumnName] %in% tenure),]
    }
  } else {
    if(!("CUTENURE" %in% colnames(dataframe))){
      cat("The standard column names were not found please input a specialColumnName next time.\n")
    } else {
      dataframe <- dataframe[which(dataframe$CUTENURE %in% tenure),]
    }
  }
  return(dataframe)
}

# Function that returns a data frame subset by marital status
# Parameters
## dataframe: data frame, the data frame of which you want to subset
## status: integer vector, marital status of reference person
### 1 Married
### 2 Widowed
### 3 Divorced
### 4 Separated
### 5 Never married
## specialColumnName: string, the name of the column that contains the Marital Status variable
subsetByMaritalStatus <- function(dataframe, status, specialColumnName = NULL){
  dataframe <- as.data.frame(dataframe)
  rownames(dataframe) <-NULL
  if(!is.null(specialColumnName)){
    if(!(specialColumnName %in% colnames(dataframe))){
      cat("The special column name is not a column name in the data frame.\n")
    } else {
      dataframe <- dataframe[which(dataframe[,specialColumnName] %in% status),]
    }
  } else {
    if(!("MARITAL1" %in% colnames(dataframe))){
      cat("The standard column names were not found please input a specialColumnName next time.\n")
    } else {
      dataframe <- dataframe[which(dataframe$MARITAL1 %in% status),]
    }
  }
  return(dataframe)
}
