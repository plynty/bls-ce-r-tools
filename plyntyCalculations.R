# Plynty Calculations
rm(list=ls())

##################
### Parameters ###
##################

# The directory in which you are working
my_dir <- "~/bls-ce-r-tools/"

year <- 2014

# Age Range
minAge <- 55
maxAge <- 64

# Income brackets
incomeBrackets <- c(5000,15000,53000,77000,86000,120000,180000,240000)

changeString <- "Age1Both"

#####################################
### Reading in the necessary Data ###
#####################################

setwd(my_dir)

# Loading functions from CE_PUMD_Wrangler.R
source("CE_PUMD_Wrangler.R")

fmly <- readCEData("fmli")

mtab <- readCEData("mtbi")

rownames(fmly) <- NULL
rownames(mtab) <- NULL

convertStubFiles()

###########################
### Subsetting the Data ###
###########################

# subsetting for the age bracket
fmly <- subsetByAge(fmly, minAge = minAge, maxAge = maxAge, reference = TRUE)

# Converting Inclass variable to integer
fmly$INCLASS <- as.integer(fmly$INCLASS)

# changing the income classes to fit our income brackets
fmly <- changeIncomeClasses(fmly, incomeBreakpoints = incomeBrackets)

######################################
### Creating the plynty categories ###
######################################

# Function that returns a vector of UCCs that are containted within the abbreviation level
categoricalUCCRollUp <- function(stubfile, abbreviations, ignoreUCCs = NULL){
  rownames(stubfile) <- NULL
  stubfile[which(stubfile$level == "*"),"level"] <- 0
  uccs <- vector()
  for(x in 1:length(abbreviations)){
    currentAbbreviation <- abbreviations[x]
    startingRow <- which(stubfile$ucc == currentAbbreviation)
    if(length(startingRow)>1){
      cat("There were ",length(startingRow), " matches for your currentAbbreviation.\n")
      cat("Please enter the abbreviation of the group of which you want to focus on.\n")
      readlineString <- ""
      for(x in 1:length(startingRow)){
        readlineString <- paste0(readlineString, "\"",stubfile[startingRow[x],"group"],"\"    ")
      }
      theGroup <- readline(prompt = paste0(readlineString,":\n"))
      startingRow <- startingRow[which(stubfile[startingRow,"group"] == theGroup)]
    }
    if(length(startingRow)==0){
      return(NA)
    }
    startingLevel <- stubfile[startingRow,"level"]
    currentRow <- startingRow + 1
    for(x in currentRow:nrow(stubfile)){
      if(stubfile[x,"level"]>startingLevel){
        if(!is.na(as.integer(stubfile[x,"ucc"]))){
          uccs <- c(uccs, stubfile[x,"ucc"])
        }
      } else {
        break
      }
    }
  }
  # Removing the ignoreUCCs
  if(!is.null(ignoreUCCs)){
    uccs <- uccs[!(uccs %in% ignoreUCCs)]
  }
  return(uccs)
}

# Function that creates a vector of "Human readable" income bracket names
createReadableIncomeBracketsVector <- function(incomeBrackets){
  incomeBreakpoints <- c(-Inf,incomeBrackets,Inf)
  bracketVector <- vector(length = (length(incomeBrackets)+1))
  for(x in 1:(length(incomeBreakpoints)-1)){
    if(incomeBreakpoints[x] == -Inf){
      bracketVector[x] <- paste0("Less than $",incomeBreakpoints[x+1])
    } else if(incomeBreakpoints[x+1] == Inf){
      bracketVector[x] <- paste0("More than $",incomeBreakpoints[x])
    } else {
      bracketVector[x] <- paste0("Between $",incomeBreakpoints[x]," and $",incomeBreakpoints[x+1])
    }
  }
  return(bracketVector)
}

# Creating UCC rollups for the interview files for plynty categories
iTotalExp <- categoricalUCCRollUp(IStub,"TOTALE")
iFoodAtHome <- categoricalUCCRollUp(IStub, c("FOODHO", "ALCHOM"))
iFoodAway <- categoricalUCCRollUp(IStub, c("FOODAW", "ALCAWA"))
iHousing <- categoricalUCCRollUp(IStub, c("HOUSIN"), ignoreUCCs <- categoricalUCCRollUp(IStub, c("UTILS")))
iUtilites <- categoricalUCCRollUp(IStub, c("UTILS"))
iClothingAndBeauty <- categoricalUCCRollUp(IStub, c("APPARE","PERSCA"))
iTransportation <- categoricalUCCRollUp(IStub, c("TRANS"))
iHealthcare <- categoricalUCCRollUp(IStub, c("HEALTH"))
iEntertainment <- categoricalUCCRollUp(IStub, c("ENTRTA","READIN"))
iMiscellaneous <- categoricalUCCRollUp(IStub, c("MISC","TOBACC"))
iCharitableAndFamilyGiving <- categoricalUCCRollUp(IStub, c("CASHCO"))
iInsurance <- categoricalUCCRollUp(IStub, c("LIFEIN"))
iEducation <- categoricalUCCRollUp(IStub, c("EDUCAT"))
iHousingPrinciple <- categoricalUCCRollUp(IStub,c("MRTPRI"))

# Filtering the mtab for the UCCs within the roll up categories
mtab <- filter(mtab, UCC %in% c(iFoodAtHome,iFoodAway,iHousing,iUtilites,iClothingAndBeauty,iTransportation,iHealthcare,iEntertainment,iMiscellaneous,iCharitableAndFamilyGiving,iInsurance,iEducation,iHousingPrinciple))
# Setting NA values to 0
mtab[is.na(mtab)] <- 0

# Creating variables for the different plynty categories expenditures
mtab <- mutate(mtab,
                 iTOTEXPN = ifelse(UCC %in% iTotalExp, COST * 4, 0),
                 iFOODHO = ifelse(UCC %in% iFoodAtHome, COST * 4, 0),
                 iFOODAW = ifelse(UCC %in% iFoodAway, COST * 4, 0),
                 iHOUSNG = ifelse(UCC %in% iHousing, COST * 4, 0),
                 iUTILIT = ifelse(UCC %in% iUtilites, COST * 4, 0),
                 iAPPREL = ifelse(UCC %in% iClothingAndBeauty, COST * 4, 0),
                 iTRANSN = ifelse(UCC %in% iTransportation, COST * 4, 0),
                 iHLTHCR = ifelse(UCC %in% iHealthcare, COST * 4, 0),
                 iENTRTN = ifelse(UCC %in% iEntertainment, COST * 4, 0),
                 iMISC = ifelse(UCC %in% iMiscellaneous, COST * 4, 0),
                 iINSUR = ifelse(UCC %in% iInsurance, COST * 4, 0),
                 iEDUC= ifelse(UCC %in% iEducation, COST * 4, 0),
                 iGIVNG = ifelse(UCC %in% iCharitableAndFamilyGiving, COST * 4, 0),
                 iPrinciple = ifelse(UCC %in% iHousingPrinciple, COST * -4, 0))

mtab[is.na(mtab)] <- 0

# Aggregate each expenditure variable by NEWID
iExpensesByNEWID <- group_by(mtab, NEWID) %>%
  summarise(TOTEXPN = sum(iTOTEXPN),
            FOODHO = sum(iFOODHO),
            FOODAW = sum(iFOODAW),
            HOUSNG = sum(iHOUSNG,iPrinciple), # ,iPrinciple
            UTILIT = sum(iUTILIT),
            APPREL = sum(iAPPREL),
            TRANSN = sum(iTRANSN),
            HLTHCR = sum(iHLTHCR),
            ENTRTN = sum(iENTRTN),
            MISC = sum(iMISC),
            INSUR = sum(iINSUR),
            EDUC = sum(iEDUC),
            GIVNG = sum(iGIVNG))

iExpensesByNEWID[is.na(iExpensesByNEWID)] <- 0
# Merge mtab with only the NEWIDs from the family file to include all NEWIDs
iexpensesByNEWID <- select(fmly, NEWID, INCLASS) %>% left_join(., iExpensesByNEWID, by = "NEWID")

# Calculate the mean of each plynty category
iAveragesByINCLASS <- group_by(iexpensesByNEWID, INCLASS) %>%
  summarise(TOTEXPN = round(mean(TOTEXPN), digits = 2),
            FOODHO = round(mean(FOODHO), digits = 2),
            FOODAW = round(mean(FOODAW), digits = 2),
            HOUSNG = round(mean(HOUSNG), digits = 2),
            UTILIT = round(mean(UTILIT), digits = 2),
            APPREL = round(mean(APPREL), digits = 2),
            TRANSN = round(mean(TRANSN), digits = 2),
            HLTHCR = round(mean(HLTHCR), digits = 2),
            ENTRTN = round(mean(ENTRTN), digits = 2),
            MISC = round(mean(MISC), digits = 2),
            INSUR = round(mean(INSUR), digits = 2),
            EDUC = round(mean(EDUC), digits = 2),
            GIVNG = round(mean(GIVNG), digits = 2))

iAveragesByINCLASS[is.na(iAveragesByINCLASS)] <- 0

# Ordering the iAverages data frame by income class
iAveragesByINCLASS <- iAveragesByINCLASS[order(iAveragesByINCLASS$INCLASS),]

# Creating readable rownames
rownames(iAveragesByINCLASS) <- createReadableIncomeBracketsVector(incomeBrackets = incomeBrackets)

# Creating the column and row names for the percentage dataframe
percentageColNames <- colnames(iAveragesByINCLASS)[3:length(colnames(iAveragesByINCLASS))]
percentageRowNames <- createReadableIncomeBracketsVector(incomeBrackets = incomeBrackets)

# Creating empty percentage matrix
percentageMatrix <- matrix(rep(0,(length(1:nrow(iAveragesByINCLASS)) * length(3:ncol(iAveragesByINCLASS)))), nrow = length(1:nrow(iAveragesByINCLASS)))

# Creating a vector that holds each row for the percentage matrix
temp <- vector(length = length(3:ncol(iAveragesByINCLASS)))

# Filling in the empty percentage matrix
for(x in 1:nrow(iAveragesByINCLASS)){
  for(y in 3:ncol(iAveragesByINCLASS)){
    temp[y-2] <- as.data.frame(iAveragesByINCLASS)[x,y]/sum(as.data.frame(iAveragesByINCLASS)[x,3:ncol(iAveragesByINCLASS)])
  }
  percentageMatrix[x,] <- temp
}

# Making the percentage data frame readable
percentageDF <- as.data.frame(percentageMatrix)
rownames(percentageDF) <- percentageRowNames
colnames(percentageDF) <- percentageColNames

########################################################
### Creating JSON and CSV File for use in plynty app ###
########################################################
library.packages("df2json")
setwd(my_dir)
write(df2json(percentageDF), file = paste0("plynty",changeString,".json"))
write.csv(percentageDF, file = paste0("plynty",changeString,".csv"))

#############################################################
########## Deciding which income brackets to choose #########
### Comment out if the income brackets are to your liking ###
#############################################################

# # Function that returns column number of which thtat column and the previous column are most alike
# mostLikeColumns <- function(dataframe){
#   smallestDiff <- Inf
#   for(y in 2:ncol(dataframe)){
#     difference <- 0
#     for(x in 1:nrow(dataframe)){
#       difference <- difference + (dataframe[x,y]-dataframe[x,y-1])
#     }
#     if(difference < smallestDiff){
#       smallestDiff <- difference
#       smallestDiffColumn <- y
#     }
#   }
#   return(smallestDiffColumn)
# }
# 
# # Function that returns a dataframe that has the most unique income brackets
# automaticallyMinimizingColumns <- function(dataframe, numOfColsWeWant,incomeBrackets){
#   for(i in 1:(ncol(dataframe)-numOfColsWeWant)){
#     #getting the column number to combine with the column on the left
#     colToCombine <- mostLikeColumns(dataframe = dataframe)
#     #combining the values in the columns
#     if(colToCombine == 2){
#       dataframe <- cbind(((dataframe[,colToCombine] + dataframe[,colToCombine-1])/2),dataframe[,(colToCombine+1):ncol(dataframe)])
#     } else if(colToCombine == ncol(dataframe)){
#       dataframe <- cbind(dataframe[,1:(colToCombine-2)],((dataframe[,colToCombine] + dataframe[,colToCombine-1])/2))
#     } else {
#       dataframe <- cbind(dataframe[,1:(colToCombine-2)],((dataframe[,colToCombine] + dataframe[,colToCombine-1])/2),dataframe[,(colToCombine+1):ncol(dataframe)])
#     }
#     incomeBrackets <- incomeBrackets[-(colToCombine-1)]
#     colnames(dataframe) <- createReadableIncomeBracketsVector(incomeBrackets = incomeBrackets)
#   }
#   return(dataframe)
# }
# 
# test <- automaticallyMinimizingColumns(percentageDF, 9, incomeBrackets = incomeBrackets)
# 
# barplot(t(test), beside = TRUE, col = c("darkorange","deepskyblue","forestgreen","gold","darkorchid1","red","greenyellow","violet","steelblue4"), cex.names = .9)
