# Plynty Calculations
rm(list=ls())

##################
### Parameters ###
##################

# The directory in which you are working
my_dir <- "~/"

year <- 2014

# Age Range
minAge <- 0
maxAge <- 100

# Income brackets
incomeBrackets <- c(5000,10000,15000,20000,30000,40000,50000,70000)

setwd(my_dir)

# Loading functions from CE_PUMD_Wrangler.R
source(paste0(my_dir,"/bls-ce-r-tools/CE_PUMD_Wrangler.R"))

#####################################
### Reading in the necessary Data ###
#####################################

fmly <- readCEData("fmli")
fmld <- readCEData("fmld")

mtab <- readCEData("mtbi")
expd <- readCEData("expd")

rownames(fmly) <- NULL
rownames(fmld) <- NULL
rownames(mtab) <- NULL
rownames(expd) <- NULL

convertStubFiles()

###########################
### Subsetting the Data ###
###########################
options( scipen = 20 )

# subsetting for the age bracket
fmly <- subsetByAge(fmly, minAge = minAge, maxAge = maxAge, reference = TRUE)
fmld <- subsetByAge(fmld, minAge = minAge, maxAge = maxAge, reference = TRUE)

# Converting Inclass variable to integer
fmly$INCLASS <- as.integer(fmly$INCLASS)
fmld$INCLASS <- as.integer(fmld$INCLASS)

# changing the income classes to fit our income brackets
fmly <- changeIncomeClasses(fmly, incomeBreakpoints = incomeBrackets)
fmld <- changeIncomeClasses(fmld, incomeBreakpoints = incomeBrackets)

# subsetting for the reference people who did work in the last 12 months
fmly <- subsetByWhyNoWork(fmly, NA)
fmld <- subsetByWhyNoWork(fmld, NA)

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
iTotalExp <- categoricalUCCRollUp(IntStub,"TOTALE")
iFoodAtHome <- categoricalUCCRollUp(IntStub, c("FOODHO", "ALCHOM"))
iFoodAway <- categoricalUCCRollUp(IntStub, c("FOODAW", "ALCAWA"))
iHousing <- categoricalUCCRollUp(IntStub, c("HOUSIN"), ignoreUCCs <- categoricalUCCRollUp(IntStub, c("UTILS")))
iUtilites <- categoricalUCCRollUp(IntStub, c("UTILS"))
iClothingAndBeauty <- categoricalUCCRollUp(IntStub, c("APPARE","PERSCA"))
iTransportation <- categoricalUCCRollUp(IntStub, c("TRANS"))
iHealthcare <- categoricalUCCRollUp(IntStub, c("HEALTH"))
iEntertainment <- categoricalUCCRollUp(IntStub, c("ENTRTA","READIN"))
iMiscellaneous <- categoricalUCCRollUp(IntStub, c("MISC","TOBACC"))
iCharitableAndFamilyGiving <- categoricalUCCRollUp(IntStub, c("CASHCO"))
iInsurance <- categoricalUCCRollUp(IntStub, c("LIFEIN"))
iEducation <- categoricalUCCRollUp(IntStub, c("EDUCAT"))
iHousingPrinciple <- categoricalUCCRollUp(IntStub,c("MRTPRI"))

# Creating UCC rollups for the diary files for plynty categories
dTotalExp <- categoricalUCCRollUp(IntStub,"TOTALE")
dFoodAtHome <- categoricalUCCRollUp(IntStub, c("FOODHO", "ALCHOM"))
dFoodAway <- categoricalUCCRollUp(IntStub, c("FOODAW", "ALCAWA"))
dHousing <- categoricalUCCRollUp(IntStub, c("HOUSIN"), ignoreUCCs <- categoricalUCCRollUp(IntStub, c("UTILS")))
dUtilites <- categoricalUCCRollUp(IntStub, c("UTILS"))
dClothingAndBeauty <- categoricalUCCRollUp(IntStub, c("APPARE","PERSCA"))
dTransportation <- categoricalUCCRollUp(IntStub, c("TRANS"))
dHealthcare <- categoricalUCCRollUp(IntStub, c("HEALTH"))
dEntertainment <- categoricalUCCRollUp(IntStub, c("ENTRTA","READIN"))
dMiscellaneous <- categoricalUCCRollUp(IntStub, c("MISC","TOBACC"))
dCharitableAndFamilyGiving <- categoricalUCCRollUp(IntStub, c("CASHCO"))
dInsurance <- categoricalUCCRollUp(IntStub, c("LIFEIN"))
dEducation <- categoricalUCCRollUp(IntStub, c("EDUCAT"))
dHousingPrinciple <- categoricalUCCRollUp(IntStub,c("MRTPRI"))

# Categories not in the Diary file:
## Total Expenditures
## Charitable and Family Giving
## Clothing and Beauty
## Housing Principle
## Insurance
## Utilities

# Filtering the mtab for the UCCs within the roll up categories
mtab <- filter(mtab, UCC %in% c(iFoodAtHome,iFoodAway,iHousing,iUtilites,iClothingAndBeauty,iTransportation,iHealthcare,iEntertainment,iMiscellaneous,iCharitableAndFamilyGiving,iInsurance,iEducation,iHousingPrinciple))
# Setting NA values to 0
mtab[is.na(mtab)] <- 0

# Filtering the expd for the UCCs within the roll up categories
expd <- filter(expd, UCC %in% c(dFoodAtHome,dFoodAway,dHousing,dUtilites,dClothingAndBeauty,dTransportation,dHealthcare,dEntertainment,dMiscellaneous,dCharitableAndFamilyGiving,dInsurance,dEducation))
# Setting NA values to 0
expd[is.na(expd)] <- 0

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

expd <- mutate(expd,
               dTOTEXPN = ifelse(UCC %in% dTotalExp, COST * 52, 0),
               dFOODHO = ifelse(UCC %in% dFoodAtHome, COST * 52, 0),
               dFOODAW = ifelse(UCC %in% dFoodAway, COST * 52, 0),
               dHOUSNG = ifelse(UCC %in% dHousing, COST * 52, 0),
               dUTILIT = ifelse(UCC %in% dUtilites, COST * 52, 0),
               dAPPREL = ifelse(UCC %in% dClothingAndBeauty, COST * 52, 0),
               dTRANSN = ifelse(UCC %in% dTransportation, COST * 52, 0),
               dHLTHCR = ifelse(UCC %in% dHealthcare, COST * 52, 0),
               dENTRTN = ifelse(UCC %in% dEntertainment, COST * 52, 0),
               dMISC = ifelse(UCC %in% dMiscellaneous, COST * 52, 0),
               dINSUR = ifelse(UCC %in% dInsurance, COST * 52, 0),
               dEDUC= ifelse(UCC %in% dEducation, COST * 52, 0),
               dGIVNG = ifelse(UCC %in% dCharitableAndFamilyGiving, COST * 52, 0),
               dPrinciple = ifelse(UCC %in% dHousingPrinciple, COST * -52, 0))

# Aggregate each expenditure variable by NEWID
iExpensesByNEWID <- group_by(mtab, NEWID) %>%
  summarise(iTOTEXPN = sum(iTOTEXPN),
            iFOODHO = sum(iFOODHO),
            iFOODAW = sum(iFOODAW),
            iHOUSNG = sum(iHOUSNG,iPrinciple), # ,iPrinciple
            iUTILIT = sum(iUTILIT),
            iAPPREL = sum(iAPPREL),
            iTRANSN = sum(iTRANSN),
            iHLTHCR = sum(iHLTHCR),
            iENTRTN = sum(iENTRTN),
            iMISC = sum(iMISC),
            iINSUR = sum(iINSUR),
            iEDUC = sum(iEDUC),
            iGIVNG = sum(iGIVNG))

iExpensesByNEWID <- mutate(iExpensesByNEWID,
               iTOTEXPN_PR = ifelse(iTOTEXPN == 0, 0, 1),
               iFOODHO_PR = ifelse(iFOODHO == 0, 0, 1),
               iFOODAW_PR = ifelse(iFOODAW == 0, 0, 1),
               iHOUSNG_PR = ifelse(iHOUSNG == 0, 0, 1),
               iUTILIT_PR = ifelse(iUTILIT == 0, 0, 1),
               iAPPREL_PR = ifelse(iAPPREL == 0, 0, 1),
               iTRANSN_PR = ifelse(iTRANSN == 0, 0, 1),
               iHLTHCR_PR = ifelse(iHLTHCR == 0, 0, 1),
               iENTRTN_PR = ifelse(iENTRTN == 0, 0, 1),
               iMISC_PR = ifelse(iMISC == 0, 0, 1),
               iINSUR_PR = ifelse(iINSUR == 0, 0, 1),
               iEDUC_PR= ifelse(iEDUC == 0, 0, 1),
               iGIVNG_PR = ifelse(iGIVNG == 0, 0, 1))

dExpensesByNEWID <- group_by(expd, NEWID) %>%
  summarise(dTOTEXPN = sum(dTOTEXPN),
            dFOODHO = sum(dFOODHO),
            dFOODAW = sum(dFOODAW),
            dHOUSNG = sum(dHOUSNG, dPrinciple),
            dUTILIT = sum(dUTILIT),
            dAPPREL = sum(dAPPREL),
            dTRANSN = sum(dTRANSN),
            dHLTHCR = sum(dHLTHCR),
            dENTRTN = sum(dENTRTN),
            dMISC = sum(dMISC),
            dINSUR = sum(dINSUR),
            dEDUC = sum(dEDUC),
            dGIVNG = sum(dGIVNG))

dExpensesByNEWID <- mutate(dExpensesByNEWID,
                           dTOTEXPN_PR = ifelse(dTOTEXPN == 0, 0, 1),
                           dFOODHO_PR = ifelse(dFOODHO == 0, 0, 1),
                           dFOODAW_PR = ifelse(dFOODAW == 0, 0, 1),
                           dHOUSNG_PR = ifelse(dHOUSNG == 0, 0, 1),
                           dUTILIT_PR = ifelse(dUTILIT == 0, 0, 1),
                           dAPPREL_PR = ifelse(dAPPREL == 0, 0, 1),
                           dTRANSN_PR = ifelse(dTRANSN == 0, 0, 1),
                           dHLTHCR_PR = ifelse(dHLTHCR == 0, 0, 1),
                           dENTRTN_PR = ifelse(dENTRTN == 0, 0, 1),
                           dMISC_PR = ifelse(dMISC == 0, 0, 1),
                           dINSUR_PR = ifelse(dINSUR == 0, 0, 1),
                           dEDUC_PR= ifelse(dEDUC == 0, 0, 1),
                           dGIVNG_PR = ifelse(dGIVNG == 0, 0, 1))

# Merge mtab with only the NEWIDs from the family file to include all NEWIDs
iexpensesByNEWID <- select(fmly, NEWID, INCLASS) %>% left_join(., iExpensesByNEWID, by = "NEWID")

dexpensesByNEWID <- select(fmld, NEWID, INCLASS) %>% left_join(., dExpensesByNEWID, by = "NEWID")

iexpensesByNEWID[is.na(iexpensesByNEWID)] <- 0
dexpensesByNEWID[is.na(dexpensesByNEWID)] <- 0

# Getting the averages of the expenses and percent reporting
iAveragesByINCLASS <- group_by(iexpensesByNEWID, INCLASS) %>%
  summarise(TOTEXPN = round(mean(iTOTEXPN), digits = 2),
            FOODHO = round(mean(iFOODHO), digits = 2),
            FOODAW = round(mean(iFOODAW), digits = 2),
            HOUSNG = round(mean(iHOUSNG), digits = 2),
            UTILIT = round(mean(iUTILIT), digits = 2),
            APPREL = round(mean(iAPPREL), digits = 2),
            TRANSN = round(mean(iTRANSN), digits = 2),
            HLTHCR = round(mean(iHLTHCR), digits = 2),
            ENTRTN = round(mean(iENTRTN), digits = 2),
            MISC = round(mean(iMISC), digits = 2),
            INSUR = round(mean(iINSUR), digits = 2),
            EDUC = round(mean(iEDUC), digits = 2),
            GIVNG = round(mean(iGIVNG), digits = 2),
            TOTEXPN_PR = mean(iTOTEXPN_PR),
            FOODHO_PR = mean(iFOODHO_PR),
            FOODAW_PR = mean(iFOODAW_PR),
            HOUSNG_PR = mean(iHOUSNG_PR),
            UTILIT_PR = mean(iUTILIT_PR),
            APPREL_PR = mean(iAPPREL_PR),
            TRANSN_PR = mean(iTRANSN_PR),
            HLTHCR_PR = mean(iHLTHCR_PR),
            ENTRTN_PR = mean(iENTRTN_PR),
            MISC_PR = mean(iMISC_PR),
            INSUR_PR = mean(iINSUR_PR),
            EDUC_PR = mean(iEDUC_PR),
            GIVNG_PR = mean(iGIVNG_PR))

dAveragesByINCLASS <- group_by(dexpensesByNEWID, INCLASS) %>%
  summarise(TOTEXPN = round(mean(dTOTEXPN), digits = 2),
            FOODHO = round(mean(dFOODHO), digits = 2),
            FOODAW = round(mean(dFOODAW), digits = 2),
            HOUSNG = round(mean(dHOUSNG), digits = 2),
            UTILIT = round(mean(dUTILIT), digits = 2),
            APPREL = round(mean(dAPPREL), digits = 2),
            TRANSN = round(mean(dTRANSN), digits = 2),
            HLTHCR = round(mean(dHLTHCR), digits = 2),
            ENTRTN = round(mean(dENTRTN), digits = 2),
            MISC = round(mean(dMISC), digits = 2),
            INSUR = round(mean(dINSUR), digits = 2),
            EDUC = round(mean(dEDUC), digits = 2),
            GIVNG = round(mean(dGIVNG), digits = 2),
            TOTEXPN_PR = mean(dTOTEXPN_PR),
            FOODHO_PR = mean(dFOODHO_PR),
            FOODAW_PR = mean(dFOODAW_PR),
            HOUSNG_PR = mean(dHOUSNG_PR),
            UTILIT_PR = mean(dUTILIT_PR),
            APPREL_PR = mean(dAPPREL_PR),
            TRANSN_PR = mean(dTRANSN_PR),
            HLTHCR_PR = mean(dHLTHCR_PR),
            ENTRTN_PR = mean(dENTRTN_PR),
            MISC_PR = mean(dMISC_PR),
            INSUR_PR = mean(dINSUR_PR),
            EDUC_PR = mean(dEDUC_PR),
            GIVNG_PR = mean(dGIVNG_PR))

# Ordering the data frames by income class
iAveragesByINCLASS <- iAveragesByINCLASS[order(iAveragesByINCLASS$INCLASS),]
dAveragesByINCLASS <- dAveragesByINCLASS[order(dAveragesByINCLASS$INCLASS),]

realColNames <- names(iAveragesByINCLASS)[-1]
regularNames <- realColNames[1:(length(realColNames)/2)]
percentagesNames <- realColNames[((length(realColNames)/2)+1):(length(realColNames))]

dAveragesByINCLASS <- as.data.frame(dAveragesByINCLASS)
for(x in 1:length(regularNames)){
  if(dAveragesByINCLASS[,percentagesNames[x]] != 0){
    dAveragesByINCLASS[,regularNames[x]] <- dAveragesByINCLASS[,regularNames[x]] / dAveragesByINCLASS[,percentagesNames[x]]
  }
}

iAveragesByINCLASS <- as.data.frame(iAveragesByINCLASS)
for(x in 1:length(regularNames)){
  if(iAveragesByINCLASS[,percentagesNames[x]] != 0){
    iAveragesByINCLASS[,regularNames[x]] <- iAveragesByINCLASS[,regularNames[x]] / iAveragesByINCLASS[,percentagesNames[x]]
  }
}

total_expenditure_mean <- iAveragesByINCLASS + dAveragesByINCLASS
# removeColumns <- c(seq(ncol(total_expenditure_mean),15),2,1)
# total_expenditure_mean <- total_expenditure_mean[,-removeColumns]

total_expenditure_mean <- t(total_expenditure_mean)
colnames(total_expenditure_mean) <- createReadableIncomeBracketsVector(incomeBrackets = incomeBrackets)
removeRows <- c(which(rownames(total_expenditure_mean) == "INCLASS" | rownames(total_expenditure_mean) == "TOTEXPN"),grep("_PR", rownames(total_expenditure_mean)))
total_expenditure_mean <- total_expenditure_mean[-removeRows,]

total_expenditure_mean <- as.data.frame(total_expenditure_mean)

totalMeans <- rep(0,ncol(total_expenditure_mean))
for(x in 1:ncol(total_expenditure_mean)){
  totalMeans[x] <- sum(total_expenditure_mean[,x])
}

plyntyPercentages <- total_expenditure_mean
for(x in 1:ncol(plyntyPercentages)){
  plyntyPercentages[,x] <- plyntyPercentages[,x]/totalMeans[x]
}