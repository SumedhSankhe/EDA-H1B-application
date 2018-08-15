# Load packages
pkgs <- c("data.table", "stringr", "lubridate")
sapply(pkgs, require, character = T)

# Set working directory
path <- getwd()
datFiles <- paste0(path,"/",list.files(path, "FY"))


readTransform <- function(file){
  # This function reads in an excel or a csv file and transforms it to a required format
  # Args:
  #  file : a file path to an excel or a csv file
  #
  # Returns: a transformed data.table of data
  
  st <- Sys.time()
  f <- gsub(".*\\/","",file)
  cat(" Reading",f,"....")
  
  # Use this if reading a csv file
  if (grepl(".csv",file)){
    data <- data.table(read.csv(file = file, stringsAsFactors = F))
  }
  
  # Use this if reading a xlsx file
  if(grepl(".xlsx",file)){
    data <- data.table(readxl::read_xlsx(path = file))
  }
  
  cat("Completed in", difftime(Sys.time(),st, units = "secs"),"seconds","\n")
  cat(" Starting Transformation on",f,"\n")
  
  # Renaming columns 
  if (length(grep("LCA",names(data))) > 1){
    colnames(data) <- toupper(gsub("LCA_CASE_","",names(data)))
    setnames(data, c("NUMBER", "STATUS", "SUBMIT", "FULL_TIME_POS"),
             c("CASE_NUMBER", "CASE_STATUS", "CASE_SUBMITTED", "FULL_TIME_POSITION"))
  }
  
  # Renaming columns
  if(length(which(like(colnames(data),"PW_1"))) >= 1)
    setnames(data, c("PW_1","PW_UNIT_1","WORKLOC1_STATE","WORKLOC1_CITY","PW_SOURCE_1"),
             c("PREVAILING_WAGE","PW_UNIT_OF_PAY","WORKSITE_CITY","WORKSITE_STATE","PW_WAGE_SOURCE"))
  
  # Renaming Columns
  if(length(which(like(colnames(data),"PW_WAGE_SOURCE"))) < 1){
    setnames(data,"PW_SOURCE", "PW_WAGE_SOURCE")
  }
  
  addName <- colnames(data)[which(like(colnames(data),"ADDRESS"))]
  if(length(grep("\\d",addName) >= 1)){
    setnames(data,addName[which(like(addName,"1"))],gsub("1","",addName[which(like(addName,"1"))]))
  }
  
  if(length(grep("NAIC_CODE",colnames(data)) >= 1))
    setnames(data,"NAIC_CODE","NAICS_CODE")
  
  #Select Columns which are going to be used
  dataT <- data[,.(CASE_NUMBER, CASE_STATUS, CASE_SUBMITTED, DECISION_DATE, VISA_CLASS, EMPLOYMENT_START_DATE,
                  EMPLOYMENT_END_DATE, EMPLOYER_NAME, EMPLOYER_ADDRESS, EMPLOYER_CITY, EMPLOYER_STATE,
                  EMPLOYER_POSTAL_CODE, JOB_TITLE, SOC_CODE, SOC_NAME, NAICS_CODE, FULL_TIME_POSITION,
                  PREVAILING_WAGE, PW_UNIT_OF_PAY, PW_WAGE_SOURCE, WORKSITE_CITY, WORKSITE_STATE)]
  
  # Set missing or blank values to NA
  dataT[,':='(PW_UNIT_OF_PAY = ifelse(PW_UNIT_OF_PAY == "",gsub("", NA_real_, PW_UNIT_OF_PAY),
                                      PW_UNIT_OF_PAY),
              PREVAILING_WAGE = ifelse(PREVAILING_WAGE == "", gsub("", NA_real_, PREVAILING_WAGE),
                                       PREVAILING_WAGE))]
  
  unitPay <- data[CASE_NUMBER %in% dataT[is.na(PW_UNIT_OF_PAY),CASE_NUMBER]]
  # If measure of pay is missing try to find it in the dataset if not set to NA
  if(nrow(unitPay) != 0 & "WAGE_UNIT_OF_PAY" %in% colnames(unitPay)){
    dataP <- merge(dataT[is.na(PW_UNIT_OF_PAY)], unitPay[,.(CASE_NUMBER,WAGE_UNIT_OF_PAY)])
    dataP[,PW_UNIT_OF_PAY := WAGE_UNIT_OF_PAY]
    dataP[,WAGE_UNIT_OF_PAY := NULL]
    dataT <- rbind(dataT[!is.na(PW_UNIT_OF_PAY)], dataP)
  }
  
  # Mark the year of the data
  dataT[, ':='(year = gsub(".csv","",paste0(substr(Sys.Date(), 1, 2), gsub("_.*", "", gsub(".*FY", "", file)))),
               PREVAILING_WAGE = as.numeric(PREVAILING_WAGE))]
  
  # Select applications only in the 50 states, does not include U.S. Territories
  dataT <- dataT[EMPLOYER_STATE %in% state.abb]
  
  cat(" Standardizing pay \n")
  # Standardizing pay rate to hourly
  dataT[,PREVAILING_WAGE := ifelse(PW_UNIT_OF_PAY == "Year", PREVAILING_WAGE,
                                  ifelse(PW_UNIT_OF_PAY == "Hour", 2080 * PREVAILING_WAGE,
                                         ifelse(PW_UNIT_OF_PAY == "Week", 52 * PREVAILING_WAGE,
                                                ifelse(PW_UNIT_OF_PAY == "Month", 12 * PREVAILING_WAGE,
                                                       26 * PREVAILING_WAGE))))]
  
  cat(" Standardizing dates \n")
  # Format all the dates in proper format
  dataT[,':='(CASE_SUBMITTED = as.Date(CASE_SUBMITTED, "%m/%d/%Y"),
             DECISION_DATE = as.Date(DECISION_DATE, "%m/%d/%Y"),
             EMPLOYMENT_START_DATE = as.Date(EMPLOYMENT_START_DATE, "%m/%d/%Y"),
             EMPLOYMENT_END_DATE = as.Date(EMPLOYMENT_END_DATE, "%m/%d/%Y"))]
  
  cat(" Cleansing City \n")
  # Clean employer city to filter out the erroneous ones
  dataT[, EMPLOYER_CITY := gsub('\\d', NA_real_, EMPLOYER_CITY), EMPLOYER_CITY]
  dataT[, EMPLOYER_CITY := trimws(gsub("[[:punct:]]", "", EMPLOYER_CITY)), EMPLOYER_CITY]
  dataT[EMPLOYER_CITY == "", EMPLOYER_CITY := gsub("",NA_real_,EMPLOYER_CITY)]
  dataT[,length := nchar(EMPLOYER_CITY)]
  # Cities with less that 3 letters need to be investigated
  dataT[length < 3, EMPLOYER_CITY := NA_real_]
  
  cat(" Completed",f,"in",difftime(Sys.time(),st, units = "secs"),"seconds","\n\n\n")
  # Save formatted data as rds for easier read and archive
  saveRDS(dataT,paste0("CombinedH1b ",Sys.Date(),".rds"))
  return(dataT)
}

data <- rbindlist(lapply(datFiles, readTransform))

getCities <- function(){
  # This functions downloads a database of cities and in the us with their gps locations
  # and returns a datatable of the same
  url <- "https://simplemaps.com/static/data/us-cities/uscitiesv1.4.csv"
  download.file(url = url, destfile = paste0("Cities_",Sys.Date(),".csv"), quiet = T)
  return(data.table(read.csv(paste0(path,"/",list.files(path = path,
                                                        pattern = paste0("Cities_",Sys.Date(),".csv"))))))
}

cities <- getCities()
setnames(cities, c("city", "state_id"), c("EMPLOYER_CITY", "EMPLOYER_STATE"))
cities[,':='(EMPLOYER_CITY = toupper(EMPLOYER_CITY), EMPLOYER_STATE = toupper(EMPLOYER_STATE))]
data <- merge(data, cities[,.(EMPLOYER_CITY, EMPLOYER_STATE,lat,lng)],
              by = c("EMPLOYER_CITY", "EMPLOYER_STATE"), all.x = T)

noGPS <- data[is.na(lat) | is.na(lng) | is.na(EMPLOYER_CITY)]
toGetGPS <- noGPS[,unique(paste0(EMPLOYER_ADDRESS," ",EMPLOYER_STATE))]
geoCoded <- rbindlist(lapply(toGetGPS, function(x){
  dt <- ggmap::geocode(x,"more")
}))

