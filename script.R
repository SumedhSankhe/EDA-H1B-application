pkgs <- c("data.table", "stringr", "lubridate")
sapply(pkgs, require, character = T)

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
  
  if (grepl(".csv",file)){
    data <- data.table(read.csv(file = file, stringsAsFactors = F))
  }
  
  if(grepl(".xlsx",file)){
    data <- data.table(readxl::read_xlsx(path = file))
  }
  
  cat("Completed in", difftime(Sys.time(),st, units = "secs"),"seconds","\n")
  cat(" Starting Transformation on",f,"\n")
  if (length(grep("LCA",names(data))) > 1){
    colnames(data) <- toupper(gsub("LCA_CASE_","",names(data)))
    setnames(data, c("NUMBER", "STATUS", "SUBMIT", "FULL_TIME_POS"),
             c("CASE_NUMBER", "CASE_STATUS", "CASE_SUBMITTED", "FULL_TIME_POSITION"))
  }
  
  if(length(which(like(colnames(data),"PW_1"))) >= 1)
    setnames(data, c("PW_1","PW_UNIT_1","WORKLOC1_STATE","WORKLOC1_CITY","PW_SOURCE_1"),
             c("PREVAILING_WAGE","PW_UNIT_OF_PAY","WORKSITE_CITY","WORKSITE_STATE","PW_WAGE_SOURCE"))
  
  if(length(which(like(colnames(data),"PW_WAGE_SOURCE"))) < 1){
    setnames(data,"PW_SOURCE", "PW_WAGE_SOURCE")
  }
  
  addName <- colnames(data)[which(like(colnames(data),"ADDRESS"))]
  if(length(grep("\\d",addName) >= 1)){
    setnames(data,addName[which(like(addName,"1"))],gsub("1","",addName[which(like(addName,"1"))]))
  }
  
  if(length(grep("NAIC_CODE",colnames(data)) >= 1))
    setnames(data,"NAIC_CODE","NAICS_CODE")
  
  dataT <- data[,.(CASE_NUMBER, CASE_STATUS, CASE_SUBMITTED, DECISION_DATE, VISA_CLASS, EMPLOYMENT_START_DATE,
                  EMPLOYMENT_END_DATE, EMPLOYER_NAME, EMPLOYER_ADDRESS, EMPLOYER_CITY, EMPLOYER_STATE,
                  EMPLOYER_POSTAL_CODE, JOB_TITLE, SOC_CODE, SOC_NAME, NAICS_CODE, FULL_TIME_POSITION,
                  PREVAILING_WAGE, PW_UNIT_OF_PAY, PW_WAGE_SOURCE, WORKSITE_CITY, WORKSITE_STATE)]
  
  dataT[,':='(PW_UNIT_OF_PAY = ifelse(PW_UNIT_OF_PAY == "",gsub("", NA_real_, PW_UNIT_OF_PAY),
                                      PW_UNIT_OF_PAY),
              PREVAILING_WAGE = ifelse(PREVAILING_WAGE == "", gsub("", NA_real_, PREVAILING_WAGE),
                                       PREVAILING_WAGE))]
  
  unitPay <- data[CASE_NUMBER %in% dataT[is.na(PW_UNIT_OF_PAY),CASE_NUMBER]]
  cat("\n",nrow(unitPay),"\n")
  if(nrow(unitPay) != 0 & "WAGE_UNIT_OF_PAY" %in% colnames(unitPay)){
    dataP <- merge(dataT[is.na(PW_UNIT_OF_PAY)], unitPay[,.(CASE_NUMBER,WAGE_UNIT_OF_PAY)])
    dataP[,PW_UNIT_OF_PAY := WAGE_UNIT_OF_PAY]
    dataP[,WAGE_UNIT_OF_PAY := NULL]
    dataT <- rbind(dataT[!is.na(PW_UNIT_OF_PAY)], dataP)
  }
  
  dataT[, ':='(year = gsub(".csv","",paste0(substr(Sys.Date(), 1, 2), gsub("_.*", "", gsub(".*FY", "", file)))),
               PREVAILING_WAGE = as.numeric(PREVAILING_WAGE))]
  
  dataT <- dataT[EMPLOYER_STATE %in% state.abb]
  
  cat(" Standardizing pay \n")
  dataT[,PREVAILING_WAGE := ifelse(PW_UNIT_OF_PAY == "Year", PREVAILING_WAGE,
                                  ifelse(PW_UNIT_OF_PAY == "Hour", 2080 * PREVAILING_WAGE,
                                         ifelse(PW_UNIT_OF_PAY == "Week", 52 * PREVAILING_WAGE,
                                                ifelse(PW_UNIT_OF_PAY == "Month", 12 * PREVAILING_WAGE,
                                                       26 * PREVAILING_WAGE))))]
  
  cat(" Standardizing dates \n")
  dataT[,':='(CASE_SUBMITTED = as.Date(CASE_SUBMITTED, "%m/%d/%Y"),
             DECISION_DATE = as.Date(DECISION_DATE, "%m/%d/%Y"),
             EMPLOYMENT_START_DATE = as.Date(EMPLOYMENT_START_DATE, "%m/%d/%Y"),
             EMPLOYMENT_END_DATE = as.Date(EMPLOYMENT_END_DATE, "%m/%d/%Y"))]
  
  cat(" Completed",f,"in",difftime(Sys.time(),st, units = "secs"),"seconds","\n\n\n")
  saveRDS(dataT,paste0("CombinedH1b ",Sys.Date(),".rds"))
  return(dataT)
}

data <- rbindlist(lapply(datFiles, readTransform))


getLocation <- function(dat){
  library(XML)
  dat[,loc := paste(EMPLOYER_ADDRESS,EMPLOYER_CITY, EMPLOYER_STATE, EMPLOYER_POSTAL_CODE, sep = ", ")]
  loc <- x[,unique(loc)]
  
  y <- rbindlist(lapply(seq(1:length(loc)), function(x){
    # cat(x,"of",length(loc),"\n")
    addr <- loc[x]
    url = paste('http://maps.google.com/maps/api/geocode/xml?address=', addr,'&sensor=false',sep='')  # construct the URL
    doc = xmlTreeParse(url) 
    root = xmlRoot(doc)
    lat = xmlValue(root[['result']][['geometry']][['location']][['lat']]) 
    long = xmlValue(root[['result']][['geometry']][['location']][['lng']])
    cat(cbind(addr,lat,long))
    Sys.sleep(0.1)
    return(cbind(addr,lat,long))
  }))
  
  addr <- '6th Main Rd, New Thippasandra, Bengaluru, Karnataka'  # set your address here
  
}
