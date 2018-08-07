pkgs <- c("data.table", "stringr", "lubridate")
sapply(pkgs, require, character = T)

path <- getwd()
datFiles <- paste0(path,"/",list.files(path, "FY"))

function(file){
  st <- Sys.time()
  if (grepl(".csv",file)){
    f <- gsub(".*\\/","",file)
    cat("Reading",f,"\n")
    data <- data.table(read.csv(file = file))
    cat("  Read in", difftime(Sys.time(),st, units = "secs"),"seconds","\n")
    cat("Starting Transformation on",f)
    
    if (length(grep("LCA",names(data))) > 1){
      colnames(data) <- toupper(gsub("LCA_CASE_","",names(data)))
      setnames(data, c("NUMBER", "STATUS", "SUBMIT", "FULL_TIME_POS"),
               c("CASE_NUMBER", "CASE_STATUS", "CASE_SUBMITTED", "FULL_TIME_POSITION"))
    }
    
    data <- data[,.(CASE_NUMBER, CASE_STATUS, CASE_SUBMITTED, DECISION_DATE, VISA_CLASS, EMPLOYMENT_START_DATE,
                    EMPLOYMENT_END_DATE, EMPLOYER_NAME, EMPLOYER_ADDRESS, EMPLOYER_CITY, EMPLOYER_STATE,
                    EMPLOYER_POSTAL_CODE, JOB_TITLE, SOC_CODE, SOC_NAME, NAICS_CODE, FULL_TIME_POSITION,
                    PREVAILING_WAGE, PW_UNIT_OF_PAY, PW_WAGE_LEVEL, PW_WAGE_SOURCE, WORKSITE_CITY, WORKSITE_STATE)]
    
    data[, year := paste0(substr(Sys.Date(), 1, 2), gsub("_.*", "", gsub(".*FY", "", file)))]
    
    data[,':='(CASE_SUBMITTED = as.Date(CASE_SUBMITTED, "%m/%d/%Y"),
               DECISION_DATE = as.Date(DECISION_DATE,"%m/%d/%Y"),
               EMPLOYMENT_START_DATE = as.Date(EMPLOYMENT_START_DATE, "%m/%d/%Y"),
               EMPLOYMENT_END_DATE = as.Date(EMPLOYMENT_END_DATE, "%m/%d/%Y"))]
  }
}
