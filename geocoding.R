library(data.table)

path <- getwd()
data <- readRDS(paste0(path,"/",list.files(path = path, pattern = "h1bArchive")))

noGPS <- data$reqGPS
if(nrow(noGPS) > 1){
  toGetGPS <- noGPS[,unique(address)]
  # Split the addresses into chunks
  chunks <- split(toGetGPS,seq(1,ceiling(length(toGetGPS)/100)))
  
  # Gets the GPS co-ordinates for the given set of addresses
  # Due to api limit only 20 chunks are used and we have to try again after 24hours
  newChunks <- lapply(1:20, function(x){
    chunks[[x]]
  })
  
  geoCoded <- rbindlist(lapply(newChunks, function(chunk){
    loc <- rbindlist(lapply(chunk, function(x){
      # use the google api from the ggmap package with all the details
      dt <- data.table(ggmap::geocode(x,"more"))
      if(sum(is.na(dt)) < 1){
        # extract the required columns from the data if there are no Na
        dt <- dt[,.(lon,lat,administrative_area_level_1)]
        dt[, address := x]
        setnames(dt, "administrative_area_level_1", "EMPLOYER_CITY")
        return(dt)
      } else{
        # if NA's are found then create NA columns for the three columns
        dt[,':='(lon = NA_real_, lat = NA_real_, EMPLOYER_CITY = NA_real_, address = x)]
      }
    }))
    return(loc)
  }))
  
  noGPS[,':='(lat = NULL, lon = NULL, EMPLOYER_CITY = NULL, lng = NULL)]
  newTab <- merge(geoCoded[!is.na(EMPLOYER_CITY)], noGPS, "address")
  newTab[,':='(address = NULL, length = NULL)]
  newData <- data$h1dData
  newData[,':='(length = NULL)]
  setnames(newData,"lng","lon")
  newData <- rbind(newData,newTab)
  noGPS <- noGPS[!address %in% geoCoded$address]
  
  rdsData <-  list(h1dData = cleanData, reqGPS = noGPS)
  saveRDS(rdsData, file = "h1bArchive.rds")
}else{
  write("No Addresses Geocoded", "Log.txt")
}