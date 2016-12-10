require(readr)
require(dplyr)
require(purrr)
require(stringr)
require(lubridate)

get_tidy_data <- function() {
    
    data_folder <- file.path(getwd(), "data")
    data_name <- "household_power_consumption"
    data_file <- file.path(data_folder, paste0(data_name, ".txt"))
    data_url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
    
    if(!dir.exists(data_folder)) {
        dir.create(data_folder)
        
        if(!dir.exists(data_folder)){
            stop(paste0("Cannot create folder ", data_folder))     
        }
    }
    
    if(!file.exists(data_file)) {
        
        zip_file <- file.path(data_folder, paste0(data_name, ".zip"))
        
        if(!file.exists(zip_file)) {
            
            download.file(data_url, destfile = zip_file)
            
            if(!file.exists(zip_file)) {
                stop(paste0("Cannot download zip file : ", data_url))
            }
        }
        
        unzip(zip_file, exdir = data_folder)
        
        if(!file.exists(data_file)) {
            stop(paste0("Cannot unpack zip file : ", zip_file))
        }
    }
    
    
    rwd <- read_csv2(data_file, na = "?", col_types = cols(Time = col_character()))
    
    
    if( (dim(rwd)[1] != 2075259) || (dim(rwd)[2] != 9) ) {
        stop("Data has wrong format")
    }
    
    names(rwd) <- map_chr(names(rwd), function(x) {
        x %>%
            strsplit("_") %>%
            first %>%
            str_to_title %>%
            reduce(paste0)
    } )
    
    tidy_data <- rwd %>%
        mutate(DateTime = as.POSIXct(strptime(paste(Date, Time), format = "%d/%m/%Y %H:%M:%S"))) %>%
        select(-Date, -Time) %>%
        mutate(GlobalActivePower = as.double(GlobalActivePower)) %>%
        mutate(GlobalReactivePower = as.double(GlobalReactivePower)) %>%
        mutate(GlobalIntensity = as.double(GlobalIntensity)) %>%
        mutate(SubMetering1 = as.integer(as.double(SubMetering1))) %>%
        mutate(SubMetering2 = as.integer(as.double(SubMetering2))) %>%
        mutate(SubMetering3 = as.integer(as.double(SubMetering3))) %>%
        mutate(Voltage = Voltage / 1000)    
}

ret <<- NULL

get_filtered_data <- function() {
    
    if(!is.null(ret)) {
        return(ret)
    }
    
    ret <<- get_tidy_data() %>%
        filter( (DateTime >= ymd(20070201)) & (DateTime < ymd(20070203) ) )
}

