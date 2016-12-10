
get_data <- function() {
    
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
    
    
    #rwd <- read_csv2(data_file, na = "?", col_types = cols(Time = col_character()))
    
    
    #if( (dim(rwd)[1] != 2075259) || (dim(rwd)[2] != 9) ) {
    #    stop("Data has wrong format")
    #}
    
}

ret <<- NULL

get_data <- function() {
    
    if(!is.null(ret)) {
        return(ret)
    }
    
    ret <<- get_data()
}

