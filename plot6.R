require(dplyr)
require(ggplot2)

get_data <- function() {
    
    data_folder <- file.path(getwd(), "data")
    
    file_source <- file.path(data_folder, "Source_Classification_Code.rds")
    file_summary <- file.path(data_folder, "summarySCC_PM25.rds")
    
    
    if(!dir.exists(data_folder)) {
        dir.create(data_folder)
        
        if(!dir.exists(data_folder)){
            stop(paste0("Cannot create folder ", data_folder))     
        }
    }
    
    if(!file.exists(file_source) || !file.exists(file_summary)) {
        
        zip_file <- file.path(data_folder, "exdata.zip")
        
        if(!file.exists(zip_file)) {
            
            data_url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
            
            download.file(data_url, destfile = zip_file)
            
            if(!file.exists(zip_file)) {
                stop(paste0("Cannot download zip file : ", data_url))
            }
        }
        
        unzip(zip_file, exdir = data_folder)
        
        if(!file.exists(file_source) || !file.exists(file_summary)) {
            stop(paste0("Cannot unpack zip file : ", zip_file))
        }
        
        unlink(zip_file)
    }
    
    NEI <- readRDS(file_summary)
    
    if( (dim(NEI)[1] != 6497651) || (dim(NEI)[2] != 6) ) {
        stop("NEI data has wrong format")
    }
    
    SCC <- readRDS(file_source)
    
    if( (dim(SCC)[1] != 11717) || (dim(SCC)[2] != 15) ) {
        stop("SCC data has wrong format")
    }
    
    list(NEI = NEI, SCC = SCC)
}

ret <<- NULL

get_cdata <- function() {
    
    if(!is.null(ret)) {
        return(ret)
    }
    
    ret <<- get_data()
}



# Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources 
# in Los Angeles County, California (fips == "06037"). 
# Which city has seen greater changes over time in motor vehicle emissions?

D <- get_cdata()

trend <- D$NEI %>%
    filter(type == "ON-ROAD") %>%
    filter( (fips == "24510") | (fips == "06037") ) %>%
    mutate(city = ifelse(fips == "24510", "Baltimore City, MD", "Los Angeles County, CA")) %>%
    group_by(city, year) %>%
    summarise(mean = mean(Emissions, na.rm = TRUE))

png("plot6.png", width = 700, height = 600)    

ggplot(data = trend, aes(x = year, y = mean, colour = city)) + 
    geom_point(size = 3) +
    geom_line() +
    ggtitle("Emissions from motor vehicle sources in Baltimore City, MD and Los Angeles County, CA") + 
    labs(x = "Year", y = "Emissions PM2.5, tons")

dev.off()