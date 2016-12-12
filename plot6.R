require(dplyr)
require(ggplot2)

if(!exists("get_cdata", mode = "function")) source("get_data.R")

D <- get_cdata()

# Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources 
# in Los Angeles County, California (fips == "06037"). 
# Which city has seen greater changes over time in motor vehicle emissions?

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