require(dplyr)
require(ggplot2)

if(!exists("get_cdata", mode = "function")) source("get_data.R")

D <- get_cdata()

# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

trend <- D$NEI %>%
    filter( (fips == "24510") & (type == "ON-ROAD") ) %>%
    group_by(year) %>%
    summarise(mean = mean(Emissions, na.rm = TRUE))

png("plot5.png", width = 480, height = 480)    

ggplot(data = trend, aes(x = year, y = mean, colour = mean)) + 
    geom_point(size = 3) +
    geom_smooth(aes(color=..y..), method = "loess") +
    scale_colour_gradient(limits=range(trend$mean), low="lightgreen", high="red") +

    ggtitle("Emissions PM2.5 from motor vehicle sources in Baltimore City") + 
    labs(x = "Year", y = "Emissions PM2.5, tons")

dev.off()