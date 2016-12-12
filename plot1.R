require(dplyr)

if(!exists("get_cdata", mode = "function")) source("get_data.R")

D <- get_cdata()

# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission from all sources 
# for each of the years 1999, 2002, 2005, and 2008.

trend <- D$NEI %>%
    group_by(year) %>%
    summarise(mean = mean(Emissions, na.rm = TRUE))


png("plot1.png", width = 480, height = 480)

plot(trend$year, trend$mean, main = "Total emissions PM2.5 in the United States", 
     ylab = "PM2.5, tons", xlab = "Year", type = "l")

dev.off()