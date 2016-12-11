require(dplyr)

if(!exists("get_cdata", mode = "function")) source("get_data.R")

D <- get_cdata()

# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? 
# Use the base plotting system to make a plot answering this question.

trend <- D$NEI %>%
    filter(fips == "24510") %>%
    group_by(year) %>%
    summarise(mean = mean(Emissions, na.rm = TRUE))


png("plot2.png", width = 480, height = 480)

plot(trend$year, trend$mean, main = "Total emissions PM2.5 has decreased in the Baltimore City, MD", 
     ylab = "PM2.5 emissions in the Baltimore City, MD, tons", xlab = "Year", type = "l")

dev.off()