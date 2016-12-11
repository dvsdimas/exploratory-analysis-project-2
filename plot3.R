require(dplyr)
require(ggplot2)

if(!exists("get_cdata", mode = "function")) source("get_data.R")

D <- get_cdata()

# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
# Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.


trend <- D$NEI %>%
    filter(fips == "24510") %>%
    mutate(type = as.factor(type)) %>%
    group_by(type, year) %>%
    summarise(mean = mean(Emissions, na.rm = TRUE))


png("plot3.png", width = 480, height = 480)

ggplot(trend, aes(year, mean)) + geom_line(aes(colour = type), size = 1) + 
    ggtitle("Emissions PM2.5 has decreased in the Baltimore City, MD") + 
    labs(x = "Year", y = "Emissions PM2.5, tons")

dev.off()