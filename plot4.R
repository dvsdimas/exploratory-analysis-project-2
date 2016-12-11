require(dplyr)
require(ggplot2)

if(!exists("get_cdata", mode = "function")) source("get_data.R")

D <- get_cdata()

# Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

trend <- D$SCC %>%
    select(SCC, Short.Name) %>%
    filter(grepl("coal", Short.Name, ignore.case = TRUE)) %>%
    filter(grepl("combustion", Short.Name, ignore.case = TRUE)) %>%
    mutate(SCC = as.character(SCC)) %>%
    left_join(D$NEI, by = "SCC") %>%
    select(-Pollutant, -Short.Name) %>%
    filter(!is.na(year) & !is.na(Emissions) & !is.nan(Emissions)) %>%
    group_by(year) %>%
    summarise(mean = mean(Emissions, na.rm = TRUE))

png("plot4.png", width = 480, height = 480)    

ggplot(trend, aes(year, mean)) + geom_line(aes(group=1)) + geom_point() + 
    ggtitle("Emissions PM2.5 from coal combustion-related sources across the US") + 
    labs(x = "Year", y = "Emissions PM2.5, tons")

dev.off()
