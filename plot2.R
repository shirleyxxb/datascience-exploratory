# plot2.R: Creates a graph of total PM emissions in Baltimore by year
library(dplyr)

# Load and clean incoming data: take only data from Baltimore
load_and_clean_dataset <- function(filename) {
  nei <- readRDS("summarySCC_PM25.rds")
  nei %>% filter(fips =="24510") %>% group_by(year) %>% summarize(total = sum(Emissions))
}

# Crete an appropriately themed plot
make_plot <- function(dataset) {
  options(scipen = 5)  # Don't use scientific notation
  plot(dataset$year, dataset$total,
       pch=15,
       col="blue",
       main="Baltimore PM2.5 Emissions by Year",
       xlab="Year",
       xaxt='n',
       ylab="Total Emissions in Tons")
  axis(side=1, at=dataset$year, labels=T)
  # Add a linear fit line
  model <- lm(dataset$total ~ dataset$year)
  abline(model, col="red")
}

# Create the requested file
png("plot2.png")
data <- load_and_clean_dataset("summarySCC_PM25.rds")
make_plot(data)
dev.off()