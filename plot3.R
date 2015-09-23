# plot3.R: Which type of emissions have gone up and which down in Baltimre?
library(dplyr)
library(ggplot2)
library(grid)

# Load and clean incoming data: take only data from Baltimore; find
# total emissions by type of emission and year
load_and_clean_dataset <- function(filename) {
  nei <- readRDS("summarySCC_PM25.rds")
  nei %>% filter(fips =="24510")%>% group_by(type, year) %>% summarize(total = sum(Emissions))
}

# Make a plot using the type as a facet with emissions on the left axis
make_plot <- function(dataset) {
  plot <- qplot(year, total, 
                data = dataset, 
                facets = . ~ type, 
                color = type,
                size = I(2),
                main = "Total PE2.5 Emissions by Year and Type",
                xlab = "Year",
                ylab = "Total Emissions in Tons",
                geom = c("point", "smooth"),
                method = "lm")
  # Set the axis labels explicitly
  plot <- plot + scale_x_continuous(breaks = unique(data$year))
  # Add some space between the facets
  plot <- plot + theme(panel.margin = unit(1, "lines"))
  plot
}

# Create the requested file
png("plot3.png", width = 960)
data <- load_and_clean_dataset("summarySCC_PM25.rds")
show(make_plot(data))
dev.off()