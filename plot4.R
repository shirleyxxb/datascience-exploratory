# plot4.R: How have emissions from Coal-related sources changed?
library(dplyr)
library(ggplot2)
library(grid)

# Load and clean incoming data: figure out the SCC codes for
# "coal combustion related" activities and filter the NEI data
# for only those.
load_and_clean_dataset <- function(filename, classfile) {
  # For the purpose of this project, we'll consider use of the
  # word "Coal" in the EI.Sector column coal-combusion related.
  classifications <- readRDS(classfile)
  classifications <- filter(classifications, grepl("Coal", EI.Sector))
  # Read in the NEI data and then filter so we have only coal
  nei <- readRDS("summarySCC_PM25.rds")
  nei <- filter(nei, SCC %in% classifications$SCC)
  # Now group by type and year to graph
  nei <- nei %>% group_by(type, year) %>% summarize(total = sum(Emissions))
  nei
}

# Make a plot using the type as a facet with emissions on the left axis
make_plot <- function(dataset) {
  plot <- qplot(year, total, 
                data = dataset, 
                facets = . ~ type, 
                color = type,
                size = I(4),
                main = "Total Coal-Related PE2.5 Emissions by Year and Type",
                xlab = "Year",
                ylab = "Total Emissions in Tons",
                geom = c("point", "smooth"),
                method = "lm")
  # Set the axis labels explicitly
  plot <- plot + scale_x_continuous(breaks = unique(dataset$year))
  # Add some space between the facets
  plot <- plot + theme(panel.margin = unit(1, "lines"))
  plot
}

# Create the requested file
options(scipen=5)
png("plot4.png", width = 720)
data <- load_and_clean_dataset("summarySCC_PM25.rds", "Source_Classification_Code.rds")
show(make_plot(data))
dev.off()