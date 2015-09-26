# plot5.R: How have emissions from Motor Vehicles in Baltimore changed?
library(dplyr)
library(ggplot2)
library(grid)

# Load and clean incoming data: figure out the SCC codes for
# motor vehicles and filter the NEI data only for those, in
# Baltimore City
load_and_clean_dataset <- function(filename, classfile) {
  # Load up the classifications document and select only highway vehicles
  classifications <- readRDS(classfile)
  vehicles <- filter(classifications, grepl("Highway Vehicles", SCC.Level.Two))
  # Read in the NEI data and then filter by code so we have only vehicle data
  nei <- readRDS(filename)
  nei$SCC <- as.factor(nei$SCC)
  nei <- filter(nei, SCC %in% vehicles$SCC)
  # Only take those in Baltimore
  nei <- filter(nei, fips =="24510")
  # Merge in the vehicle classifications table so we get SCC.Level.Three, which
  # we will be using as category of vehicle
  nei <- left_join(nei, vehicles)
  # Select the only columns we care about
  nei <- select(nei, SCC, Emissions, type, year, category = SCC.Level.Three)
  # Calculate the totals by year and category
  nei <- nei %>% group_by(year, category) %>% summarize(total = sum(Emissions))
  # Trim the categories
  nei$category <- factor(nei$category)
  nei
}

# Make the plot that tells the story
make_plot <- function(dataset) {
  plot <- qplot(year, total, 
                data = dataset, 
                main="Total Baltimore Motor Vehicle Emissions by Year and Category",
                ylab="Emissions in Tons of PM2.5",
                color = category, 
                size=I(1),
                geom=c("point", "smooth"), 
                method="lm")
  plot <- plot + facet_wrap(~ category, ncol = 2, scales = "free")
  # Add some space between the facets
  plot <- plot + theme(panel.margin = unit(1, "lines"))
  plot
}

# Create the requested file
options(scipen=5)
png("plot5.png", width = 1400, height=1000)
data <- load_and_clean_dataset("summarySCC_PM25.rds", "Source_Classification_Code.rds")
show(make_plot(data))
dev.off()