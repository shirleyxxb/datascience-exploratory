# plot6.R: How have emissions from motor vechicles changed
# in Baltimore and Los Angeles?
library(ggplot2)
library(grid)

# Load and clean incoming data: figure out the SCC codes for
# motor vehicles and filter the NEI data only for those, in
# Baltimore City and Los Angeles
load_and_clean_dataset <- function(filename, classfile) {
  # Load up the classifications document and select only highway vehicles
  classifications <- readRDS(classfile)
  vehicles <- filter(classifications, grepl("Highway Vehicles", SCC.Level.Two))
  # Read in the NEI data and then filter by code so we have only vehicle data
  nei <- readRDS(filename)
  nei$SCC <- as.factor(nei$SCC)
  nei <- filter(nei, SCC %in% vehicles$SCC)
  # Only take those in Baltimore
  nei <- filter(nei, fips %in% c("24510", "06037"))
  # Merge in the vehicle classifications table so we get SCC.Level.Three, which
  # we will be using as category of vehicle
  nei <- left_join(nei, vehicles)
  # Select the only columns we care about
  nei <- select(nei, SCC, Emissions, year, city = fips)
  # Calculate the totals by year and city
  nei <- nei %>% group_by(year, city) %>% summarize(total = sum(Emissions))
  # Rename the city column for easier plot readin'
  nei$city[nei$city == "24510"] <- "Baltimore"
  nei$city[nei$city == "06037"] <- "Los Angeles"
  nei
}

# Make the plot that tells the story
make_plot <- function(dataset) {
  plot <- qplot(year, total, 
                data = dataset, 
                main="Total Motor Vehicle Emissions in Baltimore and Los Angeles",
                ylab="Emissions in Tons of PM2.5",
                color = city, 
                size=I(2),
                geom=c("point", "smooth"), 
                method="lm")
  # Scale the Y axis by log so that relative change is more obvious
  plot <- plot + scale_y_log10()
  plot
}

# Create the requested file
options(scipen=5)
png("plot6.png")
data <- load_and_clean_dataset("summarySCC_PM25.rds", "Source_Classification_Code.rds")
show(make_plot(data))
dev.off()