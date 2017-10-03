#setwd before running code
data1 <- "pm25_data.zip"

#download the file
if(!file.exists(data1)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  download.file(fileURL, data1, method = "curl")
}
#unzip the file
if(!file.exists("pm25_data.txt")){
  unzip(data1)
}

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#plot1
png("plot1.png")
NEI$year <- as.character(NEI$year)

total_emissions <- with(NEI, tapply(Emissions, year, sum, na.rm = TRUE))

plot(total_emissions/10^6,xaxt ='n', type = "l", xlab = "Year", 
     ylab = "Total PM2.5 Emissions in Megatons",
     main = "PM2.5 Trend in the USA from 1999-2008")
axis(1, 1:4, labels = c("1999", "2002", "2005", "2008"))

dev.off()

#plot2
png("plot2.png")
library(dplyr)
NEI$year <- as.character(NEI$year)

baltimore_emissions <- NEI %>%
  filter(fips == "24510")%>%
  group_by(year)%>%
  summarize(sum_emissions = sum(Emissions, na.rm = T))

with(baltimore_emissions, plot(year, sum_emissions,
                               type = "l", ylab = "Total PM2.5 Emissions in tons", main = 
                                 "Total PM2.5 Emissions by Year in Baltimore, USA"))
with(baltimore_emissions, points(year, sum_emissions, pch = 20, col = "blue"))
dev.off()

#plot3
png("plot3.png")
library(ggplot2)
NEI$type <- as.factor(NEI$type)
NEI$year <- as.integer(NEI$year)
baltimore_emissions <- NEI %>%
  filter(fips == "24510")%>%
  group_by(type, year) %>%
  summarize(sum_emissions = sum(Emissions, na.rm = T))

ggplot(baltimore_emissions, aes(year, sum_emissions, col = type)) + 
  ylab("Total PM2.5 Emissions (tons)") +
  ggtitle("PM 2.5 Levels by Source Type in Baltimore from 1998-2008") + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
dev.off()

#plot4
png("plot4.png")
SCC_coal <- SCC[grepl("Coal", SCC$Short.Name), ]
joined_SCC <- inner_join(NEI, SCC_coal, by = "SCC")

coal_emissions <- merged_SCC %>%
  group_by(year) %>%
  summarise(emissions = sum(Emissions))

ggplot(coal_emissions, aes(year, emissions)) +
  geom_point(color = "red", size = 3) + 
  geom_line() +
  ylab("PM2.5 Emissions (tons)") +
  ggtitle("PM2.5 Emissions from Coal Combustion in the USA")

dev.off()

#plot5
png("plot5.png")
baltimore_motor_emissions <- NEI %>%
  filter(fips == "24510" & type == "ON-ROAD") %>%
  group_by(year)%>%
  summarize(emissions = sum(Emissions, na.rm = T))

ggplot(baltimore_motor_emissions, aes(year, emissions)) +
  geom_point(color = "blue", size = 3) +
  geom_line(color = "blue") +
  ylab("PM2.5 Emissions (tons)") +
  ggtitle("PM2.5 Emissions from Motor Vehicles in Baltimore (1998-2008)")

dev.off()

#plot6
png("plot6.png")
NEI$fips[NEI$fips == "24510"] <- "Baltimore"
NEI$fips[NEI$fips == "06037"] <- "LA"

baltimore_vs_LA_motor_emissions <- NEI %>%
  filter(fips == "Baltimore" & type == "ON-ROAD" | fips == "LA" & type == "ON-ROAD") %>%
  group_by(year, fips)%>%
  summarize(emissions = sum(Emissions, na.rm = T))

ggplot(baltimore_vs_LA_motor_emissions, aes(year, emissions, col = fips)) +
  geom_point(pch = 20, size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  ylab("PM2.5 Emissions (tons)") +
  ggtitle("PM2.5 Emissions from Motor Vehicles in Baltimore vs LA (1998-2008)")

dev.off()



