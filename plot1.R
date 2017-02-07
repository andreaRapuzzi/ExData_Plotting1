#Reads the data from file, filter them by date and creates an additional DateTime column
getData <- function()
{
  require(dplyr)
  require(lubridate)
  
  whole <- read.table("household_power_consumption.txt", sep=";", stringsAsFactors = F, na.strings = "?", header = T)
  filtered <- subset(whole, whole$Date == "1/2/2007" | whole$Date == "2/2/2007")
  augmented <- mutate(filtered, DateTime = as.POSIXct(dmy(Date) + hms(Time)))
  
  augmented
}

#Executes the complete process for plot 1 (data retrieval, plot, file generation)
do_plot1 <- function()
{
  par("mfrow" = c(1, 1))
  
  data <- getData()
  
  plot1(data)  
  dev.copy(png, file="plot1.png", width = 480, height = 480)
  dev.off()
}

#Generates plot1
plot1 <- function(data){
  with(data, hist(Global_active_power, col="red", main="Global Active Power", xlab = "Global Active Power (kilowatts)"))
}