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

#Executes the complete process for plot 3 (data retrieval, plot, file generation)
do_plot3 <- function()
{
  par("mfrow" = c(1, 1))
  
  data <- getData()
  
  plot3(data)  
  dev.copy(png, file="plot3.png", width = 480, height = 480)
  dev.off()
}

#Generates plot3
plot3 <- function(data){
  with(data, plot(Sub_metering_1~DateTime, ylab = "Energy sub metering", xlab="", type="n"))
  with(data, lines(Sub_metering_1~DateTime))
  with(data, lines(Sub_metering_2~DateTime, col = "red"))
  with(data, lines(Sub_metering_3~DateTime, col = "blue"))
  legend("topright", lty = 1, col=c("black", "red", "blue"), legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
}