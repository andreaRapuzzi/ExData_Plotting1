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

#Executes the complete process for plot 4 (data retrieval, plot, file generation)
do_plot4 <- function()
{
  par("mfrow" = c(2, 2))
  
  data <- getData()
  
  plot4(data)  
  dev.copy(png, file="plot4.png", width = 480, height = 480)
  dev.off()
}

#Generates plot4
plot4 <- function(data){
  with(data, plot(Global_active_power~DateTime, ylab = "Global Active Power", xlab="", type="n"))
  with(data, lines(Global_active_power~DateTime))
  
  with(data, plot(Voltage~DateTime, ylab = "Voltage", xlab="datetime", type="n"))
  with(data, lines(Voltage~DateTime))
  
  with(data, plot(Sub_metering_1~DateTime, ylab = "Energy sub metering", xlab="", type="n"))
  with(data, lines(Sub_metering_1~DateTime))
  with(data, lines(Sub_metering_2~DateTime, col = "red"))
  with(data, lines(Sub_metering_3~DateTime, col = "blue"))
  legend("topright", lty = 1, col=c("black", "red", "blue"), legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),  bty = "n")
  
  with(data, plot(Global_reactive_power~DateTime, xlab="datetime", type="n"))
  with(data, lines(Global_reactive_power~DateTime))
}