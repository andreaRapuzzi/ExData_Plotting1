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

#Executes the complete process for plot 2 (data retrieval, plot, file generation)
do_plot2 <- function()
{
  par("mfrow" = c(1, 1))
  
  data <- getData()
  
  plot2(data)  
  dev.copy(png, file="plot2.png", width = 480, height = 480)
  dev.off()
}

#Generates plot2
plot2 <- function(data){
  with(data, plot(Global_active_power~DateTime, ylab = "Global Active Power (kilowatts)", xlab="", type="n"))
  with(data, lines(Global_active_power~DateTime))
}