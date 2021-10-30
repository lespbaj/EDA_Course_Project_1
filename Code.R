clean_data <- function (file) {
  
  data <- read.delim(file, header = TRUE, sep = ";", nrows=10)
  dataclass  <- sapply(data, class)
  
  
  
  
  data <- read.delim(file, header = TRUE, sep = ";", colClasses = dataclass, na.strings = "?")
  data  <- mutate(data, DateT= paste(Date, Time))
  data  <-data %>% mutate( DateTime= paste(Date, Time)) %>% select(DateTime,3:9)
  data$DateTime   <- dmy_hms(data$DateTime )
  
  clean_data    <- data[year(data$DateTime)==2007 & month(data$DateTime)==2 & (day(data$DateTime)==1|day(data$DateTime)==2), ]

  ## hist(data$Global_active_power, col="red", xlab ="Global Active Power (killowatts)" , ylab = "Frequency", main ="Global Active Power")
  
  ## with(data, plot(DateTime,Global_active_power , type="l", xlab = "", ylab = "Global Active Power (kilowatts)"))
  
  ## with(data, plot(DateTime, Sub_metering_1, type = "n", ylab = "Energy Sub Metering"))
  ## with(data, points(DateTime, Sub_metering_1, type = "l", col="black"))
  ## with(data, points(DateTime, Sub_metering_2, type = "l", col="red"))
  ## with(data, points(DateTime, Sub_metering_3, type = "l", col="blue"))
  ## legend("topright", legend= c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), col = c("black", "red","blue"), lty = 1)
  
  ## with(data, plot(DateTime, Voltage, type="l", ylab= "Voltage"))
  
  ## with(data, plot(DateTime, Global_reactive_power, type= "l"))
}
Plot_1  <- function (data) {
  png("plot1.png", width=480, height = 480 )
  hist(data$Global_active_power, col="red", xlab ="Global Active Power (killowatts)" , ylab = "Frequency", main ="Global Active Power")
  dev.off()
}
Plot_2  <- function (data) {
  png("plot2.png", width=480, height = 480 )
  with(data, plot(DateTime,Global_active_power , type="l", xlab = "", ylab = "Global Active Power (kilowatts)"))
  dev.off()
}
Plot_3  <- function (data) {
  png("plot3.png", width=480, height = 480 )
  with(data, plot(DateTime, Sub_metering_1, type = "n", ylab = "Energy Sub Metering"))
  with(data, points(DateTime, Sub_metering_1, type = "l", col="black"))
  with(data, points(DateTime, Sub_metering_2, type = "l", col="red"))
  with(data, points(DateTime, Sub_metering_3, type = "l", col="blue"))
  legend("topright", legend= c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), col = c("black", "red","blue"), lty = 1)  
  dev.off()
}
Plot_4  <- function (data) {
  png("plot4.png", width=480, height = 480 )
  par(mfrow = c(2,2))
  
  with(data, plot(DateTime,Global_active_power , type="l", xlab = "", ylab = "Global Active Power (kilowatts)"))
  with(data, plot(DateTime, Voltage, type="l", ylab= "Voltage"))
  
  with(data, plot(DateTime, Sub_metering_1, type = "n", ylab = "Energy Sub Metering"))
  with(data, points(DateTime, Sub_metering_1, type = "l", col="black"))
  with(data, points(DateTime, Sub_metering_2, type = "l", col="red"))
  with(data, points(DateTime, Sub_metering_3, type = "l", col="blue"))
  legend("topright", legend= c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), col = c("black", "red","blue"), lty = 1, lwd= 0)  
  with(data, plot(DateTime, Global_reactive_power, type= "l"))
  dev.off()
}