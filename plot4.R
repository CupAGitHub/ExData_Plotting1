plotFour <- function() {
  # Read the data
  df <- read.table("power.txt", header = FALSE, sep = ";") 
  names(df) <- c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
  
  # Graph the data
  p <- plot(df$Voltage, type="l", ylab="Voltage", xlab="datetime", xaxt="n")

  # Build an index array with the final endpoint
  atArray <- lapply(unique(df$Date), function(x) which(df$Date==x)[1])
  atArray <- append(atArray, length(df$Voltage)+1) 
  
  # Build a label array with the final endpoint
  labelArray <- weekdays(as.Date(unique(df$Date), "%d/%m/%Y"), abbreviate = TRUE) 
  labelArray <- append(labelArray, weekdays((max(as.Date(unique(df$Date), "%d/%m/%Y")) + 1), abbreviate = TRUE)) 
  
  # Tailor the x axis.  
  axis(1, p, labels=labelArray, at=atArray)
} 

plotFive <- function() {
  # Read the data
  df <- read.table("power.txt", header = FALSE, sep = ";") 
  names(df) <- c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
  
  # Graph the data
  p <- plot(df$Global_reactive_power, type="l", ylab="Global_reactive_power", xlab="datetime", xaxt="n")
  
  # Build an index array with the final endpoint
  atArray <- lapply(unique(df$Date), function(x) which(df$Date==x)[1])
  atArray <- append(atArray, length(df$Global_reactive_power)+1) 
  
  # Build a label array with the final endpoint
  labelArray <- weekdays(as.Date(unique(df$Date), "%d/%m/%Y"), abbreviate = TRUE) 
  labelArray <- append(labelArray, weekdays((max(as.Date(unique(df$Date), "%d/%m/%Y")) + 1), abbreviate = TRUE)) 
  
  # Tailor the x axis.  
  axis(1, p, labels=labelArray, at=atArray)
} 

dev.new(width=480, height=480)
par(mfrow=c(2,2))

plotTwo(FALSE)
plotFour()
plotThree(FALSE)
plotFive()

dev.copy(png,'plot4.png')
dev.off()
 
