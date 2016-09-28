plotThree <- function(showBox) {

  # Read the data
  df <- read.table("power.txt", header = FALSE, sep = ";") 
  names(df) <- c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
  
  # Graph the data
  p <- plot(df$Sub_metering_1, type="l", ylab="Energy sub metering", xlab="", xaxt="n")
  lines(df$Sub_metering_2, type='l', col="red", lwd=2)
  lines(df$Sub_metering_3, type='l', col="blue", lwd=2)
  
  if(showBox == TRUE)
  {
    showBorder = "y"
  }
  else
  {
    showBorder = "n"
  }
  # Add the legend
  legend("topright", c("Sub_metering_1","Sub_metering_2", "Sub_metering_3"), lty=c(1,1,1), col=c("black","red","blue"), bty = showBorder)
  
  # Build an index array with the final endpoint
  atArray <- lapply(unique(df$Date), function(x) which(df$Date==x)[1])
  atArray <- append(atArray, length(df$Sub_metering_1)+1) 
  
  # Build a label array with the final endpoint
  labelArray <- weekdays(as.Date(unique(df$Date), "%d/%m/%Y"), abbreviate = TRUE) 
  labelArray <- append(labelArray, weekdays((max(as.Date(unique(df$Date), "%d/%m/%Y")) + 1), abbreviate = TRUE)) 
  
  # Tailor the x axis.  
  axis(1, p, labels=labelArray, at=atArray)
} 

dev.new(width=480, height=480)

plotThree(TRUE)

dev.copy(png,'plot3.png')
dev.off() 

