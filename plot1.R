plotOne <- function() {
  # Read the data
  df <- read.table("power.txt", header = FALSE, sep = ";") 
  names(df) <- c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
  
  # Group the data 
  values <- unlist(lapply(df$Global_active_power, function(x) ceiling(x/.5)))
  groups <- table(factor(values, levels=min(values):max(values)))
  
  # Graph the data
  bp <- barplot(groups, main="Global Active Power", xlab="Global Active Power (kilowatts)", ylab="Frequency", axis.lty=1, space=0, col="red", axisnames=FALSE)

  # Tailor the x axis 
  extractor <- c(TRUE, FALSE, FALSE, FALSE)
  axis(1, bp, labels=c((lapply(as.numeric(names(groups)), function(x) x/2 - .5))[extractor]), at=c(c(0:max(values))[extractor]))
  
} 


dev.new(width=480, height=480)

plotOne()

dev.copy(png,'plot1.png')
dev.off()