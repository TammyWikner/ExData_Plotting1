#
# plot4.R - Written for the Coursera Exploratory Data Analysis Week 1 
# 			Project Assignment. 
#
# Assignment - Given a picture of plot 4, construct a plot to look like the 
# 	given plot and save it to a PNG file with a width of 480 pixels and a 
#	height of 480 pixels. Name the plot plot1.png. 
#
# Goal: Using the electric power consumption data from the UCI Irvine 
#		Learning Repository, examine how household energy usage varies over 
#  		a 2-day period in February 2007, specifically over February 1 and 
#		February 2.
#
# Dataset Abstract: Measurements of electric power consumption in one 
# 	household with a one-minute sampling rate over a period of almost 4 years. 
#	Different electrical quantities and some sub-metering values are available.
#
# Instructions to Run:
#   1) Download the "household_power_consumption.zip" dataset at: 
# 		https://archive.ics.uci.edu/ml/machine-learning-databases/00235/
#   	to your working directorywhere plot1.R is located.
#   2) Save and source plot4.R in your working directory
#
# Ouput:
#	plot4.png file written to your working directory
#
#	Dataset Info:
#		Size: 2,075,259 rows and 9 columns/variables. 
#   	Variables:
#			Date: Date in format dd/mm/yyyy
#			Time: time in format hh:mm:ss
#			Global_active_power
#			Global_reactive_power
#			Voltage
#			Global_intestity
#			Sub_metering_1
#			Sub_metering_2
# 			Sub_metering_3
#
# 		

plot4 <- function(){
	
	# Unzip dataset if it hasn't been already
	zipFile <- "exdata-data-household_power_consumption.zip"
	if(!file.exists("household_power_consumption.txt")) {
		unzip(zipFile)
		filename <- "household_power_consumption.txt"
	} else {
		filename <- "household_power_consumption.txt"
	}
	
	classes <- c("character", "character","numeric","numeric", "numeric", 
				 "numeric", "numeric", "numeric", "numeric")
	
	# The two-day period for the plot data is February 1, 2007 to 
	# February 2, 2007. 
	# 
	# Due to the large size of the original dataset, a shell command was run
	# (outside of this R file) in a terminal window to find the line number 
	# of the first occurence of the date: "1/2/2007" in the dataset 
	# file: "household_power_consumption.txt" 
	#  	$ grep -n -m 1 "^1/2/2007" household_power_consumption.txt 
	#	$ 66638
	# One is then subtracted from this number to get the line number 
	# that the read.table() skip argument should be set to in a later call.
	# 
	startReadingAt <- 66638-1
	
	# Also, the shell command was used to find the line number of the first 
	# occurence of the date: "3/2/2007" in the dataset 
	# file: "household_power_consumption.txt" 
	#  	$ grep -n -m 1 "^3/2/2007" household_power_consumption.txt 
	#	$ 69518
	# One is then subtracted from this number to give you the last 
	# line with a "2/2/2007" date and thus thet last line that 
	# should be read from the dataset corresponding second day of 
	# the dates of interest. 
	# 	
	stopReadingAt <- 69518-1
	
	# Determine how many lines to read from dataset
	numLinesToRead <- stopReadingAt - startReadingAt
	
	powerConsumption <- read.table(filename, 
								   sep=";", 
								   col.names=c("Date","Time",
								   			"Global Active Power", 
								   			"Global Reactive Power", 
								   			"Voltage", 
								   			"Global Intensity",
								   			"Sub Metering 1",
								   			"Sub Metering 2",
								   			"Sub Metering 3"),
								   na.strings="?",
								   colClasses=classes,
								   skip=startReadingAt, 
								   nrows=numLinesToRead,
								   stringsAsFactors=F)
	
	# Convert $Date from 'character' class to 'Date' class
	powerConsumption$Date <-as.Date(powerConsumption$Date, format="%d/%m/%Y")
	
	# Determine locations of x-axis tick marks
	midPointTick <- nrow(powerConsumption)/2
	endPointTick <-nrow(powerConsumption)	
	
	# set png driver as the active graphics device
	png("plot4.png", width=480, height=480)
	
	# set the plot window to draw plots in row order result will be plots in 2x2 
	# type mattrix.  Set margins (bottom, left, top, right)
	par(mfrow=c(2,2), mar=c(5,4,5,1.5)) 
	
	# Plot 1 in top left - position (1,1)
	# Make lines plot showing the global minute-averaged active power usage, with 
	# appropriate y axis labels.  
	with(powerConsumption, plot(Global.Active.Power, type="l", ylab="Global Active Power", xlab="", xaxt="n"))
	axis(side=1, at=c(1, midPointTick, endPointTick), labels=c("Thu", "Fri", "Sat"))

	# Plot 2 in top right - position (1,2)
    # Make lines plot showing the voltage usage with appropriate y-axis label.
	with(powerConsumption, plot(Voltage, type="l", ylab="Voltage", xlab="datetime", xaxt="n"))
	# Annotate Plot 3 x-axis with specific tick mark locations and tick mark 
	# labels on the x-axis.	
	axis(side=1, at=c(1, midPointTick, endPointTick), labels=c("Thu", "Fri", "Sat"))
	
	# Plot 3 in bottom left - position (2,1)
	# Make lines plot showing three lines for each of the sub_metering_# active energy usage. 
	# Line 1 is the default color blank, line 2 is red, and line 3 is blue.	
	with(powerConsumption, plot(Sub.Metering.1, type="l", ylab="Energy sub metering", xlab="", xaxt="n"))
	points(powerConsumption$Sub.Metering.2, type="l", col="red")
	points(powerConsumption$Sub.Metering.3, type="l", col="blue")
	# Annotate Plot 3 x-axis with specific tick mark locations and tick mark 
	# labels on the x-axis.		
	axis(side=1, at=c(1, midPointTick, endPointTick), labels=c("Thu", "Fri", "Sat"))
	# Annotate Plot 3 with the a legend in the top righthand corner of 
	# the plot.	
	legend("topright", lty=c(1,1,1), col=c("black","red","blue"), 
		   legend=c("Sub_metering_1 ","Sub_metering_2 ","Sub_metering_3 "),
		   bty="n")
	
	# Plot 4 in bottom right - position (2,2)
	# Make lines plot showing the global minute-averaged reactive power usage, 
	# with appropriate y axis labels.  
	with(powerConsumption, plot(Global.Reactive.Power, type="l", ylab="Global_reactive_power", xlab="datetime", xaxt="n"))
	# Annotate Plot 4 x-axis with specific tick mark locations and tick mark 
	# labels on the x-axis.	
	axis(side=1, at=c(1, midPointTick, endPointTick), labels=c("Thu", "Fri", "Sat"))
	
	dev.off()
}