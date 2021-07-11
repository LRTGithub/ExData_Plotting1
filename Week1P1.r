#    Date: Date in format dd/mm/yyyy
#    Time: time in format hh:mm:ss
#    Global_active_power: household global minute-averaged active power (in kilowatt)
#    Global_reactive_power: household global minute-averaged reactive power (in kilowatt)
#    Voltage: minute-averaged voltage (in volt)
#    Global_intensity: household global minute-averaged current intensity (in ampere)
#    Sub_metering_1: energy sub-metering No. 1 (in watt-hour of active energy). It corresponds to the kitchen, containing mainly a dishwasher, an oven and a microwave (hot plates are not electric but gas powered).
#    Sub_metering_2: energy sub-metering No. 2 (in watt-hour of active energy). It corresponds to the laundry room, containing a washing-machine, a tumble-drier, a refrigerator and a light.
#    Sub_metering_3: energy sub-metering No. 3 (in watt-hour of active energy). It corresponds to an electric water-heater and an air-conditioner.

#Criteria
#    Was a valid GitHub URL containing a git repository submitted?
#    Does the GitHub repository contain at least one commit beyond the original fork?
#    Please examine the plot files in the GitHub repository. Do the plot files appear to be of the correct graphics file format?
#    Does each plot appear correct?
#    Does each set of R code appear to create the reference plot?

#Loading the data

#When loading the dataset into R, please consider the following:

#    The dataset has 2,075,259 rows and 9 columns. First calculate a rough estimate of how much memory the dataset will require in memory before reading into R. Make sure your computer has enough memory (most modern computers should be fine).
#    We will only be using data from the dates 2007-02-01 and 2007-02-02. One alternative is to read the data from just those dates rather than reading in the entire dataset and subsetting to those dates.
#    You may find it useful to convert the Date and Time variables to Date/Time classes in R using the strptime()\color{red}{\verb|strptime()|}strptime()  and as.Date()\color{red}{\verb|as.Date()|}as.Date() functions.
#    Note that in this dataset missing values are coded as ?\color{red}{\verb|?|}?.

#Making Plots

#Our overall goal here is simply to examine how household energy usage varies over a 2-day period in February, 2007. 
#Your task is to reconstruct the following plots below, all of which were constructed using the base plotting system.

#First you will need to fork and clone the following GitHub repository: https://github.com/rdpeng/ExData_Plotting1

#For each plot you should
#    Construct the plot and save it to a PNG file with a width of 480 pixels and a height of 480 pixels.
#    Name each of the plot files as plot1.png\color{red}{\verb|plot1.png|}plot1.png, 
#    plot2.png\color{red}{\verb|plot2.png|}plot2.png, etc.
#    Create a separate R code file (plot1.R\color{red}{\verb|plot1.R|}plot1.R, plot2.R\color{red}{\verb|plot2.R|}plot2.R, etc.) that 
#    constructs the corresponding plot, i.e. code in plot1.R\color{red}{\verb|plot1.R|}plot1.R constructs the plot1.png\color{red}{\verb|plot1.png|}plot1.png plot. 
#    Your code file should include code for reading the data so that the plot can be fully reproduced. 
#    You must also include the code that creates the PNG file.
#    Add the PNG file and R code file to the top-level folder of your git repository (no need for separate sub-folders)

#When you are finished with the assignment, push your git repository to GitHub so that the GitHub version of your repository is up to date. There should be four PNG files and four R code files, a total of eight files in the top-level folder of the repo.

#The four plots that you will need to construct are shown below.

library( data.table )
#library( lubridate )
# STEP 1: READ THE txt DATA FILE.-
#==================================
DataSetFull <- read.table( "household_power_consumption.txt", sep = ";", dec = ",", header = TRUE )

# STEP 2: FILTER OUT THE TWO DAYS WE NEED.-
#===========================================
DataSetFull <- DataSetFull[ ( ( DataSetFull$Date == "1/2/2007" | DataSetFull$Date == "2/2/2007" ) & DataSetFull$Date != "?" ), ]
# REMOVE NAs.
DataSetFull[ DataSetFull == "?" ] <- "NA"
removeV <- is.na( DataSetFull ) 
dim( removeV )
# change date formats.
DataSetFull[ , 1 ] <- as.Date( DataSetFull[ , 1 ], format = "%d/%m/%Y" )
# Convert dates to weekdays.
DataSetFull$Weekday <- weekdays( DataSetFull$Date)                 
DataSetFull$xSeq <-  seq.int( nrow( DataSetFull ) )
# Convert strings to numbers.
for ( j in 3:9){
        DataSetFull[ , j ] <- as.numeric( DataSetFull[ , j ] )
}
head( DataSetFull )
cat( "\n")
tail( DataSetFull )
cat( "\n")
dim( DataSetFull )
cat( "\n")
str( DataSetFull )
cat( "\n")
#summary( DataSetFull )
#cat( "\n")
names( DataSetFull )
cat( "\n")


#STEP 3: FIRST CHART HISTOGRAM
#==============================
png( "graph1hist.png", width = 480, height = 480, unit = "px" )
hist( DataSetFull$Global_active_power, main = "Global Active Power", xlab = "Global Active Power (kilowatts)", ylab = "Frequency", col = "red", breaks = 12 )
dev.off()

#STEP 4: SECOND CHART Global Active Power vs Weekday
#===================================================
png( "graph2line.png", width = 480, height = 480, unit = "px" )
with( DataSetFull, plot( xSeq, Global_active_power, xaxt = "n", type = "l", main = "Global Active Power", ylab = "Global Active Power (kilowatts)", xlab = "Weekday" ) )
Axis( x = DataSetFull$xSeq, at = c( 0, 1440, 2880), side = 1, labels = c( "Thursday", "Friday", "Saturday" ) )
dev.off()

#STEP 5: THIRD CHART Energy sub metering vs Weekday
#===================================================
png( "graph3line.png", width = 480, height = 480, unit = "px" )
with( DataSetFull, plot( xSeq, Sub_metering_1, xaxt = "n", col = "black", type = "l",  ylab = "Energy sub metering", xlab = "" ) )
with( DataSetFull, lines( xSeq, Sub_metering_2, col = "red" ) )
with( DataSetFull, lines( xSeq, Sub_metering_3, col = "blue" ) )

Axis( x = DataSetFull$xSeq, at = c( 0, 1440, 2880), side = 1, labels = c( "Thursday", "Friday", "Saturday" ) )
legend(  x = "topright", legend = c( "Sub_metering_1", "Sub_metering_2","Sub_metering_3" ), lty = c(1,1,1), col = c( 1,2,4), lwd = 2 )
dev.off()

#STEP 6: FOURTH CHART 
#===================================================
png( "graph4line.png", width = 480, height = 480, unit = "px" )
par( mfrow = c( 2, 2) )
with( DataSetFull, { 
                # graph row 1 col 1
                plot( xSeq, Global_active_power, xaxt = "n", col = "black", type = "l",  ylab = "Global Active Power", xlab = "" )
                Axis( x = DataSetFull$xSeq, at = c( 0, 1440, 2880), side = 1, labels = c( "Thursday", "Friday", "Saturday" ) )
                # graph row 1 col 2
                plot( xSeq, Voltage, xaxt = "n", col = "black", type = "l",  ylab = "Global Active Power", xlab = "datetime" )
                Axis( x = DataSetFull$xSeq, at = c( 0, 1440, 2880), side = 1, labels = c( "Thursday", "Friday", "Saturday" ) )
                # graph row 2 col 1
                plot( xSeq, Sub_metering_1, xaxt = "n", col = "black", type = "l",  ylab = "Energy sub metering", xlab = "" )
                lines( xSeq, Sub_metering_2, col = "red" )
                lines( xSeq, Sub_metering_3, col = "blue" )
                Axis( x = DataSetFull$xSeq, at = c( 0, 1440, 2880), side = 1, labels = c( "Thursday", "Friday", "Saturday" ) )
                legend(  x = "topright", legend = c( "Sub_metering_1", "Sub_metering_2","Sub_metering_3" ), lty = c(1,1,1), col = c( 1,2,4), lwd = 2 )
                # graph row 2 col 2
                plot( xSeq, Global_reactive_power, xaxt = "n", col = "black", type = "l",  ylab = "Global_reactive_power", xlab = "datetime" )
                Axis( x = DataSetFull$xSeq, at = c( 0, 1440, 2880), side = 1, labels = c( "Thursday", "Friday", "Saturday" ) )
                }
)
dev.off()
