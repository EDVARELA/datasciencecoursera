################################################################################
################################################################################


# Functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Download the raw data sets to use in this assignment.
#
# This function is aware of the OS in order to set the correct download method.
#
# @param dataSetsURL  character(1) The URL of the data sets.
# @param dataSetsPath character(1) The file system path to use in order to save
#                                  the data sets on disk.
downloadDataSets <- function(dataSetsURL, dataSetsPath) {
  downloadMethod <- if (Sys.info()['sysname'] == 'Windows') {
    'auto'
  } else {
    'curl'
  }

  download.file(dataSetsURL, dataSetsPath, downloadMethod)
}


# Read a data set located inside the given zip file.
#
# @param zipFilePath    character(1) The path to the zip file.
# @param zipDataSetPath character(1) The path to the data set inside the zip
#                                    file.
# @param ... Parameters to be passed to the `read.table` function
#
# @return data.frame A data frame with the indicated data set.
readZippedDataSet <- function(zipFilePath, zipDataSetPath, ...) {
  read.table(unz(zipFilePath, zipDataSetPath), ...)
}


# Build the first graph
#
# @param dataSet data.frame The data set from which the graph will be build
buildGraph1 <- function(dataSet) {
  with(dataSet, plot(
    DateTime,
    Global_active_power,
    main='',
    type='l',
    xlab='',
    ylab='Global Active Power'
  ))
}


# Build the second graph
#
# @param dataSet data.frame The data set from which the graph will be build
buildGraph2 <- function(dataSet) {
  with(dataSet, plot(
    DateTime,
    Sub_metering_1,
    main='',
    type='n',
    xlab='',
    ylab='Energy sub metering'
  ))

  with(dataSet, points(
   DateTime, Sub_metering_1, type='l'
  ))

  with(dataSet, points(
   DateTime, Sub_metering_2, type='l', col='red'
  ))

  with(dataSet, points(
   DateTime, Sub_metering_3, type='l', col='blue'
  ))

  legend(
    'topright',
    c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'),
    bty='n',
    lty=c(1, 1, 1),
    col=c('black', 'red', 'blue')
  )
}


# Build the third graph
#
# @param dataSet data.frame The data set from which the graph will be build
buildGraph3 <- function(dataSet) {
  with(dataSet, plot(
    DateTime,
    Voltage,
    main='',
    type='l',
    xlab='datetime',
    ylab='Voltage'
  ))
}


# Build the forth graph
#
# @param dataSet data.frame The data set from which the graph will be build
buildGraph4 <- function(dataSet) {
  with(dataSet, plot(
    DateTime,
    Global_reactive_power,
    main='',
    type='l',
    xlab='datetime',
    ylab='Global_reactive_power'
  ))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Main ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Paths & URLs +++++++++++++++++++++++++++++++++++++++++++++
dataDirPath <- file.path('.', 'data')
dataSetsPath <- file.path('.', dataDirPath, 'datasets.zip')
dataSetsURL <- 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip'
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Download the data sets to disk if it's necessary +++++++++
if (!file.exists(dataSetsPath)) {
  if (!file.exists(dataDirPath)) {
    dir.create(dataDirPath)
  }

  print('[INFO] Downloading the raw data sets...')

  downloadDataSets(dataSetsURL, dataSetsPath)
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Load the data sets into memory +++++++++++++++++++++++++++
print('[INFO] Loading the raw data sets into memory...')

electricPowerConsumption <- readZippedDataSet(
  dataSetsPath,
  'household_power_consumption.txt',
  col.names=c(
    'Date',
    'Time',
    'Global_active_power',
    'Global_reactive_power',
    'Voltage',
    'Global_intensity',
    'Sub_metering_1',
    'Sub_metering_2',
    'Sub_metering_3'
  ),
  sep=';',
  skip=66637,
  nrows=2880
)
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Transform the Date and Time variables into a single one ++
print('[INFO] Tidying the raw data sets...')

dateTimeVector <- mapply(function(strDate, strTime) {
  as.POSIXct(strptime(paste(' ', strDate, strTime), '%e/%m/%Y %H:%M:%S'))
}, electricPowerConsumption$Date, electricPowerConsumption$Time)

electricPowerConsumption <- data.frame(
  DateTime=as.POSIXct(dateTimeVector, origin='1970-01-01'),
  electricPowerConsumption[, 3:ncol(electricPowerConsumption)]
)
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Build the graph ++++++++++++++++++++++++++++++++++++++++++
print('[INFO] Building the graph...')

# Open a new file device
png(filename='plot4.png')

par(mfcol=c(2,2), mar=c(4, 4, 2, 2), oma=c(1, 1, 1, 1))
buildGraph1(electricPowerConsumption)
buildGraph2(electricPowerConsumption)
buildGraph3(electricPowerConsumption)
buildGraph4(electricPowerConsumption)

# Close the file device
dev.off()
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
