
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

# Build the graph ++++++++++++++++++++++++++++++++++++++++++
print('[INFO] Building the graph...')

# Open a new file device
png(filename='plot1.png')

hist(
  electricPowerConsumption$Global_active_power,
  col='Red',
  main='Global Active Power',
  xlab='Global Active Power (kilowatts)'
)

# Close the file device
dev.off()
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

