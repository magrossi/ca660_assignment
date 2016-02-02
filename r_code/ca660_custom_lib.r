library(zoo)
library(lubridate)

# To import use: source("functions.txt")
# Example:
# 1) Read from CSO (px) file:
# mydata <- data.frame(read.px("filename"))
# 2) Filter the data that you want
# filtered_data <- mydata[mydata$Area == "National" & mydata$OtherFilter == "Other Stuff", ]
# 3) Pre-process CSO
# ppd <- pre.process.cso.data(filtered_data, value_column = "value", date_column = "Year")
# >> ppd will be a yearly time series with all years populated (by interpolation) <<
# >> you can use ppd to do you time series analysis <<
# Use: coredata(x) on the result of the function to get a normal ts

#
# Pre-processes CSO data frame that has a date and value column
#
pre.process.cso.data <- function(x, value_column, date_column, start = NULL, end = NULL) {
  # Transform input data in standard format
  df <- data.frame(date=as.Date(paste(levels(x[, date_column]), "0101", sep = ""), format="%Y%m%d"), value=x[, value_column])

  # Calculate start and end period
  if (is.null(start))
    start <- min(df$date)
  if (is.null(end))
    end <- max(df$date)

  # Full year list
  years <- seq.Date(start, end, "year")

  # All values list
  values <- merge(x = data.frame(date=years), y = df, all.x=TRUE)
  
  # Zoo values
  zoo_values <- zoo(values$value, values$date)
  
  # Calculate approximated values by linear interpolation
  zoo_apprx <- na.approx(zoo_values)

  # Convert to time series data
  ts_start = as.numeric(format(start, "%Y"))

  # Return time series data
  return(ts(coredata(zoo_apprx), start = ts_start, frequency = 1))
}

