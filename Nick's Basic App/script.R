library(dplyr)
library(ggplot2)
# Read in google flu data from google website.
# The data is stored in a .csv format (even though it is named .txt), and
#
# This data file has a column for the date, and then separate columns for each location.  
google_data = read.csv(file        = "http://www.google.org/flutrends/about/data/flu/us/data.txt", 
                       header      = TRUE, 
                       skip        = 10)   # first 10 lines are google disclaimer

google_data$Date = as.Date(google_data$Date)

# Choose location and dates
location = "Iowa"
dates = c("2014-07-01","2015-06-30")

# Extract data for desired location

# First we take our data table and select only two columns from it: 
# 1. The location column corresponding to the location that we chose above
# 2. The Date column, which we will need to plot our data

selected_data = select_(.data = google_data, 
                         Location = location, 
                         "Date") 

# Next, we filter the data so that only dates that fall within the user's selected date range (dates[1] and dates[2]) are returned.
filtered_data = filter(selected_data, 
                       Date >=dates[1] & Date <= dates[2] )


# Plot data
# To use the ggplot function (part of the ggplot2 library), specify:
# the x variable (here, Date), 
# the y variable( here, Location)
# the data that you are using to make the plot.  Our data is the filtered_data we created above

ggplot(data = filtered_data, 
       aes(x = Date, 
           y = Location)) + 
  geom_line() + # add geom_line() to make a line connecting the data points in our plot
  labs(y ='', title = location) # set labels for the axes and the title of the plot

