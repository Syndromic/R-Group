library(shiny)


# Read in Google Flu Trends data from its website
google_data = read.csv(file   =  "http://www.google.org/flutrends/about/data/flu/us/data.txt", 
                       header = TRUE,       
                       skip   = 10) 

# Next, we use the google_data to create a list of location names.  
# The location names are the names of the columns in our data table.  
# We remove the first column, since it is the column for date, and is not a location.
loc_names = sort(names(google_data[,-1]))

# Define the UI for the google flu trends plot demo
shinyUI(fluidPage(
  
  #  Application title
  titlePanel("Google Flu Trends"),
  
  # Gives the ui a layout with a control panel on the left side and a plot next to it
  sidebarLayout(
    
    #Specify the contents of the control panel
    sidebarPanel(
      
      # Allow user to choose the location they wish to view.  
      # This will create a user input variable called "location".
      selectInput(inputId  = "location", 
                  label    = "Choose a location:", 
                  choices  = loc_names, 
                  selected = "Iowa"),
      
      # Allow user to choose the range of dates they wish to plot.  
      # This will create a user input variable called "dateRange",
      dateRangeInput(inputId = 'dateRange',
                     label   = 'Date range input: yyyy-mm-dd',
                     start   = "2013-08-01", 
                     end     = "2015-07-01") # Date when Google Flu Trends stopped providing data
    ),
    
    # Here we specify the plot to be shown to the user.  
    # Our plot is called "google_plot", and is created in the server.R file.
    mainPanel(
      plotOutput("google_plot")
    )
  )
))
