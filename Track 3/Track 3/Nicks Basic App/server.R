library(shiny)
library(dplyr)
library(ggplot2)
  

google_data = read.csv(file   =  "http://www.google.org/flutrends/about/data/flu/us/data.txt", 
                       header = TRUE,       # Data contain headers
                       skip   = 10)         # Skip the first 10 lines

google_data$Date = as.Date(google_data$Date)

# The shiny server will take user input information and use that to create our plot
shinyServer(function(input, output) {
  
  # This is a function which filters our google_data information to only return data for
  # the location that has been chosen and the dates that have been chosen
  selectedData = reactive({

    #  Select only data from the chosen location
     selected_data = select_(google_data, Location = input$location, "Date") 
    #  Filter data so that only the date range we want is returned 
     filtered_data = filter(selected_data, Date >= input$dateRange[1] & Date <= input$dateRange[2] )
     
     # Finally, we return the data that we have selected
     return(filtered_data)
  })

  # Once we have selected our data, we can make our plot. We call our plot "google_plot", the same name that is 
  # referenced in the ui.R file
  output$google_plot = renderPlot({
    p = ggplot(data = selectedData(), 
           aes(x = Date, 
               y = Location)) + 
      geom_line() + 
      labs(y ='Flutrends Indicator', title = input$location)
    
    return(p)
  })
})
    
   
