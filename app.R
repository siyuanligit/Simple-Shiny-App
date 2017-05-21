#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

# loading required package
library(shiny)
library(readr)
library(plyr)
library(ggplot2)


# download access log
system("curl -s http://users.csc.tntech.edu/~elbrown/access_log.bz2 > log")

# read in data using read.table
suppressWarnings(data <- read.table("log", fill = TRUE, header = FALSE, stringsAsFactors = FALSE))

# naming
names(data) <- c("ip", "iden", "authusr", "datetime", "timezone", "request", "status", "bytes", "webpage", "system")

# transform datetime and timezone
data$datetime <- as.POSIXct(data$datetime, format = "[%d/%b/%Y:%H:%M:%S") + (as.double(substr(data$timezone, start = 2, stop = 5)) - 600)*36
data$timezone <- NULL

# count table which includes frequency
frequency_date <- count(data, 'datetime')


# UI
ui <- navbarPage("Access Log Exploration:",
  
  # initiate tab panel 1
  tabPanel("Time Selection Filtering",
    
    # sidebar layout
    sidebarLayout(
      sidebarPanel(
        
        # slider to select range of time to display
        sliderInput("range", "Select the range of entries:",
          min = 1, max = length(frequency_date$freq), value = c(30000,60000)),
        
        # a button, click to plot
        actionButton("plotbutton", "Click to Show Plot", class = "btn-primary")
      ),
      
      # output plot in main panel
      mainPanel(
        
        plotOutput("time_plot")
        
      )
    )
  ),
  
  # initiate tab panel 2
  tabPanel("Complete Data Filtering",
    
    # sidebar layout
    sidebarLayout(
      sidebarPanel(
        
        # select the status you want to filter
        selectInput("option_status", "Status filters:", 
          choices = c("200", "404", "non-404"))
        
      ),
      
      # output plot in main panel
      mainPanel(
        
        plotOutput("complete_plot"),
        
        # also plot a non-404 status histogram
        plotOutput("filter_plot")
        
      )
    )
  )
)

# SERVER
server <- function(input, output) {
  
  # define button action
  plot_freq_date <- eventReactive(input$plotbutton, {
    
    # define output graph range
    x <- input$range[1]:input$range[2]
    
    # find the selected range of date and time
    start_range <- substr(as.character(frequency_date$datetime[input$range[1]]), 1, 20)
    end_range <- substr(as.character(frequency_date$datetime[input$range[2]]), 1, 20)
    plot_title <- paste("Access activity from ", start_range, " to ", end_range, " CST")
    
    # plot graph
    suppressWarnings(
      ggplot(frequency_date[x,], aes(x = datetime, y = freq)) + 
        geom_line(aes(color = freq)) +
        geom_point(aes(color = freq)) +
        scale_colour_gradient2(low = "grey", mid = "green" , high = "red", midpoint = 5) +
        xlab("Datetime") +
        ylab("Frequency") +
        ggtitle(plot_title))
    
  })
  
  # plot complete plot based on filter
  output$complete_plot <- renderPlot({
    
    # 200 status
    if (input$option_status == "200"){
      suppressWarnings(
        ggplot(
          data[which(as.character(data$status) == "200"),], 
          aes(x = datetime)
        ) + 
          geom_freqpoly(binwidth = 86400) +
          xlab("Time") +
          ylab("Frequency"))
    } 
    
    # 400 status
    else if (input$option_status == "404"){
      suppressWarnings(
        ggplot(
          data[which(as.character(data$status) == "404"),], 
          aes(x = datetime)
        ) + 
          geom_freqpoly(binwidth = 86400) +
          xlab("Time") +
          ylab("Frequency"))
    }
    
    # non-404 status
    else {
      suppressWarnings(
        ggplot(
          data[which(as.character(data$status) != "404"),], 
          aes(x = datetime)
        ) + 
          geom_freqpoly(binwidth = 86400) +
          xlab("Time") +
          ylab("Frequency"))
    }
    
  })
  
  # show the histogram of different status if non-404 is selected
  output$filter_plot <- renderPlot({
    
    if (input$option_status == "non-404"){
      suppressWarnings(
        ggplot(data[which(as.character(data$status) != "404" & 
            nchar(as.character(data$status)) < 4 & 
            nchar(as.character(data$status)) > 1),], 
          aes(x = status)) + 
          geom_histogram(aes(fill = status), stat = "count") +
          xlab("Status Type") +
          ylab("Frequency"))
    }
    
  })
  
  # plot the line chart based on button reaction
  output$time_plot <- renderPlot({
    
    plot_freq_date()
    
  })
  
}

# run application 
shinyApp(ui = ui, server = server)

