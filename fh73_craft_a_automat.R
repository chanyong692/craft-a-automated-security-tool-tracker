# fh73_craft_a_automat.R

# Load required libraries
library(shiny)
library(ggplot2)
library(plotly)
library(reshape2)
library(dygraphs)

# UI component
ui <- fluidPage(
  titlePanel("Automated Security Tool Tracker"),
  sidebarLayout(
    sidebarPanel(
      textInput("tool_name", "Enter tool name:"),
      actionButton("add_tool", "Add Tool"),
      actionButton("del_tool", "Delete Tool"),
      hr(),
      textInput("event_name", "Enter event name:"),
      actionButton("add_event", "Add Event"),
      actionButton("del_event", "Delete Event")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Tool Tracker", 
                 dataTableOutput("tool_table")),
        tabPanel("Event Log", 
                 dygraphOutput("event_log")),
        tabPanel("Analytics", 
                 ggplotlyOutput("analytics_plot"))
      )
    )
  )
)

# Server component
server <- function(input, output) {
  # Create reactive expression to store tool data
  tools <- eventReactive(input$add_tool, {
    tool_name <- input$tool_name
    if (nchar(tool_name) > 0) {
      tools_list <- c(tool_name, tools_list)
    }
    return(tools_list)
  }, ignoreNULL = FALSE, init = character(0))
  
  # Create reactive expression to store event data
  events <- eventReactive(input$add_event, {
    event_name <- input$event_name
    if (nchar(event_name) > 0) {
      events_list <- rbind(events_list, data.frame(Event = event_name, Time = Sys.time()))
    }
    return(events_list)
  }, ignoreNULL = FALSE, init = data.frame(Event = character(0), Time = as.POSIXct(character(0))))
  
  # Output tool table
  output$tool_table <- renderDataTable({
    data.frame(Tool = tools())
  })
  
  # Output event log
  output$event_log <- renderDygraph({
    dygraph(events(), main = "Event Log") %>% 
      dyOptions(stepPlot = TRUE)
  })
  
  # Output analytics plot
  output$analytics_plot <- renderPlotly({
    events_df <- melt(events(), id.vars = "Time", variable.name = "Event")
    ggplot(events_df, aes(x = Time, fill = Event)) + 
      geom_bar() + 
      labs(x = "Time", y = "Count", fill = "Event")
  })
}

# Run the application
shinyApp(ui = ui, server = server)