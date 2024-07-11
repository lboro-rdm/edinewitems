library(shiny)
library(DT)
library(lubridate)
library(shinycssloaders)

source("script.R")
source("global.R")

# Define UI for application that draws a table
ui <- fluidPage(
  # Application title
  titlePanel("New EDI items in the repo"),
  p("Keywords used: race, disability, discrimination, colonialism, racism, 
    impairment, dyscalculia, equality, equity, EDI, LGBT, LGBT+, queer, lesbian, 
    disabled, fibromyalgia, autism, neurodiversity, gay, transgender, Mental health,
    schizophrenia, PTSD, ableism, epilepsy, stroke, anxiety, depression, syndrome,
    colonialism, feminism, LGBTQ+, gender, intersectionality, racial, feminism, 
    BAME, whiteness"),
  
  # Input control for start date
  sidebarPanel(
    dateInput("start_date", "Start Date:", value = Sys.Date() - days(30), max = Sys.Date()) # Restrict date input
  ),
  
  # CSS to set the background color and font size
  tags$head(
    tags$style(HTML("
      body {
        background-color: #FEFAF5;
        font-size: 16px;
      }
    "))
  ),
  
  # Show a table of the column with a spinner
  mainPanel(
    withSpinner(DTOutput("citations_table"))
  )
)

# Define server logic
server <- function(input, output, session) {
  # Define a reactive value to store the fetched data
  fetched_data <- reactiveVal(NULL)
  
  # Function to fetch API data
  observeEvent(input$start_date, {
    start_date <- input$start_date
    fetched_data(fetch_api_data(start_date))
  })
  
  # Render the table when fetched_data is updated
  output$citations_table <- renderDT({
    fetched_df <- fetched_data()
    if (!is.null(fetched_df)) {
      df <- data.frame(Citation = paste0('<a href="', fetched_df$URL, '" target="_blank">', fetched_df$Citation, '</a>'))
      datatable(df, escape = FALSE, options = list(
        dom = 't',
        paging = FALSE,
        searching = FALSE,
        columnDefs = list(list(
          targets = 0,
          render = JS(
            'function(data, type, row, meta) {',
            'return type === "display" ? data : data;', # Prevent escaping HTML
            '}'
          )
        ))
      ))
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
