
library(shiny)
library(tidyr)
library(ggplot2)
library(dplyr)
library(purrr)
library(stringr)
library("DataExplorer")
#source("word_cloud.r") 
# Define UI for data upload app ----
ui <- fluidPage(
  #shinythemes::themeSelector(),
  # App title ----
  titlePanel("Data Explorer"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      h6("Default Data: MTCARS"),
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      #checkboxInput("Columns",colnames(file1), TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        tabPanel("Overview", h4("Structure of Dataset"),
                 verbatimTextOutput("columns"),
                 h4("Data Description"),
                 plotOutput("data_desc"),
                 h4("Data Missing"),
                 plotOutput("data_missing"),
                 h4("Columns With Type"),
                 tableOutput("coltype"),
                 h4("Summary of Data"),
                 verbatimTextOutput("summary_data")),
        tabPanel("Data",h4("First 6 Data Points"),
                 tableOutput("head"),
                 h4("Last 6 Data Points"),
                 tableOutput("tail")),
        tabPanel("Visualization",
                  h4("Correlation"),
                    plotOutput("corrplot"),
                  h4("Histrogram"),
                    plotOutput("histoplot"),
                  h4("Density Plot"),
                    plotOutput("desityplot"),
                  h4("PCA Plot"),
                    plotOutput("pcaplot")
                )
      )
      
    )
    
  )
)
options(shiny.maxRequestSize=30*1024^2)
# Define server logic to read selected file ----
server <- function(input, output,session) {
   read <- function(input){
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
  }
  data <- reactive({
    if (length(input$file1) == 0){
      return (mtcars)
    }
    else{
      read(input)
    }
  })
  choices_column <- reactive({
    choices_column <- colnames(data)
  })
  observe({
    updateSelectInput(session = session, inputId = "column_name", choices = choices_column())
  })
 
  column_type <- function(a){
    data <-a
    textual_data <- 0
    categorical_data <-  0
    numeric_data <- 0
    unsupported_data <-0
    for (i in colnames(data)) {
      lengths <- 0
      if (class(data[[i]]) == "factor") {
        lengths <- str_count(data[[i]],pattern = "\\W+")
        #print(lengths)
        if (length(which(lengths >= 3)) > length(lengths)*0.75) {
          textual_data <- textual_data+1
        }else{
          categorical_data <- categorical_data + 1
        }
      }
      else if (class(data[[i]]) == "numeric" | class(data[[i]]) =="integer"){
        numeric_data <- numeric_data +1
        }
      else {
        unsupported_data <- unsupported_data + 1
      }
    }
    return (list(numeric_data = as.integer(numeric_data),textual_data = as.integer(textual_data),
                 categorical_data = as.integer(categorical_data) , unsupported_data = as.integer(unsupported_data)))
  }
  output$head <- renderTable(head(data()))
  output$tail <- renderTable(tail(data()))
  output$columns <- renderPrint({ 
    str(data())
  })
  output$summary_data <- renderPrint({ 
      return (data()%>%
                keep(is.numeric)%>%
                summary())
  })
  output$coltype <- renderTable({ 
  column_type(data())
  })
  output$corrplot <- renderPlot({
    return (data() %>%
      keep(is.numeric) %>% 
        na.omit()%>%
      plot_correlation(type = "c"))
  })
  output$histoplot <- renderPlot({
    return (data() %>%
              keep(is.numeric) %>% 
              na.omit()%>%
              plot_histogram())
  }) 
  output$desityplot <- renderPlot({
    return (data() %>%
              keep(is.numeric) %>% 
              na.omit()%>%
              plot_density())
  })
  output$pcaplot <- renderPlot({
    return (data() %>%
              keep(is.numeric) %>% 
              na.omit()%>%
             plot_prcomp())
  })
  
  #output$word_cloud<- renderPlot({
   # return (cloud(data[[5]]))
  #})
  output$data_desc <- renderPlot({
        return (plot_intro(data()))
  })
  output$data_missing <- renderPlot({
    return (plot_missing(data()))
  }) 
}
# Run the application 
shinyApp(ui = ui, server = server)

