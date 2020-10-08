# Measure Glucose 

library(tidyverse)
library(janitor)
library(lubridate)
library(plotly)
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(dygraphs)
library(xts)
library(DT)


ui <- fluidPage(theme = "cerulean",
                navbarPage("Glucose",
                           # Start Input Tab ----
                           tabPanel("Input",
                                    fluidRow(
                                      column(3,
                                             wellPanel(id="well_input",
                                                       h4("Upload files for computation"), 
                                                       fluidRow(  fileInput("file1", "Upload text file",
                                                                            multiple = FALSE,
                                                                            accept  = c(".txt"))
                                                       ),
                                                       )
                                      ),
                                      #TabSet for displaying select files and pre-computed data tab
                                      column(9,wellPanel(    
                                        tabsetPanel(
                                          tabPanel(
                                            "One Day",
                                            br(),
                                            fluidPage(
                                              fluidRow(selectInput("choose_day","Choose Date",choices = NA)),
                                              fluidRow(dygraphOutput("one_day_plot") %>% withSpinner())
                                            )
                                            
                                          ),
                                          tabPanel(
                                            "Summary",
                                            br(),
                                            fluidPage(
                                              fluidRow(DT::dataTableOutput("summary_table")),
                                              fluidRow(plotlyOutput("summary_plot") %>% withSpinner())
                                            )
                                            
                                          ),
                                          tabPanel(
                                            "Daily Variation",
                                            br(),
                                            fluidPage(
                                             plotOutput("all_day_plot", height = "1000px") %>% withSpinner()
                                            )
                                            
                                          )
                                          
                                        ) 
                                        
                                      ) #well input panel ends
                                      )
                                    )
                                    
                           )
                           
                )
                
                
)

server <- function(input, output, session) {
  
  observe({
    raw_data() -> raw_data1
    raw_data1 %>% 
      mutate(Month = Time %>% month(label = TRUE),
             Day = Time %>% day()) %>% 
      mutate(date = paste0(Day," ",Month)) %>% 
      pull(date) %>% 
      unique() -> choices_for_date
    
    
    updateSelectInput(session,"choose_day","Choose Date", choices = c("All",choices_for_date))
  })

    
raw_data <- reactive({
  
  req(input$file1)
  
  tryCatch(
    {
    input$file1$datapath %>% 
      read_tsv(skip = 2) %>% 
      select( "Time", "Historic Glucose (mg/dL)") %>% 
      mutate(Time = Time %>% parse_date_time(orders = "%Y-%m-%d %H:%M")) %>% 
      rename(Glucose = "Historic Glucose (mg/dL)") -> df
  },
  error = function(e) {
    # return a safeError if a parsing error occurs
    stop(safeError(e))
  }
  )
  
  return(df)
})
                
  
output$all_day_plot <- renderPlot({
    
        raw_data() %>% 
          mutate(Day = Time %>% day()) %>% 
          mutate(Day = Day %>% as_factor %>% fct_inorder()) %>% 
          ggplot(aes(Time,Glucose)) +
          geom_line(aes(color = Glucose))+
          geom_hline(yintercept = 80, 
                     color = 'darkgreen',
                     alpha = 0.5) +
          geom_hline(yintercept = 140, 
                     color = 'darkred',
                     alpha = 0.5) +
          scale_color_gradient(low= '#00ff00',
                               high = '#ff0000') +
          theme_minimal()+
          # Add nrow = number of days 
          facet_wrap(~Day, nrow = 14, scales = "free") -> my_plot
        
        my_plot 
  
  })

output$one_day_plot <- renderDygraph({
  
  raw_data() -> raw_data1
  
  if(input$choose_day == "All"){
    raw_data1 %>% 
      mutate(Month = Time %>% month(label = TRUE),
             Day = Time %>% day()) %>% 
      mutate(date = paste0(Day," ",Month)) %>% 
      mutate(ones = ifelse(Glucose > 140 | Glucose < 80, 1,0)) %>% 
      mutate(number_of_rows = row_number()) %>% 
      mutate(percentage = (sum(ones)/max(number_of_rows))*100) %>%
      select(Time, Glucose,percentage) -> my_plot
      
    my_plot %>% pull(percentage) %>% unique() -> percentage_number
    
    my_plot %>% 
      select(Time, Glucose) -> my_plot
    
  } else{
    raw_data1 %>% 
      mutate(Month = Time %>% month(label = TRUE),
             Day = Time %>% day()) %>% 
      mutate(date = paste0(Day," ",Month)) %>% 
      filter(date == input$choose_day) %>% 
      mutate(ones = ifelse(Glucose > 140 | Glucose < 80, 1,0)) %>% 
      group_by(date) %>%
      mutate(number_of_rows = row_number()) %>% 
      mutate(percentage = (sum(ones)/max(number_of_rows))*100) %>%
      select(Time, Glucose,percentage) -> my_plot 
    
    my_plot %>% pull(percentage) %>% unique() -> percentage_number
    
    my_plot %>% 
      select(Time, Glucose) -> my_plot
    
  }
  
  don = xts(my_plot,
          order.by = my_plot$Time)

  dygraph(don, main = paste0("Glucose Graph ( <small>",percentage_number %>% round(1),"% not in specified range</small> )")) %>%
  dyOptions(useDataTimezone = TRUE) %>% 
  dyRangeSelector() %>% 
  dyLegend(show = "follow") %>% 
  dyLimit(limit = 140, label = "140", labelLoc = c("left", "right"),
            color = "red", strokePattern = "solid") %>% 
    dyLimit(limit = 80, label = "80", labelLoc = c("left", "right"),
            color = "red", strokePattern = "solid") 
    

})

output$summary_table <- renderDataTable({
  raw_data() %>% 
    mutate(Month = Time %>% month(label = TRUE),
           Day = Time %>% day()) %>% 
    mutate(date = paste0(Day," ",Month)) %>% 
    mutate(date = date %>% as_factor()) %>% 
    group_by(date) %>% 
    mutate(Minimum = Glucose %>% min(),
           Maximum = Glucose %>% max(),
           Average = Glucose %>% mean()) -> my_data
  
  my_data %>% 
    filter(Glucose == Minimum) %>% 
    select(date,Time, Minimum) -> minimum_data
  
  my_data %>% 
    filter(Glucose == Maximum) %>% 
    select(date,Time, Maximum,Average) -> maximum_data
  
  minimum_data %>% 
    full_join(maximum_data, by= "date") %>%
    mutate(hr = Time.x %>% hour(),
           min = Time.x %>% minute(),
           Time.x = paste0(hr,":",min)) %>% 
    mutate(hr = Time.y %>% hour(),
           min = Time.y %>% minute(),
           Time.y = paste0(hr,":",min)) %>% 
    select(-hr,-min) %>% 
    rename("Date" = date) %>%
    mutate(Average = Average %>% round(1)) %>% 
    rename("Average Glucose" =  Average) %>%
    mutate(Minimum = paste0(Minimum," at ", Time.x)) %>% 
    mutate(Maximum = paste0(Maximum," at ", Time.y)) %>% 
    select(-Time.x, -Time.y)
})

output$summary_plot <- renderPlotly({
  
  raw_data() %>% 
  mutate(Month = Time %>% month(label = TRUE),
         Day = Time %>% day()) %>% 
    mutate(date = paste0(Day," ",Month)) %>% 
    mutate(date = date %>% as_factor()) %>% 
    group_by(date) %>% 
    summarise(Minimum = Glucose %>% min(),
              Maximum = Glucose %>% max(),
              Average = Glucose %>% mean()) %>% 
    pivot_longer(-date,
                 names_to = "Status",
                 values_to = "value") %>% 
    ggplot(aes(date, value))+
    geom_col(aes(fill = date)) +
    theme_minimal()+
    facet_wrap(~Status) -> my_plot_summary
  
  my_plot_summary %>% ggplotly()
  
})
  
}

shinyApp(ui, server)



  
  
  

  