library(shiny)
library(tidyverse)
library(ggplot2)

data <- read_delim("Rail_Equipment_Accident_Incident_Data.csv")

ui <- fluidPage(
  titlePanel("Info 201 Final project"),
  tabsetPanel(
    tabPanel(
      "Home",
      img(src="https://media.npr.org/assets/img/2023/02/05/ap23035653276274-f6d38396bc1cd8b060e6801e2506f98b047f62bf-s800-c85.webp",height=400,width=400),
      h1("Project Overview"),
      p("The railroad industry is a crucial part of transportation infrastructure, but it is not without risk. Railroad accidents and incidents can result in significant harm to human life, the environment, and the economy. Understanding the patterns and causes of these incidents is critical for improving safety and preventing future accidents.
        This project aims to explore the available railroad accident and incident data and use statistical analysis techniques to gain insights into the factors that influence these events."),
      h2("Audience"),
      p("The audience for this project may include individuals and organizations involved in the railroad industry, such as railroad companies, regulators, and safety organizations. It may also include researchers and analysts interested in transportation safety or statistical analysis of complex datasets. The insights gained from this project 
        may be relevant to policymakers and decision-makers involved in transportation safety at the local, national, or international level."),
      h2("Question"),
      p("Some questions we are focues on are:"),
      tags$ul(
        tags$li("What are the leading causes of accidents?"),
        tags$li("How does weather affects accidents?"),
        tags$li("What is the cost impact of accidents?")
      ),
      h2("Railroad Accident & Incident Data"),
      p("Safety data related to Railway Equipment Failures and Accidents"),
      p("We will analyze railroad accident data to determine causes and impacts of railroad accidents."),
      a("https://www.kaggle.com/datasets/chrico03/railroad-accident-and-incident-data"),
      p("Dataset published by the Federal Railroad Administration, Office of Railroad Safety; contains data on railway incidents from 1975 to 2022."),
      p("Includes data on:"),
      tags$ul(
        tags$li("Railway company involved"),
        tags$li("Hazmat cars involved"),
        tags$li("Number of people evacuated"),
        tags$li("Number of employees on-duty")
      ),
      textOutput("nrow"),
      textOutput("ncol"),
      p(""),
      strong("Sample data"),
      tags$div(style="height: 600px; overflow-x: scroll; width: 1200px; overflow-y: scroll;", tableOutput("sample_table")),
      h2("Creators"),
      tags$ul(
        tags$li("Karina Yang"),
        tags$li("Caleb Lee"),
        tags$li("Lily Bates"),
        tags$li("Joe Wicorek")
      ),
    ),
    
    tabPanel(
      "Accident Causes",
      mainPanel(
        # Sidebar with inputs
        sidebarLayout(
          sidebarPanel(
            uiOutput("sortedcauses"),
          ),
          
          # Main panel with output
          mainPanel(
            # Plot output
            plotOutput(outputId = "line_plot", height=800,width=1600),
            p(" "),
            p("This graph allows the exploration of accident causes. 
            Specifically this shows the cumulative count of accidents by
            cause. The graph shows that top 3 causes of accidents are
            are related to switches, broken rails, and showing movemet.")
          )
        )
      )
    ),
    
    tabPanel(
      "Weather and Visibility Trends",
      sidebarLayout(
        sidebarPanel(
          p("Observe the effect on weather on incidents."),
          radioButtons("weather", 
                       "Choose weather type to veiw",
                       choices = unique(data$`Weather Condition`),
                       selected = "Clear"), ## button for selecting weather
          radioButtons("vis_button",
                       "Display time of day accident occured?",
                       choices = c("Yes", "No"),
                       selected = "Yes") ## button for selecting time of day
        ),
        mainPanel(
          plotOutput("weather_graph"), ## graph output from user input
          textOutput("weatherobs"), ## note about how many observations user is seeing
          p(" "),
          p("This graph shows that the majority of crashes happen in clear weather
            condtions and when it is dark and visibility is low. Overall,
            the most common type of crash is a derailment followed by 
            highway/railroad crossings. During clear weather, more accidents 
            happen in the daytime compared to other weather types. Though this 
            coorelation may be due to the high prevelance of clear weather, it may 
            also suggest that less caution is taken in clear, high visibility
            conditions, leading to more accidents.")
        ))
    ), 
    
    
    tabPanel(
      "Track Type and State Analysis",
      sidebarLayout(
        sidebarPanel(
          p("Observe the count of accidents based on States and Track Type"),
          radioButtons("state", 
                       "Choose which State to view",
                       choices = unique(data$`State Name`),
                       selected = "ILLINOIS"), ## button for selecting weather
        ),
        mainPanel(
          plotOutput("state_graph"),
          textOutput("totalCount"),
          p(""),
          p("This panel allows users to explore railway incidents and accidents data. Specifically, this panel enables users to analyze the count of accidents incurred by railway incidents in different states, depending on the type of track involved. The panel includes a sidebar panel where users can select the state they want to examine, and the main panel displays a histogram that shows the distribution of incidents by track type. The output also provides a text summary of the total number of incidents in the selected state. This panel is part of a larger Shiny application that allows users to explore various aspects of railway incidents and accidents data.")
        )
      )),
    tabPanel(
      "Conclusion",
      mainPanel(
        img(src = "https://www.linkpicture.com/q/conclusion_1.jpg", height = "200px", width = "250px"),
        p("In conclusion, one notable insight we found from the dataset is that the number of railroad accidents and incidents has been declining steadily over the past few decades, despite an increase in rail traffic. This could be attributed to improved safety regulations and advancements in technology."),
        p("Based on the dataset provided, it seems that weather conditions such as heavy rain, snow, fog, and high winds can have a significant impact on railroad accidents and incidents. These weather conditions can lead to reduced visibility, slippery tracks, and decreased braking efficiency, which can increase
          the risk of accidents and incidents."),
        p("The cost impact of railroad accidents can be quite substantial, with direct costs including property damage, medical expenses, and legal fees, as well as indirect costs such as lost productivity and reputation damage."),
        p("We believe continued investment in safety measures and technology could lead to further improvements in rail safety and a reduction in accidents and incidents."),
        p("In terms of data quality, the dataset appears to be of reasonable quality, with a large amount of data spanning several decades. However, there may be issues with underreporting or inaccuracies in the data that could impact the results."),
        p("To advance the project, we can analyze the impact of other factors such as train speed and maintenance practices. Additionally, incorporating data on near-miss incidents and near-collisions could provide valuable insights into potential safety risks.")
      )
    ), 
  ))


server <- function(input, output) {
  
  #calc the number of rows and columns and their output functions
  nrow <- nrow(data)
  output$nrow <- renderText(paste("The number of rows is:", nrow))
  ncol <- ncol(data)
  output$ncol <- renderText(paste("The number of columns is:", ncol))
  
  #get some data to display with output function
  sample_data <- data[sample(nrow(data), 10), ]
  output$sample_table <- renderTable(sample_data, options = list(scrollX = TRUE, scrollY = "300px"))  
  
  choices <- data %>%
    filter(`Report Year` >= 2000)
  choices <- choices %>%
    mutate(year = substr(Date,nchar(Date)-3,nchar(Date))) %>%
    group_by(year, `Accident Cause`) %>%
    summarise(count = n()) %>%
    group_by(`Accident Cause`) %>%
    mutate(cum_count = cumsum(count)) %>%
    filter(cum_count > 1000) %>%
    arrange(`Accident Cause`)
  sorted_choices <- choices$`Accident Cause`
  
  # Select input for causes to plot
  output$sortedcauses <- renderUI({
    selectInput("cause",
                label = "Select Multiple Causes to plot",
                choices = sorted_choices,
                selected = sorted_choices[1],
                multiple = TRUE)
  })
  
  # Reactive expression for filtered data by cause
  filtered_data <- reactive({
    accident_cause_data <- data %>% 
      filter(`Report Year` >= 2000) %>%
      mutate(year = substr(Date,nchar(Date)-3,nchar(Date))) %>%
      filter(`Accident Cause` %in% input$cause) %>%
      group_by(year, `Accident Cause`) %>%
      summarise(count = n()) %>%
      group_by(`Accident Cause`) %>%
      mutate(cum_count = cumsum(count))
  })
  
  # Render plot output
  output$line_plot <- renderPlot({
    # Plot the cumulative count by cause and year using ggplot2
    ggplot(filtered_data(), aes(x = year, y = cum_count, color = `Accident Cause`, group=`Accident Cause`)) +
      geom_line(aes(group=`Accident Cause`)) +
      labs(title = "Cumulative count of accidents by cause and year",
           x = "Year",
           y = "Cumulative count") +
      theme_minimal() +
      theme(legend.position = "right",
            legend.direction = "vertical",
            legend.spacing.y = unit(0.5, "cm"),
            axis.text = element_text(size = 16),
            axis.title = element_text(size = 18),
            plot.title = element_text(size = 18),
            legend.text = element_text(size = 11))
  })
  
  weathergraph <- reactive({
    data %>% 
      filter(`Weather Condition` %in% input$weather) %>% 
      filter(`Report Year` == 2022)
  })  ## weather crash graph 
  
  output$weather_graph <- renderPlot ({
    if(input$vis_button == "Yes")({
      ggplot(weathergraph(), (aes (y= `Accident Type`, fill = factor(Visibility))))+
        geom_histogram(stat = "count")
    })else ({
      ggplot(weathergraph(), (aes (y= `Accident Type`)))+
        geom_histogram(stat = "count")
    }) 
  }) ## time of day button for weather crash graph 
  
  output$weatherobs <- renderPrint({
    data %>% 
      filter(`Weather Condition` %in% input$weather) %>% 
      filter(`Report Year` == 2022) %>%  
      nrow() %>% 
      cat("There were", . , "total collisions in the selected weather type.")
  }) ## code for text output of how many observations user is seeing in weather crash graph
  
  stategraph <- reactive({
    data %>% 
      filter(`State Name` %in% input$state) %>% 
      filter(`Report Year` >= 1990)
  }) 
  
  output$state_graph <- renderPlot ({
    ggplot(stategraph(), (aes (x = `Track Type`)))+
      geom_histogram(stat = "count")
  })
  
  output$totalCount <- renderPrint({
    data %>% 
      filter(`State Name` %in% input$state) %>% 
      filter(`Report Year` >= 1990) %>%  
      nrow() %>% 
      cat("The count was", . , " in this state.")
  })
}


shinyApp(ui = ui, server = server)
