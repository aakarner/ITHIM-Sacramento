# This file is part of ITHIM Sacramento.

# File: App.R
# Purpose: Reading necessary functions for deploying the shiny.app. And shiny app implementation

# library definitions
library(shiny)
library(ggplot2)

# reading functions from external R files
# functions for physical activity
source("02_R Scripts/01_Functions_PA.R")

# functions for traffic injury
source("02_R Scripts/02_Functions_RI.R")

# functions for module integration
source("02_R Scripts/03_Functions_Integration.R")

###################### ITHIM application for Equity Analysis - Web Interface - Shiny App - Server/UI ######################
# app.R has 2 main components, UI and Server.
# User Interface (UI)
#     -About Page
#     -Simple Aggregated Plots Page
#     -Advanced Plots Page
#     -Custom Scenarios Page
# Server Function
#     -Simple Plot
#     -Advanced Plot
#     -Customizable Plot
# App Function

# User Interface ============================================================================================================
# Uses fluidPage and navbar for Layout
ui <- fluidPage(
  
  
  titlePanel("ITHIM-Sacramento Equity Analysis Tool"),  
  
  # Creates Title Tab
  navbarPage("ITHIM-Sac",
             
             # Creates About Page
             tabPanel("About and FAQ",
                      fluidRow(
                        column(6, 
                               includeHTML("ITHIM_About.html")# Pulls About page from Markdown File
                        )
                      )           
             ),
             # Creates Simple Aggregated Plot Page
             tabPanel("Simple Aggregated Plots",
                      sidebarLayout(
                        # Creates sidebar with Radio buttons
                        sidebarPanel(
                          # Parameter description
                          # barID: 1-future years,2-scenarios
                          # yaxisID: 1-Death total; 2-Death age.std; 3-DALYs total; 4-DALYs age.std
                          radioButtons("selectbarID", label = h3("Select Scenario"), 
                                       choices = list("2016 MTP/SCS Adopted Plan in Future Years" = 1, 
                                                      "Planning Scenarios in 2036" = 2), 
                                       selected = 1),
                          radioButtons("selectyaxisID", label = h3("Select Units"), 
                                       choices = list("Deaths - total" = 1, "Deaths - standardized by age and population" = 2, 
                                                      "Disability-Adjusted Life Years (DALYs) - total" = 3, 
                                                      "Disability-Adjusted Life Years (DALYs) - standardized by age and population" = 4
                                       ), 
                                       selected = 1)
                        ),
                        mainPanel(
                          plotOutput("SimplePlot")
                        )
                      )
             ),
             
             # Creates Advanced Plot Page          
             tabPanel("Advanced Plots",
                      sidebarLayout(
                        sidebarPanel(
                          # Parameter description
                          # countyID: 1-ELD; 2-PLA; 3-SAC; 4-SUT; 5-YOL; 6-YUB; 7-All
                          # barID: 1-future years,2-scenarios,3-customized
                          # outcomeID: 1-physical activity; 2-injury; 3-both
                          # demogrID: 1-Race/ethnicty; 2-household income
                          # yaxisID: 1-Death total; 2-Death age.std; 3-DALYs total; 4-DALYs age.std; 5-physical activity data
                          radioButtons("selectCounty", label = h3("Select County"), 
                                       choices = list("El Dorado" = 1, 
                                                      "Placer" = 2, 
                                                      "Sacramento" = 3, 
                                                      "Sutter"= 4, 
                                                      "Yolo"= 5, 
                                                      "Yuba"= 6,
                                                      "ALL"= 7), 
                                       selected = 1),
                          radioButtons("selectbarID_Adv", label = h3("Select Scenario"), 
                                       choices = list("2016 MTP/SCS Adopted Plan in Future Years" = 1, 
                                                      "Planning Scenarios in 2036" = 2), 
                                       selected = 1),
                          # checkboxGroupInput("selectoutcomeID",label = h3("Select Outcome"), choices = list("Physical Activity" = 1, "Injury" = 2), 
                          #                                  selected = 1),
                          radioButtons("selectoutcomeID", label = h3("Select Outcome"),
                                       choices = list("Physical Activity" = 1, "Injury" = 2,
                                                      "Both Physical Activity and Injury" = 3),
                                       selected = 1),
                          radioButtons("selectdemogrID", label = h3("Select Demographic"), 
                                       choices = list("Race/Ethnicity" = 1, 
                                                      "Household Income (for physcial activity outcome only)" = 2), 
                                       selected = 1),
                          radioButtons("selectyaxisID_Adv", label = h3("Select Units"), 
                                       choices = list("Deaths - total" = 1, "Deaths - standardized by age and population" = 2, 
                                                      "Disability Adjusted Life Years (DALYs) - total" = 3, 
                                                      "DALYs - standardized by age and population" = 4, 
                                                      "'Physical Activity' outcome only" = 5), 
                                       selected = 1)
                          # conditionalPanel(condition = "input.selectoutcomeID.length > 1 ",
                          #                  radioButtons("selectdemogrID", label = h3("Select Demographic"), 
                          #                               choices = list("Race/Ethnicity" = 1, "Household Income" = 2), 
                          #                               selected = 1), 
                          #                  radioButtons("selectyaxisID1", label = h3("Select Units"), 
                          #                               choices = list("Deaths - [Total]" = 1, "Death - [Age Standardized]" = 2, 
                          #                                              "Disability Adjusted Life Years (DALYs) - [Total]" = 3, "DALYs - [Age Standardized]" = 4, 
                          #                                              "Physical Activity Data" = 5), 
                          #                               selected = 1)
                          # ),
                          # conditionalPanel(condition = "input.selectoutcomeID == '1'",
                          #                  radioButtons("selectdemogrID", label = h3("Select Demographic"), 
                          #                               choices = list("Race/Ethnicity" = 1, "Household Income" = 2), 
                          #                               selected = 1), 
                          #                  radioButtons("selectyaxisID1", label = h3("Select Units"), 
                          #                               choices = list("Deaths - [Total]" = 1, "Death - [Age Standardized]" = 2, 
                          #                                              "Disability Adjusted Life Years (DALYs) - [Total]" = 3, "DALYs - [Age Standardized]" = 4, 
                          #                                              "Physical Activity Data" = 5), 
                          #                               selected = 1)
                          #                  ),
                          # conditionalPanel(condition = "input.selectoutcomeID == '2'",
                          #                  radioButtons("selectdemogrID", label = h3("Select Demographic"), 
                          #                               choices = list("Race/Ethnicity" = 1, "Household Income" = 2), 
                          #                               selected = 1), 
                          #                  radioButtons("selectyaxisID1", label = h3("Select Units"), 
                          #                               choices = list("Deaths - [Total]" = 1, "Death - [Age Standardized]" = 2, 
                          #                                              "Disability Adjusted Life Years (DALYs) - [Total]" = 3, "DALYs - [Age Standardized]" = 4, 
                          #                                              "Physical Activity Data" = 5), 
                          #                               selected = 1)
                          # ),
                          # conditionalPanel(condition = "input.selectoutcomeID.length => 1",
                          #                  radioButtons("selectdemogrID", label = h3("Select Demographic"), 
                          #                               choices = list("Race/Ethnicity" = 1, "Household Income" = 2), 
                          #                               selected = 1), 
                          #                  radioButtons("selectyaxisID1", label = h3("Select Units"), 
                          #                               choices = list("Deaths - [Total]" = 1, "Death - [Age Standardized]" = 2, 
                          #                                              "Disability Adjusted Life Years (DALYs) - [Total]" = 3, "DALYs - [Age Standardized]" = 4, 
                          #                                              "Physical Activity Data" = 5), 
                          #                               selected = 1)
                          # ),
                          
                          # radioButtons("selectdemogrID", label = h3("Select Demographic"), 
                          #              choices = list("Race/Ethnicity" = 1, "Household Income" = 2), 
                          #              selected = 1),
                          
                          # sliderInput(inputId = "mwt",
                          #             label = "Mean Walking Time (min per week)",
                          #             value = 47.49, min = 20, max = 100),
                        ),
                        mainPanel(
                          plotOutput("AdvancedPlot")
                        )
                      )
             )
             # Creates Custom Scenarios Page
             #Upload Panel from http://shiny.rstudio.com/gallery/upload-file.html
             # 01_Data/EQ/ActiveTransport/c1-c2-c3
             # 01_Data/EQ/PVD/c1-2-3
             
             # tabPanel("Custom Scenarios", 
             #          
             #          sidebarLayout(
             #            sidebarPanel(
             #              fileInput('file1', 'Choose file to upload',
             #                        multiple = TRUE,
             #                        accept = c(
             #                          'text/csv',
             #                          'text/comma-separated-values',
             #                          'text/tab-separated-values',
             #                          'text/plain',
             #                          '.csv',
             #                          '.tsv'
             #                        )
             #              ),
             #              
             #              tags$hr(),
             #              p('Upload a Person Vehicle Distance File',
             #                a(href = "07_PersonVehicleDistance_Costom1.csv", "PersonVehicleDistance_Custom1")
             #              ),
             #              fileInput('file2', 'Choose file to upload',
             #                        multiple = TRUE,
             #                        accept = c(
             #                          'text/csv',
             #                          'text/comma-separated-values',
             #                          'text/tab-separated-values',
             #                          'text/plain',
             #                          '.csv',
             #                          '.tsv'
             #                        )
             #              ),
             #              
             #              tags$hr(),
             #              p('Upload an Active Transport By Race File',
             #                a(href = "01_ActiveTransport_byRace.C1.csv", "Active Transport By Race")
             #              ),
             #              fileInput('file3', 'Choose file to upload',
             #                        multiple = TRUE,
             #                        accept = c(
             #                          'text/csv',
             #                          'text/comma-separated-values',
             #                          'text/tab-separated-values',
             #                          'text/plain',
             #                          '.csv',
             #                          '.tsv'
             #                        )
             #              ),
             #              
             #              tags$hr(),
             #              p('Upload an Active Transport By Income File',
             #                a(href = "02_ActiveTransport_byIncome.C1.csv", "Active Transport By Income")
             #              ),
             #              fileInput('file4', 'Choose file to upload',
             #                        multiple = TRUE,
             #                        accept = c(
             #                          'text/csv',
             #                          'text/comma-separated-values',
             #                          'text/tab-separated-values',
             #                          'text/plain',
             #                          '.csv',
             #                          '.tsv'
             #                        )
             #              ),
             #              
             #              tags$hr(),
             #              p('Upload a Mean Active Transport Time by Race File',
             #                a(href = "03_PopulationMeanATTimebyRace.C1.csv", "Mean Active Transport Time by Race")
             #              ),
             #              fileInput('file5', 'Choose file to upload',
             #                        multiple = TRUE,
             #                        accept = c(
             #                          'text/csv',
             #                          'text/comma-separated-values',
             #                          'text/tab-separated-values',
             #                          'text/plain',
             #                          '.csv',
             #                          '.tsv'
             #                        )
             #              ),
             #              
             #              tags$hr(),
             #              p('Upload a Mean Active Transport Time by Race File',
             #                a(href = "04_PopulationMeanATTimebyIncome.C1.csv", "Mean Active Transport Time by Income")
             #              ),
             #              radioButtons("selectCounty_cust", label = h3("Select County"), 
             #                           choices = list("El Dorado" = 1, "Placer" = 2, "Sacramento" = 3, "Sutter"= 4, "Yolo"= 5, "Yuba"= 6, "All"= 7), 
             #                           selected = 1),
             #              radioButtons("selectbarID_cust", label = h3("Select Scenario"), 
             #                           choices = list("Future Years" = 1, "Scenarios" = 2, "Customized" = 3), 
             #                           selected = 1),
             #              radioButtons("selectoutcomeID_cust", label = h3("Select Outcome"), 
             #                           choices = list("Physical Activity" = 1, "Injury" = 2, "Both" = 3), 
             #                           selected = 1),
             #              radioButtons("selectdemogrID_cust", label = h3("Select Demographic"), 
             #                           choices = list("Race/Ethnicity" = 1, "Household Income" = 2), 
             #                           selected = 1),
             #              radioButtons("selectyaxisID_cust", label = h3("Select Units"), 
             #                           choices = list("Deaths - [Total]" = 1, "Death - [Age Standardized]" = 2, 
             #                                          "Disability Adjusted Life Years (DALYs) - [Total]" = 3, "DALYs - [Age Standardized]" = 4, 
             #                                          "Physical Activity Data" = 5), 
             #                           selected = 1)
             #            ),
             #            mainPanel(
             #              plotOutput("CustomizablePlot")
             #            )
             #            
             #          )
             # )
  )
)

# Server Function ===========================================================================================================
server <- function(input, output) {
  
  # data <- reactive({
  #       (input$select)
  #     })
  
  # Plots Simple Aggregated Graph using inputs from Simple Aggregated Plot radio buttons  
  output$SimplePlot <- renderPlot({
    # Parameter description
    # barID: 1-future years,2-scenarios
    # yaxisID: 1-Death total; 2-Death age.std; 3-DALYs total; 4-DALYs age.std
    aggr.outcome.shiny.app(barID = as.integer(input$selectbarID),yaxisID = as.integer(input$selectyaxisID))
  },height = 600)
  
  # Plots Advanced Graphs using inputs from Advanced Plot radio buttons
  output$AdvancedPlot <- renderPlot({
    
    # Parameter description
    # countyID: 1-ELD; 2-PLA; 3-SAC; 4-SUT; 5-YOL; 6-YUB; 7-All
    # barID: 1-future years,2-scenarios,3-customized
    # outcomeID: 1-physical activity; 2-injury; 3-both
    # demogrID: 1-Race/ethnicty; 2-household income
    # yaxisID: 1-Death total; 2-Death age.std; 3-DALYs total; 4-DALYs age.std; 5-physical activity data
    integrated.shiny.app(countyID = as.integer(input$selectCounty), barID = as.integer(input$selectbarID_Adv),
                         outcomeID = as.integer(input$selectoutcomeID),demogrID = as.integer(input$selectdemogrID), 
                         yaxisID = as.integer(input$selectyaxisID_Adv)
    )
  },height = 600)
  
  # Creates Customizable Plot from Input File
  output$CustomizablePlot <- renderPlot({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    
    # Re - input the vehicle distance data with Custom Scenarios
    PersonVehicleDist.C1 <- read.csv(inFile$datapath)
    AT.file.C1.byRace <- read.csv(inFile$datapath)
    AT.file.C1.byIncome <- read.csv(inFile$datapath)
    AT_Pop_MeanTimebyRace.C1 <- read.csv(inFile$datapath)
    AT_Pop_MeanTimebyIncome.C1 <- read.csv(inFile$datapath)
    
    
    # Parameter description
    # countyID: 1-ELD; 2-PLA; 3-SAC; 4-SUT; 5-YOL; 6-YUB; 7-All
    # barID: 1-select outcome,2-scenarios,3-customized
    # outcomeID: 1-physical activity; 2-injury; 3-both
    # demogrID: 1-Race/ethnicty; 2-household income
    # yaxisID: 1-Death total; 2-Death age.std; 3-DALYs total; 4-DALYs age.std; 5-physical activity data
    integrated.shiny.app(countyID = as.integer(input$selectCounty_cust), barID = as.integer(input$selectbarID_cust),
                         outcomeID = as.integer(input$selectoutcomeID_cust),demogrID = as.integer(input$selectdemogrID_cust), 
                         yaxisID = as.integer(input$selectyaxisID_cust))
  })
  
  
}

# App Function ===========================================================================================================
shinyApp(ui = ui, server = server)