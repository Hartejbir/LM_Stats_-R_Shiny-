
# import libraries
library(dplyr)
library(DT)
library(flexdashboard)
library(ggplot2)
library(ggthemes)
library(googleVis)
library(plotly)
library(readr)
library(shiny)
library(shinyjqui)
library(shinythemes)
library(shinyWidgets)



dataset = read.csv('50_Startups.csv')

dataset$State <- factor(dataset$State,
                        levels = c('New York', 'California', 'Florida'),
                        labels = c(0, 1, 2))


library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = Profit ~ .,
               data = training_set)


# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)


pred_reg = lm(formula = Profit ~ ., data = test_set)


# Plots Playground - Test However You Would Like Your Plots to Look Like with ggplot2 and plotly R packages
qplot(dataset$State, dataset$Profit)
ggplot(dataset, aes(State, Profit, colour = State)) + 
  labs(x = "Research and Development Spend ($)", y = "Profit ($)") +
  geom_point(size = 3) + 
  geom_smooth(method = "lm") +
  coord_cartesian() +
  theme_minimal()

ggplotly(ggplot(dataset, aes(x = R.D.Spend, y = Profit, colour = State, shape = State)) + 
  ggtitle("Linear Model of Profit Affected by Research & Development Spending") +
  labs(x = "Research and Development Spent ($)", y = "Profit ($)") + 
  geom_point(size = 3) + 
  geom_smooth(method = "lm") +
  coord_cartesian() +
  theme_tufte() 
)

ggplotly(ggplot(dataset, aes(Profit, colour = State, fill = State)) + geom_density(alpha = 0.5) + theme_light())



ui <- fluidPage(theme = shinytheme("cosmo"),
                
                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
                ),
                
                #includeScript('www/script.js'),
                
                titlePanel("Regression Analysis - Profit by U.S. State"),
                div(class = "Tab", sortableTabsetPanel(
                  tabPanel("Data Visualization",
                           div(class = "Data .animated .bounce",
                           sidebarLayout(
                             div(
                               id = "sbl",
                             div(id = "sidebar", class = "shadow p-3 mb-5 bg-white rounded", sidebarPanel(
                               helpText('Select Your Section or All: '),
                               selectInput(inputId = 'section', label = strong("Section"), 
                                           choices <- c('All', 'R.D.Spend', 'Administration', 'Marketing.Spend'),
                                           uiOutput('Section')
                               ),
                               helpText('Select Your State or All: '),
                               selectInput(inputId = 'state', label = strong("State"),
                                           choice <- c('All', 'New York', 'California', 'Florida'),
                                           uiOutput('State')
                               ),
                               
                               
                               div( id = 'box', 
                                   helpText(id = "text", "Legend"),
                                   h5("Section:"),
                                   p("Administration - Budget Spent on Administration"),
                                   p("Marketing.Spend - Budget Spend on Marketing"),
                                   p("R.D.Spend - Budget Spend on Research and Development"),
                                   h5("State:"),
                                   p("0 - New York"),
                                   p("1 - California"),
                                   p("2 - Florida")
                                  ),
                               textOutput(outputId = "Status_ID")
                                )
                              )
                             ),
                           mainPanel(
                             jqui_resizable(div(id = 'Reg', plotlyOutput(outputId = 'RegPlot'))), 
                             
                             jqui_resizable(div(id = 'Dist',plotlyOutput(outputId = 'DistPlot')))
                          )
                      )
                    )     
                  ),
                  
                  tabPanel("Data Table", 
                           div(id = 'dataTable', class = 'Data', 
                            helpText('Datasheet Source: '),
                            jqui_resizable(DT::dataTableOutput("RegData")))),
                  tabPanel("Model Summary",
                           jqui_resizable(div(id = 'summary', class = 'Data', style = "background: white; border-width: 2px; border-color: #A9A9A9 ",
                           helpText('Regression Analysis'),
                           (verbatimTextOutput(outputId = 'Res_ID')),
                           div(style = "background: white; border-width: 2px; border-color: #A9A9A9 ",
                               textOutput(outputId = 'Results'))))
                  )
                )
          )
)


# Shiny Server
server <- function(input, output) {
  
  #Render Data Table
  output$RegData <- DT::renderDataTable({dataset}, 
                                        style = 'bootstrap', 
                                        class = 'table-bordered table-striped table-hover', 
                                        rownames = FALSE, options = list(lengthMenu = c(5, 10, 15, 20, 25, 50), 
                                                                         pageLength = 10))
  
  # Render Plot
  output$RegPlot <- renderPlotly({
    #qplot(dataset$R.D.Spend, dataset$Profit, 
     #     xlab = 'State', ylab ='Profit', col = dataset$State) + geom_point(size = 3)
    if (input$state == 'All') {
      dataview = dataset
      if (input$section == 'All') {
        ggplotly(
          ggplot(dataview, aes(State, Profit, colour = State, shape = State)) + 
            ggtitle('Linear Model of Profit by State') + 
            labs(x = "State", y = "Profit ($)") +
            geom_point(size = 2, alpha = 0.7) + 
            geom_smooth(method = "lm") +
            coord_cartesian() +
            theme_minimal(), tooltip = c("x", "y")
        )
      } else if (input$section == 'R.D.Spend') {
        ggplotly(
          ggplot(dataview, aes(x = R.D.Spend, y = Profit, colour = State, shape = State)) + 
            ggtitle("Linear Model of Profit Affected by Research & Development Spending") +
            labs(x = "Research and Development Spent ($)", y = "Profit ($)") + 
            geom_point(size = 2, alpha = 0.7) + 
            geom_smooth(method = "lm") +
            coord_cartesian() +
            theme_minimal(), tooltip = c("x", "y", "colour")
        )
      } else if (input$section == 'Administration') {
        ggplotly(
          ggplot(dataview, aes(x = Administration, y = Profit, colour = State, shape = State)) + 
            ggtitle("Linear Model of Profit Affected by Research & Development Spending") +
            labs(x = "Administration ($)", y = "Profit ($)") + 
            geom_point(size = 2, alpha = 0.7) + 
            geom_smooth(method = "lm") +
            coord_cartesian() +
            theme_minimal(), tooltip = c("x", "y", "colour")
        )
      } else if (input$section == 'Marketing.Spend') {
        ggplotly(
          ggplot(dataview, aes(x = Marketing.Spend, y = Profit, colour = State, shape = State)) + 
            ggtitle("Linear Model of Profit Affected by Marketing Spending") +
            labs(x = "Marketing Spent ($)", y = "Profit ($)") + 
            geom_point(size = 2, alpha = 0.7) + 
            geom_smooth(method = "lm") +
            coord_cartesian() +
            theme_minimal(), tooltip = c("x", "y", "colour")
        )
      }
    } else if (input$state == "New York") {
      dataview = filter(dataset, dataset$State == 0)
      if (input$section == 'All') {
        ggplotly(
          ggplot(dataview, aes(State, Profit, colour = State)) + 
            ggtitle('Linear Model of Profit by State') + 
            labs(x = "State", y = "Profit ($)") +
            geom_point(size = 2, alpha = 0.7) + 
            geom_smooth(method = "lm") +
            coord_cartesian() +
            theme_minimal(), tooltip = c("x", "y")
        )
      } else if (input$section == 'R.D.Spend') {
        ggplotly(
          ggplot(dataview, aes(x = R.D.Spend, y = Profit, colour = State)) + 
            ggtitle("Linear Model of Profit Affected by Research & Development Spending") +
            labs(x = "Research and Development Spent ($)", y = "Profit ($)") + 
            geom_point(size = 2, alpha = 0.7) + 
            geom_smooth(method = "lm") +
            coord_cartesian() +
            theme_minimal(), tooltip = c("x", "y")
        )
      } else if (input$section == 'Administration') {
        ggplotly(
          ggplot(dataview, aes(x = Administration, y = Profit, colour = State, xlab = R.D.Spend, ylab = Profit)) + 
            ggtitle("Linear Model of Profit Affected by Research & Development Spending") +
            labs(x = "Administration ($)", y = "Profit ($)") + 
            geom_point(size = 2, alpha = 0.7) + 
            geom_smooth(method = "lm") +
            coord_cartesian() +
            theme_minimal(), tooltip = c("x", "y")
        )
      } else if (input$section == 'Marketing.Spend') {
        ggplotly(
          ggplot(dataview, aes(x = Marketing.Spend, y = Profit, colour = State)) + 
            ggtitle("Linear Model of Profit Affected by Marketing Spending") +
            labs(x = "Marketing Spent ($)", y = "Profit ($)") + 
            geom_point(size = 2, alpha = 0.7) + 
            geom_smooth(method = "lm") +
            coord_cartesian() +
            theme_minimal(), tooltip = c("x", "y")
        )
      } 
    } else if (input$state == "California") {
        dataview = filter(dataset, dataset$State == 1)
        if (input$section == 'All') {
          ggplotly(
            ggplot(dataview, aes(State, Profit, colour = State)) + 
              ggtitle('Linear Model of Profit by State') + 
              labs(x = "State", y = "Profit ($)") +
              geom_point(size = 2, alpha = 0.7) + 
              geom_smooth(method = "lm") +
              coord_cartesian() +
              theme_minimal(), tooltip = c("x", "y")
          )
        } else if (input$section == 'R.D.Spend') {
          ggplotly(
            ggplot(dataview, aes(x = R.D.Spend, y = Profit, colour = State)) + 
              ggtitle("Linear Model of Profit Affected by Research & Development Spending") +
              labs(x = "Research and Development Spent ($)", y = "Profit ($)") + 
              geom_point(size = 2, alpha = 0.7) + 
              geom_smooth(method = "lm") +
              coord_cartesian() +
              theme_minimal(), tooltip = c("x", "y")
          )
        } else if (input$section == 'Administration') {
          ggplotly(
            ggplot(dataview, aes(x = Administration, y = Profit, colour = State, xlab = R.D.Spend, ylab = Profit)) + 
              ggtitle("Linear Model of Profit Affected by Research & Development Spending") +
              labs(x = "Administration ($)", y = "Profit ($)") + 
              geom_point(size = 2, alpha = 0.7) + 
              geom_smooth(method = "lm") +
              coord_cartesian() +
              theme_minimal(), tooltip = c("x", "y")
          )
        } else if (input$section == 'Marketing.Spend') {
          ggplotly(
            ggplot(dataview, aes(x = Marketing.Spend, y = Profit, colour = State)) + 
              ggtitle("Linear Model of Profit Affected by Marketing Spending") +
              labs(x = "Marketing Spent ($)", y = "Profit ($)") + 
              geom_point(size = 2, alpha = 0.7) + 
              geom_smooth(method = "lm") +
              coord_cartesian() +
              theme_minimal(), tooltip = c("x", "y")
          )
        } 
      } else if (input$state == "Florida") {
          dataview = filter(dataset, dataset$State == 2)
          if (input$section == 'All') {
            ggplotly(
              ggplot(dataview, aes(State, Profit, colour = State)) + 
                ggtitle('Linear Model of Profit by State') + 
                labs(x = "State", y = "Profit ($)") +
                geom_point(size = 2, alpha = 0.7) + 
                geom_smooth(method = "lm") +
                coord_cartesian() +
                theme_minimal(), tooltip = c("x", "y")
            )
          } else if (input$section == 'R.D.Spend') {
            ggplotly(
              ggplot(dataview, aes(x = R.D.Spend, y = Profit, colour = State)) + 
                ggtitle("Linear Model of Profit Affected by Research & Development Spending") +
                labs(x = "Research and Development Spent ($)", y = "Profit ($)") + 
                geom_point(size = 2, alpha = 0.7) + 
                geom_smooth(method = "lm") +
                coord_cartesian() +
                theme_minimal(), tooltip = c("x", "y")
            )
          } else if (input$section == 'Administration') {
            ggplotly(
              ggplot(dataview, aes(x = Administration, y = Profit, colour = State, xlab = R.D.Spend, ylab = Profit)) + 
                ggtitle("Linear Model of Profit Affected by Research & Development Spending") +
                labs(x = "Administration ($)", y = "Profit ($)") + 
                geom_point(size = 2, alpha = 0.7) + 
                geom_smooth(method = "lm") +
                coord_cartesian() +
                theme_minimal(), tooltip = c("x", "y")
            )
          } else if (input$section == 'Marketing.Spend') {
            ggplotly(
              ggplot(dataview, aes(x = Marketing.Spend, y = Profit, colour = State)) + 
                ggtitle("Linear Model of Profit Affected by Marketing Spending") +
                labs(x = "Marketing Spent ($)", y = "Profit ($)") + 
                geom_point(size = 2, alpha = 0.7) + 
                geom_smooth(method = "lm") +
                coord_cartesian() +
                theme_minimal(), tooltip = c("x", "y")
            )
          }
      }
  })
  
  output$DistPlot <- renderPlotly({
    if (input$state == 'All') {
      dataview = filter(dataset)
      ggplotly(
        ggplot(dataset, aes(Profit, colour = State, fill = State)) + ggtitle("Distribution of Profit ($)") + 
        xlab("Profit") + ylab("Density") +
        geom_density(alpha = 0.5) +
        coord_cartesian() +
        theme_minimal(), tooltip = c("x", "y", "colour")
      )
    } else if (input$state == 'New York') {
      dataview = filter(dataset, dataset$State == 0)
      ggplotly(
        ggplot(dataview, aes(Profit, colour = State, fill = State)) + ggtitle("Distribution of Profit ($)") + 
        xlab("Profit") + ylab("Density") +
        geom_density(alpha = 0.5) +
        coord_cartesian() +
        theme_minimal(), tooltip = c("x", "y")
      )
    } else if (input$state == 'California') {
      dataview = filter(dataset, dataset$State == 1)
      ggplotly(
        ggplot(dataview, aes(Profit, colour = State, fill = State)) + ggtitle("Distribution of Profit ($)") + 
        xlab("Profit") + ylab("Density") +
        geom_density(alpha = 0.5) +
        coord_cartesian() +
        theme_minimal(), tooltip = c("x", "y")
      )
    } else if (input$state == 'Florida') {
      dataview = filter(dataset, dataset$State == 2)
      ggplotly(
        ggplot(dataview, aes(Profit, colour = State, fill = State)) + ggtitle("Distribution of Profit ($)") + 
        xlab("Profit") + ylab("Density") +
        geom_density(alpha = 0.5) +
        coord_cartesian() +
        theme_minimal(), tooltip = c("x", "y")
      )
    }
  })
  
  
  output$Status_ID <- renderText({
    if (input$state == "All" & input$section == "All") {
      paste("You have selected", input$section, "sections for", input$state, "states.")
    } else if (input$state == 'All' & input$section != 'All') {
      paste("You have selected", input$section, "for", input$state, "states")
    } else if (input$state != 'All' & input$section == 'All') {
      paste("You have selected", input$section, "sections for", input$state)
    } else {
      paste("You have selected", input$section, "for", input$state)
    }
    
  })
  
  
  output$Res_ID <- renderPrint({
    summary(regressor)
  })
  
  output$Results <- renderPrint({
    paste("Significant Variables: Budget Spending in Reserach & Development (R.D.Spend)")
  })
  
  output$Section <- renderUI(
    jqui_toggle("#Section", "fade")
  )
  
  output$State <- renderUI(
    jqui_effect(input$state, "slideToggle", duration = 200)
  )
  
}


# Create ShinyApp Function
shinyApp(ui = ui, server = server)