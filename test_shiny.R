library(shiny)

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----    
  titlePanel("Fit Band Data Analysis"),
  
  
  # Main panel for displaying outputs ----
  mainPanel(
    
    
    tabsetPanel(type = "tabs",
                tabPanel("DataSet",selectInput(inputId = "dataset",
                                               label = "Select DataSet",
                                               choices = c("TestBandData1","Raw_data")),numericInput(inputId = "obs",
                                                                                                 label = "Number of observations to view:",
                                                                                                 value = 10), tableOutput("view")),
                tabPanel("Univariate",
                         radioButtons('dist', 
                                      h5('Select the options from the given'), 
                                      c("Steps" = "steps",
                                        "Calories Burned" = "cal",
                                        "Light Sleep" = "light_slp", 
                                        "Deep Sleep" = "deep_slp",
                                        "Total sleep" = "tot_slp"
                                      ), inline = TRUE),plotOutput("distPlot"),h4('Five number summary'),verbatimTextOutput("summary")),
                tabPanel("Hyp_testing",
                         h4('male vs day'),verbatimTextOutput("ttest1"),h4('female vs day'),verbatimTextOutput("ttest2")),
                
                
                
                tabPanel("Bivariate",
                         radioButtons('dist1', 
                                      h5('Select the options from the given'), 
                                      c("Plot: Steps-Total sleep" = "steps_tot_slp_plt",
                                        "Plot: CaloriesBurned-Totalsleep" = "calorie_Total_sleep_plt",
                                        "Graph: Steps-Total sleep" = "steps_tot_slp_grp",
                                        "Graph: CaloriesBurned-Totalsleep" = "calorie_Total_sleep_grp"
                                      ), inline = FALSE),plotOutput("distPlot1"),h4('Correlation analysis: Steps-Total sleep'),verbatimTextOutput("res"),h4('Correlation analysis: Steps-Total sleep'),verbatimTextOutput("res1"))
                ,
                tabPanel("Classifier",h4('Confusion matrix'),verbatimTextOutput("cm"),h4('Accuracy'),verbatimTextOutput("accuracy")),
                tabPanel("Conclusion",h4('
                                         There are 157 observations and 11 variables. Dataset is divided into Male and female Dataset,
                                         where 49.27% of Male had Good sleep on weekdays and 41.66% of male had Good sleep on weekend.
                                         87% of Female had Good sleep on weekdays and 80% of Female had good sleep on weekend.'
                ),h4('As there is no significance difference between means for Male and Female with respect to Weekdays and Weekends sleep.
                                            We considered the whole dataset and 66.87% of students had good sleep. 62.85% female and 37.14% male had good sleep. 
                     '))
                )
    
    ) 
    )


# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Return the requested dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "TestBandData1" =BandData,
           "Raw_data"=Project_data)
  })
  
  # Show the first "n" observations ----
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
    
  })
  
  
  output$distPlot <- renderPlot({
    dist <- switch(input$dist,
                   steps =hist(TestBandData1$Steps,
                               main = toupper("Steps"),
                               ylab = "frequency",
                               col = "blue"),
                   cal = hist(TestBandData1$Calories_Burned,
                              main = toupper("Calories"),
                              ylab = "frequency",
                              col = "blue"),
                   light_slp = hist(TestBandData1$Light_sleep,
                                    main = toupper("Light sleep"),
                                    ylab = "",
                                    col = "blue"),
                   deep_slp =hist(TestBandData1$Deep_sleep,
                                  main = toupper("Deep sleep"),
                                  ylab = "",
                                  col = "blue"),
                   
                   tot_slp =hist(TestBandData1$Total_slp,
                                 main = toupper("Total sleep"),
                                 ylab = "",
                                 col = "blue")
                   
    )
  })
  output$summary <- renderPrint({
    summary(datasummary1)
  })
  
  output$distPlot1 <- renderPlot({
    dist1 <- switch(input$dist1,
                    steps_tot_slp_plt =plot(TestBandData1$Steps,TestBandData1$Total_slp,
                                            main = toupper("Steps-Total sleep"),
                                            xlab = "Steps",
                                            ylab = "Total sleep"),
                    calorie_Total_sleep_plt = plot(TestBandData1$Calories_Burned,TestBandData1$Total_slp,
                                                   main = toupper("Steps-Total sleep"),
                                                   xlab = "Calories burned",
                                                   ylab = "Total sleep"),
                    steps_tot_slp_grp =barplot(TestBandData1$Steps,TestBandData1$Total_slp,
                                               main = toupper("Steps-Total sleep"),
                                               xlab = "Total sleep",
                                               ylab = "Steps"),
                    calorie_Total_sleep_grp = barplot(TestBandData1$Calories_Burned,TestBandData1$Total_slp,
                                                      main = toupper("Calories  burned-Total sleep"),
                                                      xlab = "Total sleep",
                                                      ylab = "Calories burned")
                    
                    
                    
    )
  })
  
  
  
  output$ttest1<-renderPrint({t.test(male$Total_slp~male$Weekend)})
  
  output$ttest2<-renderPrint ({t.test(female$Total_slp~female$Weekend)})
  
  output$res <-renderText ({cor(TestBandData1$Steps,TestBandData1$Total_slp)})
  output$res1 <-renderText ({cor(TestBandData1$Calories_Burned,TestBandData1$Total_slp)})
  
  output$cm <-renderPrint ({table(test$class,pred)})
  output$accuracy <-renderPrint ({accuracy*100})
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
