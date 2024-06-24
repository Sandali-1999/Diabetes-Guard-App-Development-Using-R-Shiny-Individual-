#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("glmnet")

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr) # Required for gather function
library(glmnet) # For logistic regression model

# Load the diabetes data set
data <- read.csv(file.path(getwd(), "diabetes.csv"))

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Diabetes Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName= "home", icon = icon("house")),
      menuItem("Objective", tabName = "objective", icon = icon("bullseye")),
      menuItem("Description", tabName = "description", icon=icon("eye")),
      menuItem("Introduction", tabName = "introduction", icon=icon("readme")),
      menuItem("Symptoms", tabName = "symptoms", icon=icon("heart-circle-bolt")),
      menuItem("Data Exploration Part1", tabName = "dataexplorationpart1", icon = icon("magnifying-glass-chart")),
      menuItem("Data Exploration Part2", tabName = "dataexplorationpart2", icon = icon("magnifying-glass-chart")),
      menuItem("Diabetes Prediction", tabName = "diabetesprediction", icon = icon("calculator")),
      menuItem("To Do List", tabName = "todolist", icon = icon("check-square"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "home",
              fluidRow(
                titlePanel(HTML("<b><h1 style='font-family:Arial Rounded MT Bold; font-size: 70px; color: #00008B; text-align: center;'>Diabetes Guard</h1></b>")),
                tags$video(
                  src = "https://videos.pexels.com/video-files/7579983/7579983-hd_1920_1080_25fps.mp4",
                  type = "video/mp4",
                  width = "100%",
                  height = 500,
                  controls = TRUE
                )
              )
      ),
      # Second tab content
      tabItem(tabName = "objective",
              tabPanel(title = "Diabetes",
                       h3(HTML("<b>OBJECTIVE</b>")),
                       tags$div(
                         style = "font-family: 'Comic Sans MS'; color: #000000; font-size: 15px;",
                         "Diabetes is a very serious illness. It not only affects the body but can also lead to other dangerous conditions like heart attacks and blindness. ",
                         "Normally, people need to go to a doctor and wait for some time to get their test results. ",
                         "This dataset is from a research organization called the National Institute of Diabetes and Digestive and Kidney Diseases. ",
                         "The purpose of this Diabetes Guard App is to help doctors and patients to predict if a person has diabetes or not based on certain tests. ",
                         "All the people in this dataset are women who are at least 21 years old. ",
                         "The dataset includes different health measurements like the number of times a person has been pregnant, their body mass index (BMI), insulin levels, age, and more."
                       ),
                       imageOutput("diabetes_img")
              )
      ),
      # Third tab content
      tabItem(tabName = "description",
              tabPanel(title = "Description",
                       h3(HTML("<b>DESCRIPTION</b>")),
                       tags$div(
                         style = "font-family: 'Comic Sans MS'; color: #000000; font-size: 15px;",
                         tags$ul(
                           tags$li("Pregnancies: No. of times pregnant"),
                           tags$li("Glucose: Plasma Glucose Concentration (mg/dl)"),
                           tags$li("Blood Pressure: Diastolic Blood Pressure(mmHg)"),
                           tags$li("Skin Thickness: A value used to estimate body fat. Normal Triceps SkinFold Thickness in women is 23mm. Higher thickness leads to obesity and chances of diabetes increases."),
                           tags$li("Insulin: 2-Hour Serum Insulin (mu U/ml)"),
                           tags$li("BMI: Body Mass Index (weight in kg/ height in m2)"),
                           tags$li("Diabetes Pedigree Function: It provides information about diabetes history in relatives and genetic relationship of those relatives with patients. Higher Pedigree Function means patient is more likely to have diabetes."),
                           tags$li("Age: Age (years)"),
                           tags$li("Outcome: Class Variable (0 or 1) where ‘0’ denotes patient is not having diabetes and ‘1’ denotes patient having diabetes.")
                         )
                       ),
                       imageOutput("description_img")
              )
      ),
      #Fourth tab content
      tabItem(tabName = "introduction", tabPanel(title = "Introduction",
                                                 h3(HTML("<b>INTRODUCTION</b>")),
                                                 tags$div(
                                                   style = "font-family: 'Comic Sans MS'; color: #000000; font-size: 15px;",
                                                   "Diabetes, a chronic metabolic disorder, poses a significant health challenge worldwide.
                         It is characterized by elevated blood sugar levels resulting from either insufficient insulin production or the body's ineffective use of insulin. 
                         This condition, if left unmanaged, can lead to severe complications such as heart disease, kidney failure, vision impairment, and nerve damage. 
                         Diabetes encompasses several types, with the most common being type 1 and type 2. 
                         Type 1 diabetes typically develops in childhood or adolescence and requires lifelong insulin therapy, whereas type 2 diabetes, which accounts for the majority of cases, often develops in adulthood and is closely linked to lifestyle factors such as obesity and physical inactivity. 
                         Understanding the causes, symptoms, and management strategies for diabetes is crucial in preventing its onset and reducing its associated health risks."
                                                 ),
                                                 imageOutput("introduction_img")
      )
      ),
      #Fifth tab content
      tabItem(tabName = "symptoms", tabPanel(title = "DIABETES SYMPTOMS",
                                             h3(HTML("<b>DIABETES SYMPTOMS</b>")),
                                             imageOutput("symptoms_img")
      )),
      #Sixth tab content
      tabItem(tabName = "dataexplorationpart1", 
              fluidRow(
                tabPanel(title = "Diabetes Data Exploration", 
                         h3(HTML("<b>DIABETES DATA EXPLORATION</b>"))),
                tags$div(
                  style = "font-family: 'Comic Sans MS'; color: #000000; font-size: 15px;", "Scatter Plots are Describe:",
                  tags$ul(
                    tags$li("When the glucose level increases, the insulin level also increases."),
                    tags$li("When the skin thickness increases, the BMI also increases.")
                    
                  )
                ),
                sidebarPanel(
                  selectInput("x", "X-axis Variable:", choices = colnames(data)[-1]),
                  selectInput("y", "Y-axis Variable:", choices = colnames(data)[-c(1, 9)]),
                  sliderInput("age_range", "Select Age Range:", min = 20, max = 80, value = c(20, 80), step = 5)
                ),
                mainPanel(
                  plotOutput("scatterplot")
                )
              )
      ),
      #Seventh tab content
      tabItem(tabName = "dataexplorationpart2",
              fluidRow(
                tabPanel(title = "Diabetes Data Analysis",
                         h3(HTML("<b>DIABETES DATA EXPLORATION</b>")),tags$div(
                           style = "font-family: 'Comic Sans MS'; color: #000000; font-size: 15px;", "Box-Plots and Violine Plots Describe:",
                           tags$ul(
                             tags$li("Variables such as glucose levels, age, blood pressure, BMI, skin thickness, and the diabetes pedigree function tend to be higher in diabetic patients compared to non-diabetic individuals.")
                             ),
                           "Correlation Map Shows that:",  
                           tags$ul(
                             tags$li("Glucose level have higher correlation in the outcome compare to the other variables")
                           )
                         )
                         ),
                sidebarPanel(
                  helpText("Explore Diabetes Data"),
                  selectInput("plot_type", "Select Plot Type:",
                              choices = c("Heatmap",
                                          "Diabetes Pedigree Function Histogram",
                                          "Boxplots for Outcomes and Different Variables",
                                          "Boxplots - BMI, Pregnancies vs Outcome",
                                          "Violin Plot - Diabetes Pedigree Function vs Outcome",
                                          "Violin Plot - Insulin vs Outcome",
                                          "Descriptive Statistics"))
                ),
                mainPanel(
                  plotOutput("plot"),
                  verbatimTextOutput("summary")
                )
              )
      ),
      #Eighth tab content
      tabItem(tabName = "diabetesprediction",
              fluidRow(
                tabPanel(title = "Diabetes Prediction",
                         h3(HTML("<b>DIABETES PREDICTION</b>"))),
                column(6,
                       numericInput("Pregnancies", "Pregnancies:", value = 1, min = 0, max = 17),
                       numericInput("Glucose", "Glucose:", value = 100, min = 0, max = 200),
                       numericInput("BloodPressure", "Blood Pressure:", value = 70, min = 0, max = 122),
                       numericInput("SkinThickness", "Skin Thickness:", value = 20, min = 0, max = 99),
                       numericInput("Insulin", "Insulin:", value = 79, min = 0, max = 846),
                       numericInput("BMI", "BMI:", value = 30, min = 0, max = 67),
                       numericInput("DiabetesPedigreeFunction", "Diabetes Pedigree Function:", value = 0.5, min = 0, max = 2),
                       numericInput("Age", "Age:", value = 40, min = 20, max = 81),
                       actionButton("predict_button", "Predict"),
                       textOutput("prediction_text")
                ),
                column(6,
                       imageOutput("prediction_img")
                )
              )
      ),
      #Ninth tab content
      tabItem(tabName = "todolist",
              fluidRow(
                tabPanel(title = "To Do List",
                         h3(HTML("<b>TO DO LIST</b>"))),
                tags$div(
                  style = "font-family: 'Comic Sans MS'; color: #000000; font-size: 15px;", "If you have diabetes do following things:",
                  tags$ul(
                    tags$li("Make and eat healthy food."),
                    tags$li("Be active most days."),
                    tags$li("Test your blood sugar often."),
                    tags$li("Take medicines as prescribed, even if you feel good."),
                    tags$li("Learn ways to manage stress."),
                    tags$li("Cope with the emotional side of diabetes."),
                    tags$li("Go to checkups.")
                  )
                ),
                tags$video(
                  src = "https://videos.pexels.com/video-files/7423640/7423640-uhd_3840_2160_30fps.mp4",
                  type = "video/mp4",
                  width = "100%",
                  height = 500,
                  controls = TRUE
                )
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Filter data based on age range
  filtered_data <- reactive({
    data[data$Age >= input$age_range[1] & data$Age <= input$age_range[2], ]
  })
  
  # Create scatterplot
  output$scatterplot <- renderPlot({
    ggplot(filtered_data(), aes_string(x = input$x, y = input$y)) +
      geom_point() +
      labs(title = paste("Scatterplot of", input$x, "vs.", input$y),
           x = input$x,
           y = input$y)
  })
  
  # Define Pictures
  # add output in server
  output$diabetes_img <- renderImage({
    list(src = file.path(getwd(), "look at app.jpg"),
         width = "100%",
         height = 400)
  }, deleteFile = FALSE)
  
  output$description_img <- renderImage({
    list(src = file.path(getwd(), "122.jpg"),
         width = "100%",
         height = 350)
  }, deleteFile = FALSE)
  
  output$introduction_img <- renderImage({
    list(src = file.path(getwd(), "stop_dia.jpg"),
         width = "100%",
         height = 400)
  }, deleteFile = FALSE)
  
  output$symptoms_img <- renderImage({
    list(src = file.path(getwd(), "symptoms.png"),
         width = "100%",
         height = 450)
  }, deleteFile = FALSE)
  
  output$prediction_img <- renderImage({
    list(src = file.path(getwd(), "check.jpg"),
         width = "100%",
         height = 400)
  }, deleteFile = FALSE)
  
  # Define reactive plot
  output$plot <- renderPlot({
    if (input$plot_type == "Heatmap") {
      # Heatmap
      heatmap_data <- as.matrix(cor(data))
      heatmap(heatmap_data,
              Rowv = NA,
              Colv = NA,
              col = colorRampPalette(c("white", "lightblue", "darkblue"))(100),
              scale = "none",
              margins = c(5, 10),
              main = "Correlation Heatmap",
              xlab = "",
              ylab = "")
    } else if (input$plot_type == "Diabetes Pedigree Function Histogram") {
      # Histogram for Diabetes Pedigree Function
      ggplot(data, aes(x = DiabetesPedigreeFunction)) +
        geom_histogram(fill = "lightblue", color = "black", bins = 20) +
        labs(title = "Diabetes Pedigree Function Histogram",
             x = "Diabetes Pedigree Function",
             y = "Frequency")
    } else if (input$plot_type == "Boxplots for Outcomes and Different Variables") {
      # Boxplots for outcomes and different variables
      data_long <- gather(data, key = "Variable", value = "Value", -Outcome, -Pregnancies,-DiabetesPedigreeFunction, -BMI, -Insulin)
      ggplot(data_long, aes(x = Variable, y = Value, fill = factor(Outcome))) +
        geom_boxplot() +
        labs(title = "Boxplots for Outcomes and Different Variables",
             x = "Variable",
             y = "Value")
    } else if (input$plot_type == "Boxplots - BMI, Pregnancies vs Outcome") {
      # Boxplots for BMI, Pregnancies vs Outcome
      data_long <- gather(data, key = "Variable", value = "Value", -Outcome)
      data_filtered <- data_long[data_long$Variable %in% c("BMI", "Pregnancies"), ]
      
      ggplot(data_filtered, aes(x = Variable, y = Value, fill = factor(Outcome))) +
        geom_boxplot() +
        labs(title = "Boxplots - BMI, Pregnancies vs Outcome",
             x = "Variable",
             y = "Value")
    } else if (input$plot_type == "Violin Plot - Diabetes Pedigree Function vs Outcome") {
      # Violin Plot for Diabetes Pedigree Function vs Outcome
      ggplot(data, aes(x = factor(Outcome), y = DiabetesPedigreeFunction)) +
        geom_violin(fill = "lightblue") +
        labs(title = "Violin Plot - Diabetes Pedigree Function vs Outcome",
             x = "Outcome",
             y = "DiabetesPedigreeFunction")
    } else if (input$plot_type == "Violin Plot - Insulin vs Outcome") {
      # Violin Plot for Insulin vs Outcome
      ggplot(data, aes(x = factor(Outcome), y = Insulin)) +
        geom_violin(fill = "lightblue") +
        labs(title = "Violin Plot - Insulin vs Outcome",
             x = "Outcome",
             y = "Insulin")
    } else if (input$plot_type == "Descriptive Statistics") {
      # Descriptive Statistics
      output$summary <- renderPrint({
        summary(data)
      })
    }
  })
  
  # Define model training and prediction
  observeEvent(input$predict_button, {
    # Prepare data for modeling
    new_data <- data.frame(Pregnancies = input$Pregnancies,
                           Glucose = input$Glucose,
                           BloodPressure = input$BloodPressure,
                           SkinThickness = input$SkinThickness,
                           Insulin = input$Insulin,
                           BMI = input$BMI,
                           DiabetesPedigreeFunction = input$DiabetesPedigreeFunction,
                           Age = input$Age)
    
    # Train logistic regression model
    model <- glm(Outcome ~ ., data = data, family = binomial)
    
    # Predict using the model
    prediction <- predict(model, newdata = new_data, type = "response")
    
    # Display prediction result
    output$prediction_text <- renderText({
      if (prediction > 0.5) {
        "The model predicts that the individual has diabetes."
      } else {
        "The model predicts that the individual does not have diabetes."
      }
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
