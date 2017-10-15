#################################################
# Packages
#################################################

instant_pkgs <- function(pkgs) {  
  pkgs_miss <- pkgs[which(!pkgs %in% installed.packages()[, 1])] 
  if (length(pkgs_miss) > 0) { 
    install.packages(pkgs_miss) 
  } 
  
  if (length(pkgs_miss) == 0) { 
    message("\n ...Packages were already installed!\n") 
  } 
  
  # install packages not already loaded: 
  pkgs_miss <- pkgs[which(!pkgs %in% installed.packages()[, 1])] 
  if (length(pkgs_miss) > 0) { 
    suppressPackageStartupMessages(install.packages(pkgs_miss)) 
  } 
  
  # load packages not already loaded: 
  attached <- search() 
  attached_pkgs <- attached[grepl("package", attached)] 
  need_to_attach <- pkgs[which(!pkgs %in% gsub("package:", "", attached_pkgs))] 
  
  if (length(need_to_attach) > 0) { 
    for (i in 1:length(need_to_attach)) 
      suppressPackageStartupMessages(require(need_to_attach[i], character.only = TRUE)) 
  } 
  
  if (length(need_to_attach) == 0) { 
    message("\n ...Packages were already loaded!\n") 
  } 
} 

packages_to_load <- c("readxl", "dplyr", "tidyr", "magrittr", 
                      "ggplot2", "shiny", "ggthemes",
                      "shinydashboard", "lubridate")

instant_pkgs(packages_to_load)

## Setup ggplot theme
theme_project <- function() {
  theme_bw()+
    theme(legend.position = "top") 
}

######################################################################
# Importing Data
######################################################################
# downloads <- "C:\\Users\\Jacob\\Downloads\\"
dropbox <- "C:\\Users\\Jacob\\Dropbox\\Semester 2 2017\\MXB344\\Project\\Data\\"

location <- dropbox

load(file = paste0(location, "domseedlingcover_rq1.RData"))
load(file = paste0(location, "domshannons_rq1.RData"))
load(file = paste0(location, "domsla_rq1.RData"))
load(file = paste0(location, "hobo_rq2.RData"))
load(file = paste0(location, "species_richness_rq3.RData"))
load(file = paste0(location, "species_rawabiotic_modelling.RData"))


# df2.data <- read_excel(paste0(location, "jenn_data.xls"), sheet = "seedling")
# df2.desc <- read_excel(paste0(location, "jenn_data.xls"), sheet = "description")
# df2.sub8 <- read_excel(paste0(location, "jenn_data.xls"), sheet = "example8")
# 
# df2.data %<>% mutate(dist = as.numeric(dist),
#                      richness = as.integer(richness)) %>%
#   filter(type != 'S') %>%
#   mutate(type_code = ifelse(type == "R", 1, 0))



# Test hwo to use modelling in dashboard
# fit <- glm(dom_simpson ~ Site,
#            family = binomial(link = "logit"),
#            data = quadrat_modelling.df)



######################################################################
# Create Shiny App
######################################################################
# Define UI for application that draws a histogram
dashboard_ui <- shinyUI(
  dashboardPage(
    dashboardHeader(title = "Rainforest Analysis Dashboard"),
    dashboardSidebar(),
    dashboardBody(
      navbarPage(
        title = "Limestone Rainforest",
        tabPanel("All Raw Data",
                 fluidRow(
                   column(6,
                          box(title = "Graph", solidHeader = TRUE,
                              status = "primary", width = 200,
                              plotOutput("exploratoryPlot_sa")
                          )
                   ),
                   column(3,
                          selectInput("response_sa", "Response",
                                      choices = names(species_rawabiotic_modelling.df),
                                      selected = "dom_simpson"),
                          selectInput("predictor_sa", "Predictor",
                                      choices = names(species_rawabiotic_modelling.df),
                                      selected = "site")
                   ),
                   column(3,
                          selectInput("colour_group_sa", "Colour Grouping",
                                      choices = names(species_rawabiotic_modelling.df),
                                      selected = "site"),
                          selectInput("plotDesign_sa", "Plot Design",
                                      choices = c("Point"),
                                      selected = "Point")
                   )
                 )
        ),
        tabPanel("RQ1 - Seedling Cover",
                 fluidRow(
                   column(6,
                          box(title = "Graph", solidHeader = TRUE,
                              status = "primary", width = 200,
                              plotOutput("exploratoryPlot")
                          )
                   ),
                   column(3,
                          selectInput("response", "Response",
                                      choices = names(domseedlingcover_rq1.df),
                                      selected = "dom_seedling_cover"),
                          selectInput("predictor", "Predictor",
                                      choices = names(domseedlingcover_rq1.df),
                                      selected = "site")
                   ),
                   column(3,
                          selectInput("colour_group", "Colour Grouping",
                                      choices = names(domseedlingcover_rq1.df),
                                      selected = "site"),
                          selectInput("plotDesign", "Plot Design",
                                      choices = c("Line", "Point", "Box Plot"),
                                      selected = "Point")
                   )
                 )
        ),
        tabPanel("RQ1 - Shannons Index",
                 fluidRow(
                   column(6,
                          box(title = "Graph", solidHeader = TRUE,
                              status = "primary", width = 200,
                              plotOutput("exploratoryPlot_s")
                          )
                   ),
                   column(3,
                          selectInput("response_s", "Response",
                                      choices = names(domshannons_rq1.df),
                                      selected = "dom_shannon"),
                          selectInput("predictor_s", "Predictor",
                                      choices = names(domshannons_rq1.df),
                                      selected = "site")
                   ),
                   column(3,
                          selectInput("colour_group_s", "Colour Grouping",
                                      choices = names(domshannons_rq1.df),
                                      selected = "site"),
                          selectInput("plotDesign_s", "Plot Design",
                                      choices = c("Line", "Point", "Box Plot"),
                                      selected = "Point")
                   )
                 )
        ),
        tabPanel("RQ1 - SLA",
                 fluidRow(
                   column(6,
                          box(title = "Graph", solidHeader = TRUE,
                              status = "primary", width = 200,
                              plotOutput("exploratoryPlot_s")
                          )
                   ),
                   column(3,
                          selectInput("response_s", "Response",
                                      choices = names(domsla_rq1.df),
                                      selected = "av_dom_sla_from_ind"),
                          selectInput("predictor_s", "Predictor",
                                      choices = names(domsla_rq1.df),
                                      selected = "site")
                   ),
                   column(3,
                          selectInput("colour_group_s", "Colour Grouping",
                                      choices = names(domsla_rq1.df),
                                      selected = "site"),
                          selectInput("plotDesign_s", "Plot Design",
                                      choices = c("Line", "Point", "Box Plot"),
                                      selected = "Point")
                   )
                 )
        ),
        tabPanel("RQ2 - Hobo",
                 fluidRow(
                   column(6,
                          box(title = "Graph", solidHeader = TRUE,
                              status = "primary", width = 200,
                              plotOutput("exploratoryPlot_h")
                          )
                   ),
                   column(3,
                          selectInput("response_h", "Response",
                                      choices = names(hobo_rq2.df),
                                      selected = "temp"),
                          selectInput("predictor_h", "Predictor",
                                      choices = names(hobo_rq2.df),
                                      selected = "date_time")
                   ),
                   column(3,
                          selectInput("colour_group_h", "Colour Grouping",
                                      choices = names(hobo_rq2.df),
                                      selected = "hobo_id"),
                          selectInput("plotDesign_h", "Plot Design",
                                      choices = c("Line", "Point", "Box Plot"),
                                      selected = "Line"),
                          selectInput("min_date_time", "From:",
                                      choices = sort(unique(hobo_rq2.df$date_time)),
                                      selected = sort(unique(hobo_rq2.df$date_time))[60]),
                          selectInput("max_date_time", "To:",
                                      choices = sort((unique(hobo_rq2.df$date_time)),
                                                     decreasing = TRUE)),
                         checkboxGroupInput("hobo_choice", "Hobo ID",
                                            choices = sort(unique(hobo_rq2.df$hobo_id)),
                                            selected = c(3, 6, 8, 13))
                   )
                 )
        ),
        tabPanel("RQ3 - Species Richness",
                 fluidRow(
                   column(6,
                          box(title = "Graph", solidHeader = TRUE,
                              status = "primary", width = 200,
                              plotOutput("exploratoryPlot_s")
                          )
                   ),
                   column(3,
                          selectInput("response_s", "Response",
                                      choices = names(species_richness_rq3.df),
                                      selected = "richness"),
                          selectInput("predictor_s", "Predictor",
                                      choices = names(species_richness_rq3.df),
                                      selected = "site")
                   ),
                   column(3,
                          selectInput("colour_group_s", "Colour Grouping",
                                      choices = names(species_richness_rq3.df),
                                      selected = "site"),
                          selectInput("plotDesign_s", "Plot Design",
                                      choices = c("Line", "Point", "Box Plot"),
                                      selected = "Point")
                   )
                 )
        )
        # ,tabPanel("Model Development",
        #           fluidPage(
        #             fluidRow(
        #               column(6,
        #                      box(title = "Model Output", solidHeader = TRUE,
        #                          status = "primary", width = 200,
        #                          tableOutput("modelFit")
        #                      )
        #               )
        #             ),
        #             fluidRow(
        #               column(3,
        #                      selectInput("model_response", "Response",
        #                                  choices = names(species_rawabiotic_modelling.df),
        #                                  selected = "dom_simpson")
        #               ),
        #               column(3,
        #                      checkboxGroupInput("model_predictors", "Predictors",
        #                                         choices = names(quadrat_modelling.df),
        #                                         selected = c("av_lai"))
        #               ),
        #               column(3,
        #                      selectInput("family", "Family",
        #                                  choices = c("binomial", "poisson", "gaussian"),
        #                                  selected = "poisson"),
        #                      selectInput("link", "GLM Link",
        #                                  choices = c("logit", "probit", "log"),
        #                                  selected = "log"),
        #                      selectInput("model_type", "Model",
        #                                  choices = c("Frequentist", "Bayesian"),
        #                                  selected = "Frequentist")
        #               )
        #             )
        #           )
        # )
        # ,tabPanel("Secondary Data - Exploratory Analysis",
        #          plotOutput("exploratoryPlot2"),
        #          fluidRow(
        #            column(3,
        #                   selectInput("response2", "Response",
        #                               choices = names(df2.data),
        #                               selected = "richness"),
        #                   selectInput("predictor2", "Predictor",
        #                               choices = names(df2.data),
        #                               selected = "type"),
        #                   selectInput("plotDesign2", "Plot Design",
        #                               choices = c("Line", "Point", "Box Plot"),
        #                               selected = "Point"))
        #          ))
        # ,tabPanel("Secondary Data - Model Development",
        #          fluidPage(
        #            tableOutput("modelFit2"),
        #            fluidRow(
        #              column(3,
        #                     selectInput("model_response2", "Response",
        #                                 choices = names(df2.data),
        #                                 selected = "richness")),
        #              column(3,
        #                     checkboxGroupInput("model_predictors2", "Predictors",
        #                                        choices = names(df2.data),
        #                                        selected = c("LAI"))
        #              ),
        #              column(3,
        #                     selectInput("family2", "GLM Family",
        #                                 choices = c("binomial", "poisson"),
        #                                 selected = "poisson"),
        #                     selectInput("link2", "Canonical Link",
        #                                 choices = c("logit", "probit", "log"),
        #                                 selected = "log"))
        #            )
        #          )
        # )
      )
    )
  )
)

# Define server logic required
dashboard_server <- function(input, output, server) {
  
  ######################################################
  # Quadrat lvl dataset exporation
  ########################################################
  exploratoryData <- reactive({
    quadrat_modelling.df %>%
      mutate_("response_var" = input$response,
              "predictor_var" = input$predictor,
              "col_var" = input$colour_group) %>%
      mutate(col_var = factor(col_var, ordered = TRUE))
    
  })
  
  
  output$exploratoryPlot <- renderPlot({
    if (!is.numeric(exploratoryData()[[input$predictor]][1]) & input$plotDesign == "Box Plot") {
      exploratoryData() %>%
        ggplot(aes(x=predictor_var, y=response_var, col=col_var)) +
        geom_point(size=3) +
        geom_boxplot() +
        theme_project() +
        xlab(input$predictor) +
        ylab(input$response) +
        scale_colour_tableau(name = input$colour_group)
    } else if (input$plotDesign == "Line"){
      exploratoryData() %>%
        ggplot(aes(x=predictor_var, y=response_var, col=col_var)) +
        geom_point(size=3) +
        geom_line() +
        theme_project() +
        xlab(input$predictor) +
        ylab(input$response) +
        scale_colour_tableau(name = input$colour_group)
    } else {
      exploratoryData() %>%
        ggplot(aes(x=predictor_var, y=response_var, col=col_var)) +
        geom_point(size=3) +
        theme_project() +
        xlab(input$predictor) +
        ylab(input$response) +
        scale_colour_tableau(name = input$colour_group)
    }
  })
  
  ###################################################
  # Species Exploratory Analysis
  ###################################################
  exploratoryData_s <- reactive({
    species_modelling.df %>%
      mutate_("response_var" = input$response_s,
              "predictor_var" = input$predictor_s,
              "col_var" = input$colour_group_s) %>%
      mutate(col_var = factor(col_var, ordered = TRUE))
    
  })
  
  
  output$exploratoryPlot_s <- renderPlot({
    if (!is.numeric(exploratoryData_s()[[input$predictor_s]][1]) & input$plotDesign_s == "Box Plot") {
      exploratoryData_s() %>%
        ggplot(aes(x=predictor_var, y=response_var, col=col_var)) +
        geom_point(size=3) +
        geom_boxplot() +
        theme_project() +
        xlab(input$predictor_s) +
        ylab(input$response_s) +
        scale_colour_tableau(name = input$colour_group_s)
    } else if (input$plotDesign_s == "Line"){
      exploratoryData_s() %>%
        ggplot(aes(x=predictor_var, y=response_var, col=col_var)) +
        geom_point(size=3) +
        geom_line() +
        theme_project() +
        xlab(input$predictor_s) +
        ylab(input$response_s) +
        scale_colour_tableau(name = input$colour_group_s)
    } else {
      exploratoryData_s() %>%
        ggplot(aes(x=predictor_var, y=response_var, col=col_var)) +
        geom_point(size=3) +
        theme_project() +
        xlab(input$predictor_s) +
        ylab(input$response_s) +
        scale_colour_tableau(name = input$colour_group_s)
    }
  })
  
  ###################################################
  # Species and Raw Abiotic Exploratory Analysis
  ###################################################
  exploratoryData_sa <- reactive({
    species_modelling.df %>%
      mutate_("response_var" = input$response_sa,
              "predictor_var" = input$predictor_sa,
              "col_var" = input$colour_group_sa) %>%
      mutate(col_var = factor(col_var, ordered = TRUE))
    
  })
  
  
  output$exploratoryPlot_sa <- renderPlot({
    if (!is.numeric(exploratoryData_sa()[[input$predictor_sa]][1]) & input$plotDesign_sa == "Box Plot") {
      exploratoryData_sa() %>%
        ggplot(aes(x=predictor_var, y=response_var, col=col_var)) +
        geom_point(size=3) +
        geom_boxplot() +
        theme_project() +
        xlab(input$predictor_sa) +
        ylab(input$response_sa) +
        scale_colour_tableau(name = input$colour_group_sa)
    } else if (input$plotDesign_sa == "Line"){
      exploratoryData_sa() %>%
        ggplot(aes(x=predictor_var, y=response_var, col=col_var)) +
        geom_point(size=3) +
        geom_line() +
        theme_project() +
        xlab(input$predictor_sa) +
        ylab(input$response_sa) +
        scale_colour_tableau(name = input$colour_group_sa)
    } else {
      exploratoryData_sa() %>%
        ggplot(aes(x=predictor_var, y=response_var, col=col_var)) +
        geom_point(size=3) +
        theme_project() +
        xlab(input$predictor_sa) +
        ylab(input$response_sa) +
        scale_colour_tableau(name = input$colour_group_sa)
    }
  })
  
  ###################################################
  # Hobo Exploratory Analysis
  ###################################################
  exploratoryData_h <- reactive({
    output <- hobo_modelling.df %>%
      mutate_("response_var" = input$response_h,
              "predictor_var" = input$predictor_h,
              "col_var" = input$colour_group_h) %>%
      mutate(col_var = factor(col_var, ordered = TRUE)) %>%
      filter(date_time > input$min_date_time, date_time < input$max_date_time,
             hobo_id %in% input$hobo_choice)

    
  })
  
  
  output$exploratoryPlot_h <- renderPlot({
    if (!is.numeric(exploratoryData_h()[[input$predictor_h]][1]) & input$plotDesign_h == "Box Plot") {
      exploratoryData_h() %>%
        ggplot(aes(x=predictor_var, y=response_var, col=col_var)) +
        geom_point(size=1) +
        geom_boxplot() +
        theme_project() +
        xlab(input$predictor_h) +
        ylab(input$response_h) +
        scale_colour_tableau(name = input$colour_group_h)
    } else if (input$plotDesign_h == "Line"){
      exploratoryData_h() %>%
        ggplot(aes(x=predictor_var, y=response_var, col=col_var)) +
        geom_point(size=1) +
        geom_line(size = 1) +
        theme_project() +
        xlab(input$predictor_h) +
        ylab(input$response_h) +
        scale_colour_tableau(name = input$colour_group_h)
    } else {
      exploratoryData_h() %>%
        ggplot(aes(x=predictor_var, y=response_var, col=col_var)) +
        geom_point(size=1) +
        theme_project() +
        xlab(input$predictor_h) +
        ylab(input$response_h) +
        scale_colour_tableau(name = input$colour_group_h)
    }
  })
  
  ###########################################
  # Dashboard Modelling
  ############################################
  output$modelFit <- renderTable({
    
    formula.predictor <- input$model_predictors[1]
    
    if(length(input$model_predictors) >= 2) {
      for (i in 2:length(input$model_predictors)) {
        formula.predictor <- paste(formula.predictor, 
                                   input$model_predictors[i], 
                                   sep=" + ")
      }
    }
    
    # Frequentist Modelling
    if (input$model_type == "Frequentist") {
      
      if(input$family == "Binomial") {
        fit <- glm(paste(input$model_response, formula.predictor, sep=" ~ "),
                   family = binimial(link = paste0(input$link)),
                   data = species_rawabiotic_modelling.df)
        
        outputTable <- data.frame(Variable = row.names(summary(fit)$coefficients),
                                  Coefficients = summary(fit)$coefficients[1:(length(input$model_predictors) + 1)],
                                  StandardError = summary(fit)$coefficients[(2+length(input$model_predictors)):(2*(length(input$model_predictors) + 1))],
                                  P_Value = summary(fit)$coefficients[(3*(length(input$model_predictors) + 1) + 1):(4*(length(input$model_predictors) + 1))]) %>%
          mutate(Significance = ifelse(P_Value < 0.01, "***", 
                                       ifelse(P_Value < 0.05, "**", 
                                              ifelse(P_Value < 0.1, "*",
                                                     "-"))))
      }
      
    } else {
      
      outputTable <- "Insert INLA Analysis..."
      
    }
    
  })
  
}

# Run the application 
shinyApp(ui = dashboard_ui, server = dashboard_server)

