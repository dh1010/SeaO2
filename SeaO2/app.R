#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(seacarb)
library(shiny)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("SeaO2"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(width = 3,
                 selectInput("parms",
                             "Choose two carbonate system parameters:",
                             choices = c("8_pH and Alkalinity",
                                         "9_pH and DIC",
                                         "15_Alkalinity and DIC",
                                         "21_pCO2 and pH",
                                         "24_pCO2 and Alkalinity",
                                         "25_pCO2 and DIC")),
                 textInput('Var1', 'Variable 1: (comma delimited)', "8"),
                 textInput('Var2', 'Variable 2: (comma delimited)', "0.002"),
                 sliderInput("Salinity",
                             "Salinity:",
                             value = 35,
                             min = 19,
                             max = 40),
                 sliderInput("Temperature",
                             "Temperature (C):",
                             value = 20,
                             min = 9,
                             max = 33),
                 actionButton("Run_model", "Run SeaO2"),
                 # selectInput("plotvarx",
                 #             "x plot:",
                 #             choices = c("pH",
                 #                         "DIC",
                 #                         "ALK",
                 #                         "pCO2",
                 #                         "CO3",
                 #                         "HCO3")),
                 # selectInput("plotvary",
                 #             "y plot:",
                 #             choices = c("pH",
                 #                         "DIC",
                 #                         "ALK",
                 #                         "pCO2",
                 #                         "CO3",
                 #                         "HCO3")),
                 h5("Default values"),
                 # h5("pH = 7.9"),
                 # h5("Alkalinity = 0.0023 mol/kgw"),
                 # h5("DIC = 0.0021 mol/kgw"),
                 # h5("pCO2 = 600 uatm"),
                 # h5("Salinity = 35"),
                 # h5("Temperature 20 C"),
                 h6("klk from Lueker et al. (2000)"),
                 h6("kf from Perez and Fraga (1987)"),
                 h6("ks Dickson (1990)"),
                 h6("Created using the seacarb package version 3.2.6 ")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      textOutput("selected_parms"),
      textOutput("Var1"),
      textOutput("Var2"),
      tableOutput("carbprev"),
      tableOutput("table"),
      plotOutput("plot1"),
      plotOutput("plot2")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  fulltab <- data.frame()
  
  observe({
    p = as.numeric(str_split_fixed(input$parms, "_", 4)[,1])
    if(p == 8){
      updateTextInput(session,'Var1', value = "7.9", label = "pH")
      updateTextInput(session, 'Var2', value = "0.0023", label = "Alkalinity (eq/L)")}
    if(p == 9){
      updateTextInput(session,'Var1', value = "7.9", label = "pH")
      updateTextInput(session, 'Var2', value = "0.0021", label = "DIC (M)")}
    if(p == 15){
      updateTextInput(session,'Var1', value = "0.0023", label = "Alkalinity (eq/L)")
      updateTextInput(session, 'Var2', value = "0.0021", label = "DIC (M)")}
    if(p == 21){
      updateTextInput(session,'Var1', value = "600", label = "pCO2 (uatm)")
      updateTextInput(session, 'Var2', value = "7.9", label = "pH")}
    if(p == 24){
      updateTextInput(session,'Var1', value = "600", label = "pCO2 (uatm)")
      updateTextInput(session, 'Var2', value = "0.0023", label = "Alkalinity (eq/L)")}
    if(p == 25){
      updateTextInput(session,'Var1', value = "600", label = "pCO2 (uatm)")
      updateTextInput(session, 'Var2', value = "0.0021", label = "DIC (M)")}
  })
  
  carbdat <- eventReactive(input$Run_model,{
    carb(flag = as.numeric(str_split_fixed(input$parms, "_", 4)[,1]),
         var1 = as.numeric(unlist(strsplit(input$Var1,","))),
         var2 = as.numeric(unlist(strsplit(input$Var2,","))), 
         S = input$Salinity, 
         T = input$Temperature)%>%
      mutate(ID = row_number(),
             DIC = DIC * 1000,
             CO2 = CO2 * 1000,
             CO2pct = (CO2/DIC)*100,
             HCO3 = HCO3 * 1000,
             HCO3pct = (HCO3/DIC)*100,
             CO3 = CO3 * 1000,
             CO3pct = (CO3/DIC)*100,
             ALK = ALK * 1000)%>%
      select(ID, pH, ALK, DIC, CO3, CO3pct, HCO3, HCO3pct, CO2, CO2pct, OmegaCalcite, OmegaAragonite, pCO2, fCO2, pCO2insitu, fCO2insitu, pCO2pot, fCO2pot, T, S, P, Patm, flag)})
  
  carbspec <- reactive({select(carbdat(), ALK, CO2, HCO3, CO3)}%>%gather(Species, Conc_mM, CO2:CO3))
  
  output$carbprev <- renderTable(carbspec())
  output$table <-renderTable(fulltab <<- rbind(fulltab, carbdat())%>%mutate(ID = row_number()))
  # output$plot1<-renderPlot({ggplot(fulltab)+
  #     geom_line(aes(x = ALK, y = HCO3))+
  #     theme_bw()
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)

