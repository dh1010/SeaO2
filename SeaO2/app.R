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
  fluidRow(
    column(width = 3,
                 selectInput("parms",
                             "Choose two carbonate system parameters:",
                             choices = c("8_pH and Alkalinity",
                                         "9_pH and DIC",
                                         "15_Alkalinity and DIC",
                                         "21_pCO2 and pH",
                                         "24_pCO2 and Alkalinity",
                                         "25_pCO2 and DIC"),,
                             width = "6cm"),
                 numericInput('Var1', 'Variable 1: (comma delimited)', "8",  width = "6cm"),
                 numericInput('Var2', 'Variable 2: (comma delimited)', "0.002",  width = "6cm")
    ),
    column(width = 3,
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
           numericInput("TP", 
                        "Total P (uM):", 
                        value = 3),
           numericInput("Sit", 
                        "Total Si (uM):", 
                        value = 55)       
    ),
    column(width = 3,
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
           h6("pH scale = SWS"),
           h6("Created using the seacarb package version 3.2.6 "),
           actionButton("Run_model", "Run SeaO2")
           ),
    
    # Show a plot of the generated distribution
    column(width = 12,
      tabsetPanel(
         tabPanel("Table",
           # tableOutput("carbprev"),
          tableOutput("table"))
         # tabPanel("Plot",
         #  plotOutput("plot1"))
    ))))
    
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  fulltab <- data.frame()
  
  observe({
    p = as.numeric(str_split_fixed(input$parms, "_", 4)[,1])
    if(p == 8){
      updateNumericInput(session,'Var1', value = "7.9", label = "pH", step = 0.1)
      updateNumericInput(session, 'Var2', value = "0.0023", label = "Alkalinity (eq/L)", step = 0.0001)}
    if(p == 9){
      updateNumericInput(session,'Var1', value = "7.9", label = "pH", step = 0.1)
      updateNumericInput(session, 'Var2', value = "0.0021", label = "DIC (M)", step = 0.0001)}
    if(p == 15){
      updateNumericInput(session,'Var1', value = "0.0023", label = "Alkalinity (eq/L)", step = 0.0001)
      updateNumericInput(session, 'Var2', value = "0.0021", label = "DIC (M)", step = 0.0001)}
    if(p == 21){
      updateNumericInput(session,'Var1', value = "600", label = "pCO2 (uatm)", step = 1)
      updateNumericInput(session, 'Var2', value = "7.9", label = "pH", step = 0.0001)}
    if(p == 24){
      updateNumericInput(session,'Var1', value = "600", label = "pCO2 (uatm)", step = 1)
      updateNumericInput(session, 'Var2', value = "0.0023", label = "Alkalinity (eq/L), step = 0.0001")}
    if(p == 25){
      updateNumericInput(session,'Var1', value = "600", label = "pCO2 (uatm)", step = 1)
      updateNumericInput(session, 'Var2', value = "0.0021", label = "DIC (M)", step = 0.0001)}
  })
  
  carbdat <- eventReactive(input$Run_model,{
    carb(flag = as.numeric(str_split_fixed(input$parms, "_", 4)[,1]),
         var1 = input$Var1,
         var2 = input$Var2, 
         S = input$Salinity, 
         T = input$Temperature,
         Pt = as.numeric(input$TP)/1000000,
         Sit = as.numeric(input$Sit)/1000000,
         pHscale = "SWS"
         )%>%
      mutate(ID = row_number(),
             DIC_mM = DIC * 1000,
             CO2_mM = CO2 * 1000,
             CO2pct = (CO2/DIC)*100,
             HCO3_mM = HCO3 * 1000,
             HCO3pct = (HCO3/DIC)*100,
             CO3_mM = CO3 * 1000,
             CO3pct = (CO3/DIC)*100,
             ALK_mM = ALK * 1000)%>%
      select(ID, pH, ALK_mM, DIC_mM, CO3_mM, CO3pct, HCO3_mM, HCO3pct, CO2_mM, CO2pct, OmegaCalcite, OmegaAragonite, pCO2, fCO2, pCO2insitu, fCO2insitu, pCO2pot, fCO2pot, T, S, P, Patm, flag)})
  
  carbspec <- reactive({select(carbdat(), ALK, CO2, HCO3, CO3)}%>%gather(Species, Conc_mM, CO2:CO3))
  
  output$carbprev <- renderTable(carbspec())
  output$table <-renderTable(fulltab <<- rbind(fulltab, carbdat())%>%mutate(ID = row_number()))
  
  output$plot1<-renderPlot({ggplot(fulltab)+
      geom_line(aes(x = ALK, y = OmegaCalcite))+
      theme_bw()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

