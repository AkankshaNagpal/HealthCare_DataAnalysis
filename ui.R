
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

#library(shiny)
require(rHighcharts)
require(shiny)
shinyUI(fluidPage(


  # Application title
  titlePanel(title=h1("Health Care-Inpatient Prospective Payment System",align="left",style="margin-top: 0;
  margin-bottom: 0;
  font-size: 40px;
  color:white;border-style: solid;
    border-width: 4px;background-color:#317eac;border-color: white;")
  ),
  br(),

  # Sidebar with a slider input for number of bins
  sidebarLayout(

    sidebarPanel(
      br(),
      selectInput("dataset","Choose a Dataset",choices=c("patient","patient_state_discharges","patient_state_charges",
                                                         "patient_avg_total_payment","patient_avg_Medi_payment")),
      br(),
      numericInput("obs","No. of Obervations",6),
      br(),
      actionButton("act","Click to update the dataset",style="float: none;
  display: table-cell;
  width: 50%;"),
      br(),br(),
      h1("HIT COUNTER"), br(),
      h5(textOutput("count")),
      #selectInput("var","1.Select the variable from data set",choices=c("DRG.Definition"=1,"Provider.Id"=2,"Provider.Name"=3)),
      #br(),
#       #sliderInput("bins",
#                   "Number of bins:",
#                   min = 1,
#                   max = 50,
#                   value = 30),
      br(),
      radioButtons("color","3. Select Color of Chart",choices=c("Green","Red","Yellow"),selected="Red"),
      style=" color: #555555;
  background-color: #ffffff;
  border: 1px solid #dddddd;
border-bottom-color: transparent;
cursor: default;"
    ),
    

    # Show a plot of the generated distribution
    mainPanel(
      #tags$style("body{background-color:#62DEB0;color:#627ADE}"),
      style=" color: #555555;
      background-color: #ffffff;
      border: 1px solid #dddddd;
      border-bottom-color: transparent;
      cursor: default;",
      tabsetPanel(type="tab",
                  tabPanel("Summary",verbatimTextOutput("sum")),
                  tabPanel("Structure",verbatimTextOutput("structure")),
                  tabPanel("Data",tableOutput("data")),
                  tabPanel("View",tableOutput("view")),
                  tabPanel("Cumulative Chart",chartOutput("allCharges_mix"),
                           br(),chartOutput("allCharges_line"),
                           br(),chartOutput("allCharges_column")),
                  tabPanel("Total Discharges",chartOutput("totalDischarges_column"),br(),chartOutput("totalDischarges_line"),
                           br(),chartOutput("totalDischarges_bar"),
                           br(),chartOutput("totalDischarges_pie")),
                  tabPanel("Total Payment",chartOutput("totalPayment_column"),
                           br(),chartOutput("totalPayment_line"),
                           br(),chartOutput("totalPayment_bar"),
                           br(),chartOutput("totalPayment_pie")),
                  tabPanel("Covered Charges",chartOutput("coveredCharges_column"),
                           br(),chartOutput("coveredCharges_line"),
                           br(),chartOutput("coveredCharges_bar"),
                           br(),chartOutput("coveredCharges_pie"))
                  
                  ),
      
      tags$style("body{background-color:#84E8B9;color:orange}")
      #tags$img(src="healthcare.jpg",height=50,width=50)
      #plotOutput("distPlot")
      
    )
  )
 ))
