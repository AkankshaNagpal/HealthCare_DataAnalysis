
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(maps)
library(mapproj)
library(devtools)
library(rHighcharts)
library(datasets)
data(patient)

#source("./data/helpers.R")
#patient <- read.csv("./data/patient.csv")
patient_state_discharges <- aggregate(patient$Total.Discharges ~ patient$Provider.State,data=patient,sum)
patient_state_discharges <- patient_state_discharges[order(-patient_state_discharges[[2]]),]

#Average.Covered.Charges
patient_state_charges <- aggregate(patient$Average.Covered.Charges ~ patient$Provider.State,data=patient,sum)
patient_state_charges <- patient_state_charges[order(-patient_state_charges[[2]]),]

#Average.Total.Payments
patient_avg_total_payment <- aggregate(patient$Average.Total.Payments ~ patient$Provider.State,data=patient,sum)
patient_avg_total_payment <- patient_avg_total_payment[order(-patient_avg_total_payment[[2]]),]

#Average.Medicare.Payments
patient_avg_Medi_payment <- aggregate(patient$Average.Medicare.Payments ~ patient$Provider.State,data=patient,sum)
patient_avg_Medi_payment <- patient_avg_Medi_payment[order(-patient_avg_Medi_payment[[2]]),]

tags$head(tags$style("#text1{color: red;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
)
)

shinyServer(function(input, output) {
  
  output$dataname <- renderText({
    
    paste("Structure of dataset", input$dataset)
  })
  
  output$structure <- renderPrint({
    
    str(get(input$dataset))
  })
  
  output$observation <- renderText({
    
    input$act
    isolate(paste("First",input$obs,"Observations of the dataset",input$dataset))
  })
  
  output$view <- renderTable({
    input$act
    isolate(head(get(input$dataset), n = input$obs))
  })
  
  output$count <- renderText({
      if(!file.exists("C:/Users/Akanksha/Desktop/CMPE274/FinalProject/counter.Rdata"))
        counter <- 0
      else
        load(file="C:/Users/Akanksha/Desktop/CMPE274/FinalProject/counter.Rdata")
        counter <- counter + 1
      save(counter,file="C:/Users/Akanksha/Desktop/CMPE274/FinalProject/counter.Rdata")
      paste("Hits:" ,counter)
    })
    
  
  output$test <- renderUI(input$var)

  output$sum <- renderPrint({
    summary(get(input$dataset))
  })
  output$str <- renderPrint({
    
    str(patient)
  })
  
  
  output$data <- renderTable({
    
    colm <- as.numeric(input$var)
    #iris[colm]
    head(get(input$dataset))
    
    
  })
  
  output$allCharges_mix <- renderChart({
    a <- rHighcharts:::Chart$new()
    a$title(text = "Cumulative Chart for Various Charges")
    a$subtitle(text = "")
    a$xAxis(categories = patient_state_discharges[[1]][1:10])
    a$yAxis(title = list(text = "Numbers"))
    
    #a$data(x = patient_state_discharges[[1]][1:10], y = patient_state_discharges[[2]][1:10], color='green',type = "column", name = "Total Discharges")
    a$data(x = patient_state_charges[[1]][1:10], y = patient_state_charges[[2]][1:10], color=input$color,type = "column", name = "Covered Charges")
    a$data(x = patient_avg_total_payment[[1]][1:10], y = patient_avg_total_payment[[2]][1:10], color='blue', type = "line", name = "Avg total Payments")
    a$data(x = patient_avg_Medi_payment[[1]][1:10], y = patient_avg_Medi_payment[[2]][1:10], color='green',type = "line", name = "Average Medicare Payments")
    #a$data(x = data$month, y = data$N.ended, type = "column", name = "Ended")
    a$data(x= NULL)
    return(a)
  })
  
  output$allCharges_line <- renderChart({
    a <- rHighcharts:::Chart$new()
    a$title(text = "Cumulative Chart for Various Charges")
    a$subtitle(text = "unit 1K = 1000")
    a$xAxis(categories = patient_state_discharges[[1]][1:10])
    a$yAxis(title = list(text = "Numbers"))
    
    #a$data(x = patient_state_discharges[[1]][1:10], y = patient_state_discharges[[2]][1:10], color='green',type = "column", name = "Total Discharges")
    a$data(x = patient_state_charges[[1]][1:10], y = patient_state_charges[[2]][1:10], color=input$color,type = "line", name = "Covered Charges")
    a$data(x = patient_avg_total_payment[[1]][1:10], y = patient_avg_total_payment[[2]][1:10], color='blue', type = "line", name = "Avg total Payments")
    a$data(x = patient_avg_Medi_payment[[1]][1:10], y = patient_avg_Medi_payment[[2]][1:10], color='green',type = "line", name = "Average Medicare Payments")
    #a$data(x = data$month, y = data$N.ended, type = "column", name = "Ended")
    a$data(x= NULL)
    return(a)
  })
  
  output$allCharges_column <- renderChart({
    a <- rHighcharts:::Chart$new()
    a$title(text = "Cumulative Chart for Various Charges")
    a$subtitle(text = "unit 1K = 1000")
    a$xAxis(categories = patient_state_discharges[[1]][1:10])
    a$yAxis(title = list(text = "Numbers"))
    
    #a$data(x = patient_state_discharges[[1]][1:10], y = patient_state_discharges[[2]][1:10], color='green',type = "column", name = "Total Discharges")
    a$data(x = patient_state_charges[[1]][1:10], y = patient_state_charges[[2]][1:10], color=input$color,type = "column", name = "Covered Charges")
    a$data(x = patient_avg_total_payment[[1]][1:10], y = patient_avg_total_payment[[2]][1:10], color='blue', type = "column", name = "Avg total Payments")
    a$data(x = patient_avg_Medi_payment[[1]][1:10], y = patient_avg_Medi_payment[[2]][1:10], color='green',type = "column", name = "Average Medicare Payments")
    #a$data(x = data$month, y = data$N.ended, type = "column", name = "Ended")
    a$data(x= NULL)
    return(a)
  })
  
  output$totalDischarges_line <- renderChart({
    a <- rHighcharts:::Chart$new()
    a$title(text = "Total Discharges per State")
    a$subtitle(text = "unit 1K = 1000")
    a$xAxis(categories = patient_state_discharges[[1]][1:10])
    a$yAxis(title = list(text = "Number of Total Discharges"))
    
    a$data(x = patient_state_discharges[[1]][1:10], y = patient_state_discharges[[2]][1:10], type = "line", name = "state")
    #a$data(x = patient_state_discharges[[1]][1:10], y = patient_state_discharges[[2]][1:10], type = "bar", name = "state")
    #a$data(x = data$month, y = data$N.ended, type = "column", name = "Ended")
    #a$data(x= NULL)
    return(a)
  })
  
  output$totalDischarges_bar <- renderChart({
    a <- rHighcharts:::Chart$new()
    a$title(text = "Total Discharges per State")
    a$subtitle(text = "unit 1K = 1000")
    a$xAxis(categories = patient_state_discharges[[1]][1:10])
    a$yAxis(title = list(text = "Number of Total Discharges"))
    
    a$data(x = patient_state_discharges[[1]][1:10], y = patient_state_discharges[[2]][1:10], type = "bar", name = "state")
    #a$data(x = patient_state_discharges[[1]][1:10], y = patient_state_discharges[[2]][1:10], type = "bar", name = "state")
    #a$data(x = data$month, y = data$N.ended, type = "column", name = "Ended")
    #a$data(x= NULL)
    return(a)
  })
  
  output$totalDischarges_pie <- renderChart({
    a <- rHighcharts:::Chart$new()
    a$title(text = "Total Discharges per State")
    a$subtitle(text = "unit 1K = 1000")
    a$xAxis(categories = patient_state_discharges[[1]][1:10])
    a$yAxis(title = list(text = "Number of Total Discharges"))
    
    a$data(x = patient_state_discharges[[1]][1:10], y = patient_state_discharges[[2]][1:10], type = "pie", name = "state")
    #a$data(x = patient_state_discharges[[1]][1:10], y = patient_state_discharges[[2]][1:10], type = "bar", name = "state")
    #a$data(x = data$month, y = data$N.ended, type = "column", name = "Ended")
    #a$data(x= NULL)
    return(a)
  })
  

  output$totalDischarges_column <- renderChart({
    
    a <- rHighcharts:::Chart$new()
    a$title(text = "Total Discharges per State")
    a$subtitle(text = "unit 1K = 1000")
    a$xAxis(categories = patient_state_charges[[1]][1:10])
    a$yAxis(title = list(text = "Number of Total Discharges"))
    
    a$data(x = patient_state_charges[[1]][1:10], y = patient_state_charges[[2]][1:10], color='green',type = "column", name = "Total Discharges")
    #a$data(x = patient_state_charges[[1]][1:10], y = patient_state_charges[[2]][1:10], color='red',type = "column", name = "Covered Charges")
    #a$data(x = patient_avg_total_payment[[1]][1:10], y = patient_avg_total_payment[[2]][1:10], color='blue', type = "column", name = "Avg total Payments")
    #a$data(x = patient_state_discharges[[1]][1:10], y = patient_state_discharges[[2]][1:10], type = "bar", name = "state")
    #a$data(x = data$month, y = data$N.ended, type = "column", name = "Ended")
    a$data(x= NULL)
    return(a)
  })
  
  output$coveredCharges_column <- renderChart({
    a <- rHighcharts:::Chart$new()
    a$title(text = "Covered charges per State")
    a$subtitle(text = "unit 1K = 1000")
    a$xAxis(categories = patient_state_charges[[1]][1:10])
    a$yAxis(title = list(text = "Covered Charges"))
    
    a$data(x = patient_state_charges[[1]][1:10], y = patient_state_charges[[2]][1:10], color='purple',type = "column", name = "state")
    #a$data(x = patient_state_discharges[[1]][1:10], y = patient_state_discharges[[2]][1:10], type = "bar", name = "state")
    #a$data(x = data$month, y = data$N.ended, type = "column", name = "Ended")
    #a$data(x= NULL)
    return(a)
  })
  
  output$coveredCharges_line <- renderChart({
    a <- rHighcharts:::Chart$new()
    a$title(text = "Covered charges per State")
    a$subtitle(text = "unit 1K = 1000")
    a$xAxis(categories = patient_state_charges[[1]][1:10])
    a$yAxis(title = list(text = "Covered Charges"))
    
    a$data(x = patient_state_charges[[1]][1:10], y = patient_state_charges[[2]][1:10], color='red',type = "line", name = "state")
    #a$data(x = patient_state_discharges[[1]][1:10], y = patient_state_discharges[[2]][1:10], type = "bar", name = "state")
    #a$data(x = data$month, y = data$N.ended, type = "column", name = "Ended")
    #a$data(x= NULL)
    return(a)
  })
  
  output$coveredCharges_bar <- renderChart({
    a <- rHighcharts:::Chart$new()
    a$title(text = "Covered charges per State")
    a$subtitle(text = "unit 1K = 1000")
    a$xAxis(categories = patient_state_charges[[1]][1:10])
    a$yAxis(title = list(text = "Covered Charges"))
    
    a$data(x = patient_state_charges[[1]][1:10], y = patient_state_charges[[2]][1:10], color='green',type = "bar", name = "state")
    #a$data(x = patient_state_discharges[[1]][1:10], y = patient_state_discharges[[2]][1:10], type = "bar", name = "state")
    #a$data(x = data$month, y = data$N.ended, type = "column", name = "Ended")
    #a$data(x= NULL)
    return(a)
  })
  
  output$coveredCharges_pie <- renderChart({
    a <- rHighcharts:::Chart$new()
    a$title(text = "Covered charges per State")
    a$subtitle(text = "unit 1K = 1000")
    a$xAxis(categories = patient_state_charges[[1]][1:10])
    a$yAxis(title = list(text = "Covered Charges"))
    
    a$data(x = patient_state_charges[[1]][1:10], y = patient_state_charges[[2]][1:10], type = "pie", name = "state")
    #a$data(x = patient_state_discharges[[1]][1:10], y = patient_state_discharges[[2]][1:10], type = "bar", name = "state")
    #a$data(x = data$month, y = data$N.ended, type = "column", name = "Ended")
    #a$data(x= NULL)
    return(a)
  })
  
  output$totalPayment_column <- renderChart({
    a <- rHighcharts:::Chart$new()
    a$title(text = "Avg total Payments per State")
    a$subtitle(text = "unit 1K = 1000")
    a$xAxis(categories = patient_avg_total_payment[[1]][1:10])
    a$yAxis(title = list(text = "Avg Total Payments"))
    
    a$data(x = patient_avg_total_payment[[1]][1:10], y = patient_avg_total_payment[[2]][1:10], color='pink',type = "column", name = "state")
    #a$data(x = patient_state_discharges[[1]][1:10], y = patient_state_discharges[[2]][1:10], type = "bar", name = "state")
    #a$data(x = data$month, y = data$N.ended, type = "column", name = "Ended")
    #a$data(x= NULL)
    return(a)
  })
  
  output$totalPayment_line <- renderChart({
    a <- rHighcharts:::Chart$new()
    a$title(text = "Avg total Payments per State")
    a$subtitle(text = "unit 1K = 1000")
    a$xAxis(categories = patient_avg_total_payment[[1]][1:10])
    a$yAxis(title = list(text = "Avg Total Payments"))
    
    a$data(x = patient_avg_total_payment[[1]][1:10], y = patient_avg_total_payment[[2]][1:10], color='yellow', type = "line", name = "state")
    #a$data(x = patient_state_discharges[[1]][1:10], y = patient_state_discharges[[2]][1:10], type = "bar", name = "state")
    #a$data(x = data$month, y = data$N.ended, type = "column", name = "Ended")
    #a$data(x= NULL)
    return(a)
  })
  
  output$totalPayment_bar <- renderChart({
    a <- rHighcharts:::Chart$new()
    a$title(text = "Avg total Payments per State")
    a$subtitle(text = "unit 1K = 1000")
    a$xAxis(categories = patient_avg_total_payment[[1]][1:10])
    a$yAxis(title = list(text = "Avg Total Payments"))
    
    a$data(x = patient_avg_total_payment[[1]][1:10], y = patient_avg_total_payment[[2]][1:10], type = "bar", name = "state")
    #a$data(x = patient_state_discharges[[1]][1:10], y = patient_state_discharges[[2]][1:10], type = "bar", name = "state")
    #a$data(x = data$month, y = data$N.ended, type = "column", name = "Ended")
    #a$data(x= NULL)
    return(a)
  })
  
  output$totalPayment_pie <- renderChart({
    a <- rHighcharts:::Chart$new()
    a$title(text = "Avg total Payments per State")
    a$subtitle(text = "unit 1K = 1000")
    a$xAxis(categories = patient_avg_total_payment[[1]][1:10])
    a$yAxis(title = list(text = "Avg Total Payments"))
    
    a$data(x = patient_avg_total_payment[[1]][1:10], y = patient_avg_total_payment[[2]][1:10], type = "pie", name = "state")
    #a$data(x = patient_state_discharges[[1]][1:10], y = patient_state_discharges[[2]][1:10], type = "bar", name = "state")
    #a$data(x = data$month, y = data$N.ended, type = "column", name = "Ended")
    #a$data(x= NULL)
    return(a)
  })
  

})
