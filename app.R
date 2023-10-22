options(shiny.maxRequestSize=30*1024^2)
source("CommonPlot.R")
source("DailyForecast.R")
source("DateFormat.R")
source("DataImputation.R")
source("FindDateTimeColumnIndex.R")
source("NumericColumnAnalysis.R")
source("ParseNumber.R")
source("SharedFunc.R")
library(googlesheets4)
library(dplyr, warn.conflicts = FALSE)
library(fpp2, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(cli, warn.conflicts = FALSE)
library(crayon, warn.conflicts = FALSE)
library(expsmooth, warn.conflicts = FALSE)
library(fma, warn.conflicts = FALSE)
library(magrittr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)
library(rstudioapi, warn.conflicts = FALSE)
library(ggfortify, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(memoise, warn.conflicts = FALSE)
library(openxlsx, warn.conflicts = FALSE)
library(scales, warn.conflicts = FALSE)
library(shiny)
library(stringr, warn.conflicts = FALSE)
library(tibble, warn.conflicts = FALSE)
library(readr, warn.conflicts = FALSE)
options(dplyr.summarise.inform = FALSE)
options(dplyr.warn.conflicts = FALSE)
options(shiny.host = "0.0.0.0")
options(shiny.port = 5000)

shinyServer(function(input,output,session){
  observe({
    #datf <- read_csv("C:/Users/Shafi/OneDrive/Demo/SneakerSellPrediction/Data/sneakerData.csv", col_types =  cols(.default = col_character()))
    #datf <- read_csv("https://www.dropbox.com/scl/fi/d25m2vu1qs4ff7zblrjvf/CustomerArrival.csv?rlkey=ldpzllxv3e57zph85ybgof7ch&dl=1", col_types =  cols(.default = col_character()))    
    gs4_auth(cache = ".secrets", email = "shafiul0304034@gmail.com")
    datf <- read_sheet("https://docs.google.com/spreadsheets/d/19ZsUaiKat_MC0zHJv1cKY0G_y1E91WEna7AFIalddCo/edit#gid=0", col_types = "c")  
    #browser()
    #datf <- head(datf, 60)
    colIndicator <- findDateTimeColumnIndex(datf)
    if(colIndicator[[1]] != 0 || (colIndicator[[2]] != 0 && colIndicator[[3]] != 0))
    {
      datf <- getDataFrame() %>% na.omit()
      splitedDatf <- splitDatfForDateTimeCol(datf, colIndicator[[1]], colIndicator[[2]], colIndicator[[3]])      
    }
    else if(colIndicator[[1]] == 0 && (colIndicator[[2]] != 0 && colIndicator[[3]] == 0))
    {
      datf <- datf %>% na.omit()
      splitedDatf <- splitDatfForDateCol(datf, colIndicator[[1]], colIndicator[[2]], colIndicator[[3]])      
    }
    
    lapply(4:NCOL(splitedDatf), function(rowIndex){
      splitedDatfC <- splitedDatf %>% .[c(1,2,3,rowIndex)]
      plot <- dailyForecastAndPlot(splitedDatfC)
      insertUI(selector = '#forecast', where = "afterEnd", ui = 
                    tagList(
                      renderPlot(plot[[1]]), 
                      renderDataTable(plot[[2]], options = list(pageLength = 5, info = FALSE))
                    )
               ) 
    })
  })
  session$onSessionEnded(function() {
    stopApp()
  })
})

# output$photo <- renderImage({
# list(
#   src = file.path("Image", paste0(colnames(splitedDatfC)[4], ".png")), #"SingleRoomPrice.png"),
#   contentType = "image/png",
#   width = "100%", #width*pixelratio,
#   height = "100%" #height*pixelratio
# )
# }, deleteFile = FALSE)

#read_csv("/home/shafiul/OneDrive/UnitCodeSale4Cast/DailyBioProductPrice/Data/FoodPrice.csv")
