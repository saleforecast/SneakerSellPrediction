FROM rocker/shiny:4
# install R packages required 
# Change the packages list to suit your needs
RUN R -e "install.packages(c('googlesheets4', 'dplyr', 'fpp2', 'ggfortify', 'lubridate', 'memoise', 'openxlsx', 'scales', 'shiny', 'stringr', 'tibble', 'readr'), dependencies=TRUE)"

WORKDIR /home/shinyusr
COPY app.R app.R 
COPY .secrets .secrets
COPY Data Data
COPY rsconnect rsconnect
COPY .RData .RData
COPY .Rhistory .Rhistory
COPY CommonPlot.R CommonPlot.R
COPY DailyForecast.R DailyForecast.R
COPY DataImputation.R DataImputation.R
COPY DateFormat.R DateFormat.R
COPY FindDateTimeColumnIndex.R FindDateTimeColumnIndex.R
COPY NumericColumnAnalysis.R NumericColumnAnalysis.R
COPY ParseNumber.R ParseNumber.R
COPY SharedFunc.R SharedFunc.R
COPY ui.R ui.R
CMD Rscript app.R
