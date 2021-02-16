# Information
This application makes use of the electrical power generation obtained from the U.S Energy Information Administration website. The link can be accessed here https://www.eia.gov/electricity/data/state/.
The csv file contains 5 attributes: Year, State, Type of Producer, Energy Source and Generation. There are a total of 53,756 records from 1990 to 2019.

# Instruction
To run this application, R language and RStudio version 1.4.1103 is recommended. In this application, the library packages required are shiny, shinydashboard, ggplot2, lubridate, DT, and usmap.
To install R, click the link https://www.r-project.org/ and choose the version 4.0.4.
To install RStudio, click the link https://rstudio.com/products/rstudio/download/ and choose the RStudio Desktop version.
After downloading and installing R and RStudio, to install the required packages to run the code, in RStudio application on the bottom left section named "Console", run the 'code install.packages()' where inside the brackets, put in the name of the library to install. Note that quotation marks are required. (For example, to install shiny package use the code 'library("shiny")'')
set the working directory to the folder path where the csv file is located. This can be done using the code 'setwd()' where the path to the folder is inside the bracket.
To run the code open the app.R file in RStudio and select Run App in the top middle section of the application.
