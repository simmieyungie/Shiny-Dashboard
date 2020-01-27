#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#library
library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(readr)
library(readxl)
library(flexdashboard)
library(plotly)
library(tidyr)
#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Basic Dashboard") 

#read data
global <- read.csv("global.csv")
country_coor <- read_xlsx("country coord.xlsx")


#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Visit-us", icon = icon("send",lib='glyphicon'))))


#value boxes
frow1 <- fluidRow(
   shinydashboard::valueBoxOutput("value1")
  ,shinydashboard::valueBoxOutput("value2")
  ,shinydashboard::valueBoxOutput("value3")
)



frow2 <- fluidRow(
  box(title = "Test", width = 8, height = "500px",
  plotlyOutput("grp", width = "90%", height = "300px", title = "Country")
    ))


  
frow3 <- fluidRow(  
  box(
    title = "By Region"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("grp1", height = "300px")
  )
)



#body of dashboard
body <- dashboardBody(frow1, frow2, frow3)




#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'This is my Page title', header, sidebar, body,skin='blue')

 


server <- function(input, output, session){
  
  #Build value box
  output$value1 <-shinydashboard::renderValueBox({
    
    newexits <- global  %>%  filter(Type.of.movement == "Exit") %>% nrow()
    shinydashboard::valueBox(newexits, subtitle = "Newexits", 
                             icon = icon("stats", lib ="glyphicon" ), color = "green")
  })
  
  
  output$value2 <- shinydashboard::renderValueBox({
    newhires <- global  %>%  filter(Type.of.movement=="Entry") %>% nrow()
    shinydashboard::valueBox(newhires,subtitle = "Newhires",
                             icon = icon("stats", lib = "glyphicon"), color = "orange")
  })
  
  
  output$value3 <- shinydashboard::renderValueBox({
    netchange <- newhires - newexits
    shinydashboard::valueBox(netchange,subtitle = "Net change",
                            icon = icon("menu-hamburger", lib = "glyphicon"), color = "blue")
  })
  
  output$grp <-  renderPlotly({
    h3=global %>% 
      filter(`Type.of.movement`=="Entry") %>% 
      group_by(Month,Country) %>% 
      summarise(count=n())
    
    #Use spread to make the table ready for plots
    h3=spread(h3,key = Country,value = count)
    
    #Bar chart by country
    plot_ly(h3,
               x=h3$Month,
               hoverinfo="text") %>% 
      add_bars(y=h3$Argentina,
               name="Argentina",
               hovertext=paste(h3$Argentina)) %>% 
      add_bars(y=h3$Australia,
               name="Australia",
               hovertext=paste("Australia: ",h3$Australia)) %>% 
      add_bars(y=h3$Brazil,
               name="Brazil",
               hovertext=paste("Brazil: ",h3$Brazil)) %>%
      add_bars(y=h3$Canada,
               name="Canada",
               hovertext=paste("Canada: ",h3$Canada)) %>%
      add_bars(y=h3$India,
               name="India",
               hovertext=paste("India: ",h3$India)) %>%
      add_bars(y=h3$Romania,
               name="Romania",
               hovertext=paste("Romania: ",h3$Romania)) %>%
      add_bars(y=h3$USA,
               name="USA",
               hovertext=paste("USA",h3$USA))


    })
        
  output$grp1 <- renderPlotly({
    
    h2=global %>% 
      group_by(Month,`Type.of.movement`,Country) %>% 
      summarise(count=n())
    #Add long/lat info based on country name from countr_coor
    h2=left_join(h2,country_coor[,2:4],by=c("Country"="name"))
    #h2 is the table where I want the info
    #country_coor[,2:4] selects only long, lat and name columns
    # so that I can only add the long and lat cols to h2 after the join
    #by=c() identifies the keys in both dataframes
    plot_geo(h2,locationmode="world") %>% 
      add_markers(x=h2$longitude,
                  y=h2$latitude,
                  size=h2$count,
                  color=h2$`Type of movement`,
                  hoverinfo="text",
                  hovertext=paste(h2$`Type of movement`,": ",h2$count)) %>% 
      layout()
    
  
  })
  
}

shinyApp(ui, server)


