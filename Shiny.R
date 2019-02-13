if(!require("shiny")) install.packages("shiny"); library("shiny")
if(!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if(!require("maptools")) install.packages("maptools"); library("maptools")
if(!require("shinydashboard")) install.packages("shinydashboard"); library("shinydashboard")
if(!require("tools")) install.packages("tools"); library("tools")




ui <- dashboardPage(
  dashboardHeader(title = strong("ENTITY EXTRACTION ON JOB DESCRIPTION"),titleWidth = 650),
  dashboardSidebar(tags$style(HTML(".main-sidebar{width: 250px;}")),
                   sidebarMenu(
                     menuItem("Top Skills",tabName = "d1"),
                     menuItem("Regions",tabName = "d2"),
                     menuItem("Jobs in different State",tabName = "d3"),
                     menuItem("Most In Demand Positions",tabName = "d4"))),
  dashboardBody(tabItems(
    tabItem(tabName = "d1",
            fluidPage(titlePanel("TECHNICAL SKILLS"),
            sidebarLayout(
            sidebarPanel(
            selectInput("state1","select a state",
                        choices = unique(final_table$state),
                        selected = "CA")),
          
            plotOutput("plot1")))),
             
            
    tabItem(tabName = "d2",
            fluidPage(titlePanel("Geograhical Overview"),
              plotOutput("plot2")
            )),
    
    tabItem(tabName = "d3",
            fluidPage(titlePanel("Distribution Of jobs in different states"),
          plotOutput("plot3"))),
    
    tabItem(tabName = "d4",
          mainPanel(  fluidPage(titlePanel("Most In Demand Positions"),
                      plotOutput("plot4"))
            ))
    
    
    )))



server <- function(input,output){
  output$plot1 <- renderPlot({
  data1 <- subset(final_table, state %in% input$state1)
  plt <- barplot(colSums(data1[,10:27]),ylab = "count",las=2,ylim = c(0,250))
  text(x = plt, y = colSums(data1[,10:27]), label = colSums(data1[,10:27]), pos = 3, cex = 0.8, col = "black")
})
  
  output$plot2 <- renderPlot({
    newmap <- getMap(resolution = "low")
    plot(newmap, xlim = c(-110, -100), ylim = c(10, 50), asp = 1)
    points(description$long, description$lat, col = "red", cex = 0.5)
   
  }) 
  
  e <- data.frame(matrix(unlist(final_table$state), nrow=1911, byrow=T),stringsAsFactors=FALSE)
  
  state_count<-table(e)
  state_count<-as.data.frame(state_count)
  output$plot3 <- renderPlot({
     
    ggplot(state_count, aes(x = state_count$e, y = state_count$Freq)) + geom_bar(stat="identity") + 
      theme(axis.text.x = element_text(angle = 60, vjust = )) + labs(y = "Frequency", x= "States")
    
    
    
  })
  
  
  output$plot4 <- renderPlot({
    fLength <- function(str1, pat){
      lengths(regmatches(str1, gregexpr(paste0("\\b", pat), str1, ignore.case = TRUE)))
    }
  sum(fLength(final_table$position, "developer"))
  sum(fLength(final_table$position, "engineer"))
  sum(fLength(final_table$position, "analyst"))
  sum(fLength(final_table$position, "intern"))
  positionf<- c(750,378,82,128)
  positionm<- matrix(positionf,nrow=4,byrow = TRUE,dimnames=list(c("developer","engineer","analyst","intern"),row.names="freq"))
  positionm<-as.data.frame(as.table(positionm))
  ggplot(positionm, aes(x = as.character(positionm$Var1), y = positionm$Freq)) + geom_bar(stat="identity",width = 0.2) + 
    theme(axis.text.x = element_text(angle = 60, vjust = )) + labs(y = "Frequency", x= "Positions")})
  
  
}


shinyApp(ui = ui, server = server)