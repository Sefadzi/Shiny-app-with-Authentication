library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(sodium)
library(dplyr)
library(ggplot2)


loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%;margin: 0 auto;
                 padding: 20px;",
                 wellPanel(
                   tags$h2("LOG IN", class= "text-center", style = "padding-top: 0; color:  #333;
                           font-weight:600;"),
                   textInput("userName", placeholder = "Username", label = tagList(icon("user"),"Username")),
                   textInput("passwd", placeholder = "Password", label = tagList(icon("unlock-alt"),"Password")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "SIGN IN", style = "color: white;background-color: #3c8dbc;
                                  padding: 10px 15px ; width: 150px; cursor: pointer;
                                  font-size: 18px; fomt-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Oops! Incorrect username or password!",
                                  style = "color: red; font-weight:600;
                                  padding-top: 5px; font-size: 16px;",
                                  class = "text-center"))
                     ),
                     br(),
                     br(),
                     tags$code("Username: myuser Password: mypass"),
                     br(),
                     tags$code("Username: myuser1 Password: mypass1")
                   )
                 )
                 
)

credentials = data.frame(
  username_id = c("myuser","myuser1"),
  passod = sapply(c("mypass","mypass1"), password_store),
  permission = c("basic","advanced"),
  stringsAsFactors = F
)


data = read.csv("9.1 allBaseballData.csv.csv")

nameChoices = unique(data$name)
teamChoices = unique(data$franchName)
#statsChoices = unique(colnames(data))
statsChoices <- c("G", "AB", "R", "H", "Doubles", "Triples", "HR", "RBI", "BB", "SO")
yearChoices = unique(as.character(data$yearID))

header <- dashboardHeader( title = "Simple Dashboard", uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui<-dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
  
  login = FALSE
  USER <- reactiveValues(login = login)
  
  
  agg = reactive({
    dat = data %>%
      filter(name == input$selectPlayer)%>%
      summarise_if(is.integer, sum, na.rm =TRUE)%>%
      mutate(name =  input$selectPlayer)%>%
      select(-(yearID))
    return(dat)
  })
  
  filtData = reactive({
    filt_data = data[data$name == input$selectPlayer,]
    return(filt_data)
  })
  
  
  
  filter_team_yr = reactive({
    df <- data %>%
      group_by(input$selectTeam, yearID)%>%
      summarise_if(is.integer, sum, na.rm=TRUE)
    names(df)[1] = "Franchise Team"
    return(df)
  })
  
  yearData = reactive({
    df = data[data$yearID == input$selectYear,]
    final <- df[order(df[input$selectStats], decreasing = T), ]
    final2 <- final[1:10,]
    
    final2$name <- factor(final2$name,levels = unique(final2$name))
    
    return(final2)
  })
  
  
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(length(which(credentials$username_id==Username))==1) { 
            pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
            pasverify <- password_verify(pasmatch, Password)
            if(pasverify) {
              USER$login <- TRUE
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  })
  
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fa fa-sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })
  
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ){ 
      sidebarMenu(
        menuItem("Main Page", tabName = "dashboard", icon = icon("dashboard")),
        menuItem(text = "data", tabName = "Player Data",
                 menuSubItem(text = "Data", tabName = "data", icon = icon("angle-double-right")),
                 menuSubItem(text = "Plots", tabName = "plot", icon = icon("angle-double-right"))),
        menuItem(text = "Data per Team / Year", tabName = "dataperteam"),
        menuItem(text = "Yearly leaders", tabName = "yearlyleaders"),
        selectInput(inputId = "selectPlayer", label = "Select a player", choices = nameChoices),
        selectInput(inputId = "selectStats", label = "Select which stats to display", choices = statsChoices),
        selectInput(inputId = "selectTeam", label = "Select which team to view", choices = teamChoices),
        selectInput(inputId = "selectYear", label = "Select which year to view", choices = yearChoices)
      )
    }
  })
  
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      tabItems(
        tabItem(tabName ="dashboard", class = "active",
                fluidRow(
                  box(width = 12, dataTableOutput('results')),
                )),
        tabItem(tabName = "data",
                DTOutput("data1"),
                DTOutput("data2")),
        tabItem(tabName = "plot",
                plotOutput("plot1")),
        tabItem(tabName = "dataperteam",
                DTOutput("data3")),
        tabItem(tabName = "yearlyleaders",
                plotOutput("plot2"))
      )
      
    }
    else {
      loginpage
    }
  })
  
  output$data2 <- renderDT(
    datatable(filtData())
  )
  output$data1 <- renderDT(
    datatable(agg())
  )
  
  output$plot1 <- renderPlot({
    ggplot(filtData() ,aes_string(x="yearID", y=input$selectStats)) + geom_col(fill="blue") + ggtitle(paste0(input$selectStats, " Per Year For ", input$selectPlayer))
  })
  
  output$plot2 <- renderPlot({
    ggplot(yearData() ,aes_string(x="name", y=input$selectStats)) + geom_col(fill="blue") + ggtitle(paste0(input$selectStats, " Leaders for the year ", input$selectYear))
  })
  
  output$data3 <- renderDT(
    datatable(filter_team_yr())
  )
  
  output$results <-  DT::renderDataTable({
    datatable(iris, options = list(autoWidth = TRUE,
                                   searching = FALSE))
  })
  
}

runApp(list(ui = ui, server = server), launch.browser = TRUE)
  
  