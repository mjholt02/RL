
library(shiny)


ui <- fluidPage(

  div(id="currentui",
  
  fluidRow(
    column(4,selectInput("ranking", label = "Ranking", 
                         choices = list("Unranked"=0, 
                                        "Bronze 1" = "Bronze 1", "Bronze 2" = 2, "Bronze 3" = 3,
                                        "Silver 1" = 4, "Silver 2" = 5, "Silver 3" = 6,
                                        "Gold 1" = 7, "Gold 2" = 8, "Gold 3" = 9,
                                        "Platinum 1" = 10, "Platinum 2" = 11, "Platinum 3" = 12))),
    column(4,textInput("searchTime", label = "Searching:", value = "Enter time...")),
    column(4,actionButton("UpdateSearchTime", label = "Start Search"))
  ),
  fluidRow(    
    column(4,radioButtons("gameType", label = "Game type", choices = list("2's" = "2", "3's" = "3", "Other" = "Other"),selected = "3",inline=TRUE)),
    column(4,textInput("startTime", label = "Game start:", value = "Enter time...")),
    column(4,actionButton("UpdateStartTime", label = "Game Started"))
  ),
  fluidRow(
    column(4,radioButtons("teamColor", label = "Team Color:", choices = list("Blue" = "Blue", "Orange" = "Orange"),selected = "Blue",inline=TRUE)),
    column(4,textInput("endTime",   label = "Game End:", value = "Enter time...")),
    column(4,actionButton("UpdateEndTime", label = "Game Ended"))
  ),
  
  hr(),

  
tags$style(type='text/css', "#UpdateSearchTime { width:100%; margin-top: 25px;}"),
tags$style(type='text/css', "#UpdateStartTime { width:100%; margin-top: 25px;}"),
tags$style(type='text/css', "#UpdateEndTime { width:100%; margin-top: 25px;}"),



fluidRow(
  column(4,
         numericInput("myScore", label = "My Score:", value = ""),
         numericInput("t1Score", label = "Teammate 1 Score:", value = ""),
         numericInput("t2Score", label = "Teammate 2 Score:", value = "")

  ),

  column(4,
         numericInput("op1Score", label = "Opponent 1 Score:", value = ""),
         numericInput("op2Score", label = "Opponent 2 Score:", value = ""),
         numericInput("op3Score", label = "Opponent 3 Score:", value = "")

  ),
  
  column(4,
         numericInput("blueGoals", label = "Blue Goals:", value = 0),
         numericInput("orangeGoals", label = "Orange Goals:", value = 0),
         actionButton("UpdateBlueScore", label = "Blue Goal"),
         actionButton("UpdateOrangeScore", label = "Orange Goal")
  ),
  tags$style(type='text/css', "#UpdateBlueScore { width:49%; margin-top: 25px;background-color: blue;color: white;}"),
  tags$style(type='text/css', "#UpdateOrangeScore { width:49%; margin-top: 25px;background-color: orange;color: white;}")

  
  
  
),
hr(),
fluidRow(
  column(4,
         numericInput("myGoals", label = "My Goals:", value = "", min=0),
         numericInput("myAssists", label = "My Assists:", value = ""),
         numericInput("mySaves", label = "My Saves:", value = ""),
         numericInput("myShots", label = "My Shots:", value = "")
         
  ),
  column(4,
    checkboxGroupInput("checkGroup", label = "Misc.", 
                       choices = list("MVP" = "MVP","Overtime" = "OT" ,"My teammate quit" = "teamQuit", "Opp. teammate quit" = "oppQuit", 
                                      "Opp. Forfeit" = "oppForfeit", "We forfeit" = "teamForfeit")),
    tags$style(type='text/css', "#checkGroup { font-size:20px;}")
    ),
  
  column(4,
         actionButton("submittocsv", label = "Submit"),
         tags$style(type='text/css', "#submittocsv { width:49%; margin-top: 25px;background-color: red;color: white;}")
         )
  )
))



########SERVER##############
getTime <- function(){
  format(Sys.time(), tz="America/Chicago",usetz=FALSE, format="%H:%M:%S")
  }

server <- function(input, output, session) {
  observeEvent(input$UpdateSearchTime, {
    updateTextInput(session, "searchTime", value = getTime())
  })
    observeEvent(input$UpdateStartTime, {
    updateTextInput(session, "startTime", value = getTime())
  })
  observeEvent(input$UpdateEndTime, {
    updateTextInput(session, "endTime", value = getTime())
  })
  
  ###Goals###
  observeEvent(input$UpdateBlueScore, {
    updateTextInput(session, "blueGoals", value = input$UpdateBlueScore[1])
  })
  observeEvent(input$UpdateOrangeScore, {
    updateTextInput(session, "orangeGoals", value = input$UpdateOrangeScore[1])
  })
  
  observeEvent(input$submittocsv, { record_activity()}) 
    
  
  record_activity <- function(){

  mvp <- ifelse("MVP" %in% input$checkGroup,1,0)  
  ot <- ifelse("OT" %in% input$checkGroup,1,0)  
  
  teamQuit <- ifelse("teamQuit" %in% input$checkGroup,1,0)  
  oppQuit <- ifelse("oppQuit" %in% input$checkGroup,1,0)  
  
  oppForfeit <- ifelse("oppForfeit" %in% input$checkGroup,1,0)  
  teamForfeit <- ifelse("teamForfeit" %in% input$checkGroup,1,0)  
  
  
  
  varSet1 <- paste(input$searchTime,input$startTime,input$endTime, sep="','")
  varSet2 <- paste(input$ranking, input$gameType, input$teamColor, sep="','")
  varSet3 <- paste(input$myScore,input$t1Score,input$t2Score, input$op1Score, input$op2Score, input$op3Score, input$blueGoals, input$orangeGoals, sep="','")
  varSet4 <- paste(input$myGoals, input$myAssists, input$mySaves, input$myShots, sep="','")
  varSet5 <- paste(mvp, ot, teamQuit, oppQuit, oppForfeit, teamForfeit, sep="','")
  
  valueString <- gsub("NA", "0",paste(Sys.Date(),varSet1,varSet2,varSet3,varSet4,varSet5, sep="','"))
  
  print(valueString)
  write(valueString,file="./test.csv",append=TRUE)

insertUI(selector = "#currentui", where = "beforeBegin",
         thank_you <- 
fluidPage(
  h3("Overall Summary of games played"),
  h5("    Game summary:")

  
)#fluidpage 
)#insertUI
removeUI(selector = "#currentui")
}


    
    
    
  
  
  
    
    }

shinyApp(ui = ui, server = server)

