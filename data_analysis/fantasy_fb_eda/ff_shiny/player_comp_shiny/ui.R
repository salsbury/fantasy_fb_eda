shinyUI(fluidPage(
  includeCSS("styles.css"),
  titlePanel(h3("Comparing Fantasy Football Statistics for Two NFL Players")),
  sidebarLayout(
    sidebarPanel(
      uiOutput("choose_pos"),
      textInput("player1", "Input player name with position above:", value = "Tom Brady"),
      textInput("player2", "Input player name with position above:", value = "Peyton Manning"),
      uiOutput("choose_var"),
      helpText("Note: DNP represents either games not played or games where overall Fantasy Points were ranked less than 50th overall for that position.")
    ),
    mainPanel(plotOutput("plot")))
))