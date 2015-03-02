shinyUI(fluidPage(
  titlePanel("Bar Plot of Total Fantasy Points for Each Team"),
  sidebarLayout(
    sidebarPanel(
      selectInput("off_def", "Select either Offense or Defense:",
                  choices = c("Offense", "Defense")),
      selectInput("team", "Select the NFL Team:",
                  choices = c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI",
                              "CLE", "DAL", "DEN", "DET", "GB", "HOU",
                              "IND", "JAC", "KC", "MIA", "MIN", "NE", "NO",
                              "NYG", "NYJ", "OAK", "PHI", "PIT", "SD", "SEA",
                              "SF", "STL", "TB", "TEN", "WAS"),
                  selected = "ARI")),
    mainPanel(plotOutput("plot")))
  ))