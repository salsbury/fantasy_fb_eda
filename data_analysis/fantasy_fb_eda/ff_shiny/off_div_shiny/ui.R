shinyUI(fluidPage(
  titlePanel("Boxplots of Fantasy Points Against Each Division"),
  sidebarLayout(
    sidebarPanel(
      selectInput("team", "Select the NFL Team:",
                  choices = c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI",
                              "CLE", "DAL", "DEN", "DET", "GB", "HOU",
                              "IND", "JAC", "KC", "MIA", "MIN", "NE", "NO",
                              "NYG", "NYJ", "OAK", "PHI", "PIT", "SD", "SEA",
                              "SF", "STL", "TB", "TEN", "WAS"),
                  selected = "ARI")),
    mainPanel(plotOutput("plot")))
))