shinyUI(fluidPage(
  titlePanel("Boxplot of FFPts in 2014 for a Specific Player in Each Division"),
  sidebarLayout(
    sidebarPanel(
      textInput("player", "Input the Player Name (FirstName LastName):", value = "Peyton Manning")),
    mainPanel(plotOutput("plot"))
    )))