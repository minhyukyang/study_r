# https://rstudio.github.io/shinydashboard/get_started.html


# 1. Installation ---------------------------------------------------------
# shinydashboard requires Shiny 0.11 or above. To install, run:
  
install.packages("shinydashboard")


# 2. Basics ---------------------------------------------------------------
# A dashboard has three parts: a header, a sidebar, and a body. Here’s the most minimal possible UI for a dashboard page.

## ui.R ##
library(shinydashboard)

dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)

# You can quickly view it at the R console by using the shinyApp() function. (You can also use this code as a single-file app).

## app.R ##
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)

server <- function(input, output) { }

shinyApp(ui, server)

# Blank dashboard
# Obviously, this dashboard isn’t very useful. We’ll need to add components that actually do something. 
# In the body we can add boxes that have content.

