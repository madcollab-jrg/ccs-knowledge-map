library(shiny)
library(htmltools)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  tags$style(HTML("
  .navbar-brand {
    color: red !important;
    font-weight: bold;
  }
  ")),

  navbarPage("Community Climate Solutions - Knowledge Map",
  tabPanel("Page 1"),
  tabPanel("Page 2"),
  tabPanel("Page 3"),
  navbarMenu("More",
             tabPanel("Information"),
             tabPanel("About"))
  ),
  tags$div(
    style = "border: 2px solid black; padding: 10px; margin: 0px; width: 65%; float: left;",
    tags$p(
      style = "font-weight: bold; font-size: 25px; margin-bottom: 10px;",
      "Our Project"
    ),
    tags$p(
      style = "font-size: 20px;",
      tags$u("Wisconsin Ecolatinos"),
      "and",
      tags$u("Urban Triage"),
      ", parternered with the University of Wisconsin ",
      tags$u("Life Sciences Communication Department"),
      "and assistant professor ",
      tags$u("Kaiping Chen"),
      "for a new project aimed atinvolving communities of color in the decision-making process fordeveloping new environmental conservation solutions. The projectbrings the community, city and official offices together to listen tothe needs of communities of color and determine how it can createsolutions based on those needs."
    ),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    tags$p(
      style = "font-weight: bold; font-size: 25px; margin-bottom: 10px;",
      "Project Objectives"
    ),
    tags$p(
      style = "font-size: 20px;",
     "We have goals for this project:"
    ),
    tags$ul(
      style = "font-size: 20px;",
      tags$li("Empower these community members the resources andawareness to take action to enhance their living environmen."),
      tags$li("Create local government responsiveness to community voices!")
    )
  ),
  tags$div(
    style = "border: 2px solid black; padding: 10px; margin: 0px; width: 30%; float: right; text-align:center;",
    tags$p(
      style = "font-weight: bold; font-size: 20px; margin-bottom: 10px;",
      "Our Team"
    ),
    "The team for this project includes UW-Madison professors, students, LocalGovernment members
     and NGO representatives",
    br(),
    br(),
    tags$a(
      style = "font-weight: bold; background-color: red; color: white; border: none; text-align: center; display: inline-block; font-size: 16px; border-radius: 10px;",
      class = "btn btn-primary", # add Bootstrap button classes
      "Learn more about our team" # button text
    )
  ),
  
  tags$div(
    style = "border: 2px solid black; padding: 10px; margin-top: 10px; width: 30%; float: right; text-align:center;",
    tags$p(
      style = "font-weight: bold; font-size: 20px; margin-bottom: 10px;",
      "Our Approach"
    ),
    "For our findings, we used interviewing,surveying, knowledge mapping, usabilitytesting, and more to result in a well-rounded analysis.",
    br(),
    br(),
    tags$a(
      style = "font-weight: bold; background-color: red; color: white; border: none; text-align: center; display: inline-block; font-size: 14px; border-radius: 10px;",
      class = "btn btn-primary", # add Bootstrap button classes
      "Learn more about our approach" # button text
    )
  ),
  
  tags$div(
    style = "border: 2px solid black; padding: 10px; margin-top: 10px; width: 30%; float: right; text-align:center;",
    tags$p(
      style = "font-weight: bold; font-size: 20px; margin-bottom: 10px;",
      "Feedback"
    ),
    "Fill out a survey giving us your feedbackon this knowledge map",
    br(),
    br(),
    tags$a(
      style = "font-weight: bold; background-color: red; color: white; border: none; text-align: center; display: inline-block; font-size: 14px; border-radius: 10px;",
      class = "btn btn-primary", # add Bootstrap button classes
      "Fill out our survey here!" # button text
    )
  ),
  
  tags$div(
    style = "border: 2px solid black; padding: 10px; margin-top: 10px; width: 30%; float: right; text-align:center;",
    tags$p(
      style = "font-weight: bold; font-size: 20px; margin-bottom: 10px;",
      "Contact"
    ),
    "Have any questions or want to getinvolved? Contact the following peoplebelow:",
    tags$p(
      style = "font-weight: bold; font-size: 13px; margin-top: 10px",
      "Corey Jackson, Assistant Professor"
    ),
    "cbjackson2@wisc.edu",
    tags$p(
      style = "font-weight: bold; font-size: 13px; margin-top: 10px",
      "Kaiping Chen, Assistant Professor"
    ),
    "kchen67@wisc.edu",
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
