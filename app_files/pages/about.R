library(bs4Dash)
library(shinydashboard)

# About Page
about_tab_body <- function() {
  fluidRow(
    class = "container-row",
    style = "margin-top: 24px;",
    column(
      8,
      h1("About", class = "page-title"),
      hr(style = "border-top: 1px solid #A8AAAD;"),
      h2("About this tool", class = "page-subtitle"),
      p(
        "The app was developed using RShiny, the code is
      open source and available on GitHub -",
        tags$a("Mad Collab.",
          href = "https://github.com/madcollab-jrg/ccs-knowledge-map",
          target = "_blank"
        ),
        class = "page-para"
      ),
      h2("Faculty", class = "page-subtitle"),
      tags$ul(
        tags$li(
          tags$a("Dr Corey Jackson,",
            href = "https://coreybjackson.com",
            target = "_blank"
          ),
          "School of Information, University of Wisconsin-Madison;"
        ),
        tags$li(
          tags$a("Dr Kaiping Chen,",
            href = "http://www.kaipingchen.com",
            target = "_blank"
          ),
          "Department of Life Sciences Communication,
          University of Wisconsin-Madison"
        ),
        class = "page-para"
      ),
      h2("UW-Madison Students", class = "page-subtitle"),
      p(strong("Developers")),
      tags$ul(
        tags$li("Christian Varner, PhD student in Statistics"),
        tags$li("Niharika Chunduru, Master’s student in
        Statistics & Data Science"),
        tags$li("Shelcia David, Master’s student in
        Informations"),
        class = "page-para"
      ),
      p(strong("Researchers")),
      tags$ul(
        tags$li("Corey Black, Master’s student in the School of Information"),
        tags$li("Vincent Kong, Undergraduate student in
        Computer Science (Graduated 2023)"),
        tags$li("Vaathsalya Karpe, Undergraduate student (Graduated 2023)"),
        tags$li("Kyla Smith, Master’s student in Energy Analysis and Policy"),
        class = "page-para"
      ),
      h2("Partners", class = "page-subtitle"),
      tags$ul(
        tags$li("Wisconsin EcoLatinos"),
        tags$li("Urban Triage"),
        tags$li("Dane County Office of Energy & Climate Change"),
        tags$li("City of Madison"),
        class = "page-para"
      ),
      h2("About this project", class = "page-subtitle"),
      p("Our team aims to help different stakeholders
      (e.g., city and county government agencies from the
      state of Wisconsin that work on sustainability issues,
      local NGOs) understand the needs, perceptions, and
      concerns from Black and Latinx communities regarding the
      urgent environmental issues facing their communities.",
        class = "page-para"
      ),
      p(
        "Since summer 2022, our team has conducted three in-person
        community forums, as well as created a digital engagement",
        tags$a("Community Climate Solutions (CCS)",
          href = "https://ccs.mysocialpinpoint.com/", target = "_blank"
        ),
        "to listen to community suggestions. Our variety of
        engagement methods have attracted over 300 members from
        the Black and Latinx communities to share their opinions.",
        class = "page-para"
      ),
      p("This Knowledge Map (this site) has two goals - visualize
      community responses to environmental surveys and display
      the representativeness of responses. This understanding
      will help inform local environmental policymaking and communication,
      and empower local communities with the capacity to discuss environmental
      justice solutions.", class = "page-para"),
      h2("Referencing this tool", class = "page-subtitle"),
      p("Organisations may cite material included within the
      ScotPHO profiles tool subject to the following conditions:",
        class = "page-para"
      ),
      tags$ul(
        tags$li("Quote the source as 'Knowledge Map'"),
        tags$li("Include the following URL to the tool [URL]"),
        class = "page-para"
      ),
      blockQuote(
        "CCS Knowledge Map",
        "Developed using: RShiny",
        tags$a('"Open Source Code: Available on GitHub - Mad Collab"',
          href = "https://github.com/madcollab-jrg/ccs-knowledge-map",
          target = "_blank"
        ),
        color = "primary"
      ),
      h2("Contact Us", class = "page-subtitle"),
      p("If you have any trouble accessing any information on this site or
      have any further questions or feedback relating to the data or the tool,
      then please contact us at: [email address] and we will be happy to help.
      If you would like help developing a survey to be used in the Knowledge
      Map, please contact us at: [email address].",
        class = "page-para"
      ),
    ),
    column(
      4,
      img(
        src = "assets/about-bg.png", width = "100%", height = 300,
        style = "object-fit: cover"
      )
    )
  )
}
