library(shinydashboard)

home_tab_body = function() {
  fluidPage(style = "font-size: 2rem; text-align: justify;",
    h1("What do we do?"),
    p("We partnered with Madison area NGOs and local government stakeholders to learn more about environmental issues facing Black and Latinx communities in Dane County. Our team aims to help different stakeholders (e.g., city and county government agencies from the state of Wisconsin that work on sustainability issues, and local NGOs) understand the needs, perceptions, and concerns from Black and Latinx communities regarding the urgent environmental issues facing their communities."),
    tags$br(),
    h1("How to use this site?"),
    tags$ol(
      tags$li("Learn about the surveys to decide what information to review"),
      tags$li("Navigate to the Reporting Tool"),
      tags$li("Select a survey"),
      tags$li("Select a question"),
      tags$li("Select a geography (state, county, census tract, or zip code)"),
      tags$li("Run the report and learn from the communities"),
    )
    
  )
}

description_tab_body = function() {
  fluidPage(style = "font-size: 2rem; text-align: justify;",
    h1("Description"),
    p("Our team aims to help different stakeholders (e.g., city and county government agencies from the state of Wisconsin that work on sustainability issues, local NGOs) understand the needs, perceptions, and concerns from Black and Latinx communities regarding the urgent environmental issues facing their communities."),
    p("Since summer 2022, our team has conducted three in-person community forums, as well as created a digital engagement", 
      tags$a("Community Climate Solutions (CCS)", href="https://ccs.mysocialpinpoint.com/", target="_blank"), 
      "to listen to community suggestions. Our variety of engagement methods have attracted over 300 members from the Black and Latinx communities to share their opinions."),
    p("This Knowledge Map (this site) has two goals - visualize community responses to environmental surveys and display the representativeness of responses. This understanding will help inform local environmental policymaking and communication, and empower local communities with the capacity to discuss environmental justice solutions."),
    tags$br(),
    p("Learn more about our team here: About Us"),
    tags$br(),
    h1("Application and Code"),
    # TODO: Add this link to "Mad Collab": https://github.com/madcollab-jrg/ccs-knowledge-map
    p("The app was developed using RShiny, the code is open source and available on GitHub -", 
      tags$a("Mad Collab.", href="https://github.com/madcollab-jrg/ccs-knowledge-map", target="_blank"))
  )
}

method_tab_body = function() {
  fluidPage(style = "font-size: 1.8rem; text-align: justify;",
            h1("SURVEYS"),
            h2("Digital Surveys"),
            p(strong("CCS Urban Heat Survey:"), "The data source of the CCS air quality survey comes from our digital engagement website, the section on “Local Government And Me”, under the “Urban Heat Island” sub-section. In step 2 of this engagement, we asked a series of close-ended and open-ended survey questions to understand how our participants in Dane county are currently experiencing extreme heat during the summer, what actions they have taken, and what resources they hope to obtain during extreme heat to help them. [Download Questions]"),
            
            p(strong("CCS Urban Heat Social Map:"), "The data source of the CCS Urban Heat Social Map comes from our digital engagement website, the section on “Local Government And Me” and the “Urban Heat Island” sub-section. In step 3 of this engagement, we asked our participants to put a pin on their neighborhood map to annotate the location where they feel hot and tell us how this has impacted their health. [Download Questions]"),
            
            p(strong("CCS Tree Canopy Survey:"), "The survey is designed to gauge community opinions on various aspects of trees within their neighborhood, including their benefits, agreement with certain statements, and observations of tree-related damage to infrastructure. It also explores individual preferences and values concerning trees, such as desired types of trees to be planted, areas in need of more trees, tree-related goals, factors affecting personal tree planting decisions, and willingness to participate in their city’s tree planting efforts. [Download Questions]"),
            
            p(strong("CCS Tree Canopy Social Map:"), "The data source of the CCS Tree Canopy Social Map comes from our digital engagement website, the section on “Local Government And Me”, the “Tree Canopy” sub-section. In step 3 of this engagement, we asked our participants to put a pin on their neighborhood map to annotate the location where they hope to see more trees and the types of trees they want. [Download Questions]"),
            
            p(strong("CCS Air Quality Survey:"), "The survey focuses on understanding community perceptions and experiences related to air quality in their neighborhood, including specific problems, health impacts, pollution sources, and the effect on vulnerable populations. It also seeks input on potential solutions, such as the placement of air quality monitoring sensors, personal efforts to improve air quality, and preferred methods of communication for air quality alerts and participation in decision-making processes. [Download Questions]"),
            
            p(strong("CCS Air Quality Social Map:"), "We asked participants to mark on a map where air quality issues exist in their community. [Download Questions]"),
            
            p(strong("CCS Environmental Justice Share Ideas:"), "The data source of the CCS Environmental Justice Share Ideas comes from our digital engagement website, the section on “Environmental Justice”, under the “Share Your Ideas” sub-section. We asked participants to post comments about their perspectives of what environmental issues are urgent for them to be addressed, and their perspectives of what environmental justice means to them. [Download Questions]"),
            
            p(strong("CCS Environmental Justice Survey:"), "This survey seeks to understand public perception and opinions related to environmental justice and its impact on health and policy within the United States. It includes questions to define the term 'environmental justice,' identify significant issues, rate agreement with various statements, rank the effectiveness of actions, explore the role of new technology in slowing climate change, and gather views on communal efforts and individual responsibilities to address environmental injustice. [Download Questions]"), 
            
            h2("In-Person Surveys"),
            p(strong("In-Person Energy Saving Survey:"), "The data source of the In-Person Energy Saving Survey comes from our in-person community forums that were held in October 2022, where we invited about 100 members from Black and Latinx communities to share their opinions about energy saving at home practices. "),
            p(strong("In-Person Extreme Heat & Health Survey:"), "The data source of the In-Person Energy Saving Survey comes from our in-person community forums that were held in October 2022, where we invited about 100 members from Black and Latinx communities to share their opinions about their experiences with extreme heat during the summer and how they protect themselves, as well as what resources they hope to see in their neighborhoods. [Download Questions]"),
            p(strong("In-Person Tree Canopy Survey:"), "The data source of the In-Person Energy Saving Survey comes from our in-person community forums that were held in October 2022, where we invited about 100 members from Black and Latinx communities to share their opinions about their needs for trees, the existing tree programs they participate, and what resources regarding green spaces they hope to see.  [Download Questions]"),
            p(strong("In-Person Urban Heat Map Survey:"), "The data source of the In-Person Urban Heat Map comes from our in-person community forums that were held in March 2023, where we invited about 40 members from Latinx communities to share their feedback about the existing Urban Heat Map the City of Madison is building. [Download Questions]"),
            p(strong("In-Person Air Quality Survey:"), "The data source of the In-Person Urban Heat Map comes from our in-person community forums that were held in March 2023, where we invited about 40 members from Latinx communities to share their feedback about the existing Air Quality Sensor project the City of Madison is working on. [Download Questions]"),
            
            h2("Census Data"), 
            p(strong("2019 American Community Survey (ACS):"), "The", 
              tags$a("ACS", href="https://www.census.gov/programs-surveys/acs", target="_blank"), 
              "is an ongoing survey conducted by the U.S. Census Bureau that samples a small percentage of the population each year. It provides detailed social, economic, housing, and demographic information about the country's communities. We used the", 
              tags$a("US Census API", href="https://www.census.gov/data/developers/guidance/api-user-guide.html", target="_blank"), 
              "and to extract demographic counts (see:", 
              tags$a("Data Dictionary", href="https://www.socialexplorer.com/data/ACS2019_5yr/metadata/?ds=ACS19_5yr", target="_blank"), 
              ") in four geographies (state, county, census tract, and zip code). Many thanks to the developers of the", 
              tags$a("Opioid Environment Toolkit", href="https://geodacenter.github.io/opioid-environment-toolkit/index.html", target="_blank"),
              "which we used to model our data extraction."),
            
            tags$br(),
            h1("ANALYSIS APPROACH"),
            p("We have collected three types of data from our engagement. The first type is close-ended survey questions that use multiple choice format or ask participants to rank the options. To analyze this first type of data, we adopted the method of frequency analysis. The second type is open-ended survey questions or written notes from our participants to answer pre-filled Q&A sheets. To analyze this text data, we used various computational tools from NLP as well as human validation to uncover the themes and sentiments from communities. The third type of data is a geo-pinned annotated dataset. To analyze this data, we simply represent the responses on a map of the geography."), 
            
            tags$br(),
            h1("COMPUTING REPRESENTATIVENESS"),
            p("In addition to presented results from the surveys, we measure the representativeness of responses as they align with the actual demographics of a given population within specific geographic regions such as state, census tract, zip code, or county."),
            p("Representativeness is measured using conditional probabilities, comparing the surveyed population and the actual demographic counts obtained in the American Community Survey (ACS) or Decennial Census. This approach allows us to assess the likelihood of certain outcomes based on given conditions, enabling us to uncover any disparities or biases in the survey data. More details about the approach can be found here:"),
            tags$blockquote(
              'Qi, Miao, Owen Cahan, Morgan A. Foreman, Daniel M. Gruen, Amar K. Das, and Kristin P. Bennett.',
              tags$a('"Quantifying representativeness in randomized clinical trials using machine learning fairness metrics."', href='https://watermark.silverchair.com/ooab077.pdf?token=AQECAHi208BE49Ooan9kkhW_Ercy7Dm3ZL_9Cf3qfKAc485ysgAAA2wwggNoBgkqhkiG9w0BBwagggNZMIIDVQIBADCCA04GCSqGSIb3DQEHATAeBglghkgBZQMEAS4wEQQMfPp8lmC_bhLZ9Wm0AgEQgIIDH5W2562RW-rW3dbiurzZJROQ3CxguFPCk9peDUk4-qaXIJxHr27bMQj_j0HxPvYCkrxb6ysRTr9bSIJcX45da-tQbNu_OwCZcZYQ4R8bqDF4aVbpOItd6cdVMXaqCZnxchugyV3z06GluOpoYVDkr7M2uzTrj843JhIdwR0dEXhrzkLSKwfXOM3tlQtE5vC8sIFr18lr1mrSDPm0SePEIgsK2VKJZysZ4AUKTwz0WbsV3uNyS28aRfwk6F8sG66CnUktp0rfRNBTCglOy2PEmulIT6Ts4dPvatzLMLgELh20KIJgwgHuHBjLPg4vUn_dql2QM53JW5qvRq5RPbnTOt0gne7OFqQ_CSRWpcljr377pr9XAoliKEsuUBpVtDvDCXsclBWkA8YOKvL45iqynA1oXhElinWyPRrVWEztRlqxXEGwQ2-4eavQ9z4CH9ZHUxXbEHniTn8sbp-NJBgGQ6stQCOgbwIFriRx-xMmRnA7PuT8Cy5Zr5z1RMQ5vGgEh9U0stqc0pSNcuTVRUobUOj4SaS1X87YTd02qRIQaT1VZ3SNZQr4pogCcSQkBQioEOnHoMHI7hrep5O345Udmi8LHwfTLWNPVUki1AH1NNkZERF11S2HHU3odDrNsLDqYW3p96OTC3Dr_29HJZXm1blgV55SyrTkBFZqg6sPCIe98BqXPbpQRtJhwN6cb44RqMjHEmNaqJzJ8uBMaBWPigoyNLLB15jiIUnEl2orrCrcMmi_m6lr1cJFl3J3NenaJwiklZDyrdBngxIXTdB0_n6s_gGSnrdQx3dzNkXFYDbMDGKTW9TPDJvCgI2sHVdAY2dtfR4KGhWdkW7lZtEJ28N584Ah01w4o3mtZnh7FYJ04qRM9N8D973s64cPIhjEnde-UVW_8C2MhZ3UM8PhbbzVHHlLs2b4rV6Bdtq7kT2pDE_ueTgvLXvFbRzjsuZ46Pbdd_mngXh30lDyvUG-TjWfT1AExfTv7YBPmx6EeEcKG8oUzaC4Bx-_N2OuWTJa9l20axRkqqbHLNRDUnl75MS1Hs2DDBgPqdyvttSRmD4', target="_blank"),
              tags$i('JAMIA open'), 
              '4, no. 3 (2021): ooab077.'), 
            p("This approach ensures the survey's findings are not just insightful but also statistically reflective of the wider community, fostering a more comprehensive and equitable understanding of the population's views and preferences. Our approach enhances the reliability and validity of survey results, supporting better decision-making and policy development tailored to the real needs and characteristics of the community."),
  )
}

team_tab_body = function() {
  fluidPage(style = "font-size: 2rem; text-align: justify;",
    h2("Faculty"),
    tags$ul(
      tags$li(
        tags$a("Dr Corey Jackson,", href="https://coreybjackson.com", target="_blank"), 
        "School of Information, University of Wisconsin-Madison;"),
      tags$li(
        tags$a("Dr Kaiping Chen,", href="http://www.kaipingchen.com", target="_blank"), 
        "Department of Life Sciences Communication, University of Wisconsin-Madison")
    ),
    tags$br(),
    h2("UW-Madison Students"),
    p(strong("Developers")), 
    tags$ul(
      tags$li("Christian Varner, PhD student in Statistics"),
      tags$li("Niharika Chunduru, Master’s student in Statistics & Data Science"),
    ),
    p(strong("Researchers")),
    tags$ul(
      tags$li("Corey Black, Master’s student in the School of Information"),
      tags$li("Vincent Kong, Undergraduate student in Computer Science (Graduated 2023)"),
      tags$li("Vaathsalya Karpe, Undergraduate student (Graduated 2023)"),
      tags$li("Kyla Smith, Master’s student in Energy Analysis and Policy"),
    ),
    tags$br(),
    h2("Partners"),
    tags$ul(
      tags$li("Wisconsin EcoLatinos"),
      tags$li("Urban Triage"),
      tags$li("Dane County Office of Energy & Climate Change"),
      tags$li("City of Madison"),
    ),
    tags$br(),
    h2("Contact"),
    tags$ul(
      tags$li(tags$a("cbjackson2@wisc.edu", href="mailto:cbjackson2@wisc.edu")),
      tags$li(tags$a("kchen67@wisc.edu", href="mailto:kchen67@wisc.edu"))
    )
  )
}












