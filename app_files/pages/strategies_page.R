library(bs4Dash)
library(shinydashboard)

# Strategies Page

strategies_data_tab_body <- function() {
  fluidRow(
    class = "container-row",
    style = "margin-top: 24px;",
    column(
      12,
      h1("Outreach Strategies", class = "page-title"),
      hr(style = "border-top: 1px solid #A8AAAD;"),
    ),
    column(
      5,
      h2("Literature and Empirical", class = "page-subtitle"),
      p("The literature and empirical evidence surrounding climate change
      outreach strategies provide valuable insights into effective approaches
      for engaging diverse demographic groups. Various studies highlight the
      importance of targeted initiatives tailored to specific demographics to
      enhance representation and foster meaningful participation in climate
      discussions.", class = "page-para"),
      p("Empirical research underscores the significance of community-based
      programs, educational interventions, and culturally sensitive outreach
      efforts in overcoming barriers to engagement. By leveraging existing
      community networks, cultural traditions, and communication channels,
      organizations can build trust, address unique concerns, and promote
      sustainable behavior change.", class = "page-para"),
      p("Select a survey on the right to view the description and questions.",
        class = "page-para"
      ),
    ),
    column(
      7,
      accordion(
        id = "accordion2",
        accordionItem(
          title =
            HTML("<span>Age Groups</span><br>
          <span style='font-size: 0.85rem;' class='text-lighter'>
          From the CCS Knowledge Map</span>"),
          subtitle = "Lorem",
          status = "white",
          collapsed = FALSE,
          HTML(paste(
            "<div class='page-para'>",
            "<h4>Older African American rural people</h4>",
            "<h6>Location</h6>",
            "<p>Chapel Hill, North Carolina </p>",
            "<a
            href='https://doi.org/10.1177/0145721708325764'
            target='_blank'
            >
            Burns, D., Soward, A. C. M., Skelly, A. H., Leeman, J.,
            & Carlson, J. (2008). Effective Recruitment and Retention
            Strategies for Older Members of Rural Minorities.
            The Diabetes Educator, 34(6), 1045–1052.
            https://doi.org/10.1177/0145721708325764
              </a>",
            "</div>"
          )),
          HTML(paste(
            "<div class='page-para' style='margin-top: 2rem;'>",
            "<h4>Older adults</h4>",
            "<h6>Location</h6>",
            "<p>Richmond, Virginia</p>",
            "<a
            href='https://doi.org/10.1097/FCH.0000000000000383'
            target='_blank'
            >
            Diallo, A. F., Mackiewicz, M., Sargent, L., Roman,
            Y. M., Slattum, P. W., Waters, L., Bennett, J., Battle,
            K., Zanjani, F., Gendron, T., Winship, J., Ford, G., Falls,
            K., Price, E. T., Parsons, P., & Chung, J. (2024). Cultivating
            Relationships as a Community-Based Recruitment Strategy in
            Transdisciplinary Aging Research: Lessons From an
            Academic-Community Partnership. Family & Community Health,
            47(1), 32. https://doi.org/10.1097/FCH.0000000000000383
              </a>",
            "</div>"
          )),
          HTML(paste(
            "<div class='page-para' style='margin-top: 2rem;'>",
            "<h4>Older-adults </h4>",
            "<h6>Location</h6>",
            "<p>Canada</p>",
            "<p>
            - Photovoice is not as effective because some participants
            find it intimidating <br/>
            - Demographic would prefer sharing through 'real conversation'<br/>
            - Shifted participants to role of co-researchers, allowing
            them more control <br/>
            - Allows nuanced understanding that text does not <br/>
            - Particularly effective for place-based research<br/>
            - Encouraging community engagement
              </p>",
            "<a
            href='https://doi.org/10.1080/10437797.2020.1723763'
            target='_blank'
            >
            Burns, V., Kwan, C., & Walsh, C. A. (2020). Co-producing
            Knowledge Through Documentary Film: A Community-Based
            Participatory Study With Older Adults With Homeless Histories.
            Journal of Social Work Education, 56(sup1), S119–S130.
            https://doi.org/10.1080/10437797.2020.1723763
              </a>",
            "</div>"
          )),
          HTML(paste(
            "<div class='page-para' style='margin-top: 2rem;'>",
            "<h4>Older-adults </h4>",
            "<h6>Location</h6>",
            "<p>Netherlands</p>",
            "<p>
            - Participants are trained to use video as part of research <br/>
            - Enjoyed learning a new skill and collaborative tasks<br/>
            - Shows participants' relation with their environment <br/>
            - Allows participants to become active researchers <br/>
            - Lengthy project
              </p>",
            "<a
            href='https://doi.org/10.3390/ijerph17238977'
            target='_blank'
            >
            von Faber, M., Tavy, Z., & van der Pas, S. (2020).
            Engaging Older People in Age-Friendly Cities through
            Participatory Video Design. International Journal of
            Environmental Research and Public Health, 17(23), 8977.
            https://doi.org/10.3390/ijerph17238977
              </a>",
            "</div>"
          )),
          HTML(paste(
            "<div class='page-para' style='margin-top: 2rem;'>",
            "<h4>Older-adults </h4>",
            "<h6>Location</h6>",
            "<p>Literature review</p>",
            "<a
            href='https://doi.org/10.1111/j.1532-5415.2008.02015.x'
            target='_blank'
            >
            Mody, L., Miller, D. K., McGloin, J. M., Freeman, M.,
            Marcantonio, E. R., Magaziner, J., & Studenski, S. (2008).
            Recruitment and Retention of Older Adults in Aging Research.
            Journal of the American Geriatrics Society, 56(12), 2340–2348.
            https://doi.org/10.1111/j.1532-5415.2008.02015.x
              </a>",
            "</div>"
          )),
          HTML(paste(
            "<div class='page-para' style='margin-top: 2rem;'>",
            "<h4>Youth</h4>",
            "<h6>Location</h6>",
            "<p>Literature review</p>",
            "<p>
            - Visual methods such as digital storytelling and photovoice
            call attention to social problems that may not be visible
            to adults <br/>
            - Allow young people to take over the research process since
            they can tell their stories more easily through images
            than text<br/>
            - It also helps in increasing civic participation of youth.
              </p>",
            "<a
            href='https://doi.org/10.3102/0034654318794134'
            target='_blank'
            >
            Greene, S., Burke, K. J., & McKenna, M. K. (2018).
            A Review of Research Connecting Digital Storytelling,
            Photovoice, and Civic Engagement. Review of Educational
            Research, 88(6), 844–878.
            https://doi.org/10.3102/0034654318794134
              </a>",
            "</div>"
          )),
        ),
        accordionItem(
          title =
            HTML("<span>Income Groups</span><br>
          <span style='font-size: 0.85rem;' class='text-lighter'>
          From the CCS Knowledge Map</span>"),
          status = "white",
          collapsed = TRUE,
          HTML(paste(
            "<div class='page-para'>",
            "<h4>Low Income</h4>",
            "<h6>Location</h6>",
            "<p>Columbus, Ohio</p>",
            "<p>
            - Provide consistent interviewers for each mother to
            maintain trusty relationship <br/>
            - Dedicated phone line with staff available throughout
            business hours<br/>
            - Post cards, thank you notes, newsletters with project logo
            sent out to emphasize participant importance.<br/>
            - Provided phone alternative for interviews <br/>
            - Provided transportation schedule
              </p>",
            "<a
            href='https://doi.org/10.1016/j.cct.2011.01.007'
            target='_blank'
            >
            Nicholson, L. M., Schwirian, P. M., Klein, E. G., Skybo,
            T., Murray-Johnson, L., Eneli, I., Boettner, B., French,
            G. M., & Groner, J. A. (2011). Recruitment and retention
            strategies in longitudinal clinical studies with low-income
            populations. Contemporary Clinical Trials, 32(3), 353–362.
            https://doi.org/10.1016/j.cct.2011.01.007
              </a>",
            "</div>"
          )),
          HTML(paste(
            "<div class='page-para' style='margin-top: 2rem;'>",
            "<h4>Low-Income urban adolescents</h4>",
            "<h6>Location</h6>",
            "<p>Bronx, New York City</p>",
            "<p>
            - Study involved students as partners in the research project <br/>
            - Long term relationship (over 8 years)<br/>
            - Scheduling meetings and tasks focused on short-term
            goals: Meeting after school hours, preferred shorter
            regular meetings with short-term tasks <br/>
            - Developing ground rules and a voting process <br/>
            - Made meetings more interactive, with icebreakers and
            activities that engaged them with the topic <br/>
            - Multicultural understanding and cultural competency <br/>
            - Staff trained to deal with youth
              </p>",
            "<a
            href='https://doi.org/10.1097/fch.0000000000000057'
            target='_blank'
            >
            LoIacono Merves, M., Rodgers, C. R. R., Silver, E. J.,
            Sclafane, J. H., & Bauman, L. J. (2015). Engaging and
            Sustaining Adolescents in Community-Based Participatory
            Research. Family & Community Health, 38(1), 22–32.
            https://doi.org/10.1097/fch.0000000000000057
              </a>",
            "</div>"
          )),
          HTML(paste(
            "<div class='page-para' style='margin-top: 2rem;'>",
            "<h4>Low-income multiethnic</h4>",
            "<h6>Location</h6>",
            "<p>Minneapolis, Nashville, Stanford, Cleveland</p>",
            "<a
            href='https://doi.org/10.1186/s13063-019-3418-0'
            target='_blank'
            >
            Cui, Z., Truesdale, K.P., Robinson, T.N. et al.
            Recruitment strategies for predominantly low-income,
            multi-racial/ethnic children and parents to 3-year
            community-based intervention trials: Childhood Obesity
            Prevention and Treatment Research (COPTR) Consortium.
            Trials 20, 296 (2019).
            https://doi.org/10.1186/s13063-019-3418-0
              </a>",
            "</div>"
          )),
          HTML(paste(
            "<div class='page-para' style='margin-top: 2rem;'>",
            "<h4>Low-income</h4>",
            "<h6>Location</h6>",
            "<p>Canada</p>",
            "<p>Use of photovoice (visual qualitative research),
            a technique that involves providing, through photos,
            the space to develop dialogue about specific issues of concern.
            Researchers provide members of a community with a camera and
            ask them to take photos to answer the research question.
            Then, participants discuss in groups their images, as the
            images prove to be a catalyst in voicing opinions.</p>",
            "<a
            href='https://doi.org/10.1177/1476750320905900'
            target='_blank'
            >
            Loignon, C., Dupéré, S., Bush, P., Truchon, K., Boyer,
            S., & Hudon, C. (2020). Using photovoice to reflect on
            poverty and address social inequalities among primary
            care teams. Action Research, 147675032090590.
            https://doi.org/10.1177/1476750320905900
              </a>",
            "</div>"
          ))
        ),
        accordionItem(
          title =
            HTML("<span>Race and Ethnic Groups</span><br>
          <span style='font-size: 0.85rem;' class='text-lighter'>
          From the CCS Knowledge Map</span>"),
          status = "white",
          collapsed = TRUE,
          HTML(paste(
            "<div class='page-para'>",
            "<h4>Latino</h4>",
            "<h6>Location</h6>",
            "<p>Southeast Idaho, Southwest Oregon, Southwest Washington</p>",
            "<p>
              - Prefer taking surveys as groups <br/>
              - Prefer taking surveys orally, rather than on paper <br/>
              - Prefer groups divided based on family, rather than age groups
              </p>",
            "<a href='https://scholarsjunction.msstate.edu/jhse/vol4/iss2/11/'
            target='_blank'
            >
              Vega, L., Brody, B., & Cummins, M. (2016). Best Practices for
              Outreach and Engagement to Latino Audiences Using Community-Based
              Programs. Scholars Junction.
              https://scholarsjunction.msstate.edu/jhse/vol4/iss2/11/
              </a>",
            "</div>"
          )),
          HTML(paste(
            "<div class='page-para' style='margin-top: 2rem;'>",
            "<h4>Indigineous tribal communities</h4>",
            "<h6>Location</h6>",
            "<p>General Overview</p>",
            "<p>
           - Maintaining communication through check in emails/phone
            calls after every stage of data collection, ensuring calls
            are not missed by working in rotation, newsletters, website
            support<br/>
            - Group discussions, informal conversations over coffee, short
            interactive sessions, photo diaries,
            innovative approaches to dissemination<br/>
            - Coauthoring with peer researchers/ participants<br/>
            - Hosting socials, attending/ volunteering at community events,<br/>

            https://www.hindawi.com/journals/hsc/2023/1312525/tab5/
              </p>",
            "<a href='https://www.hindawi.com/journals/hsc/2023/1312525/tab5/'
            target='_blank'
            >
              Fisher, P. A., & Ball, T. J. (2003). Tribal Participatory
              Research:
              Mechanisms of a Collaborative Model.
              American Journal of Community
              Psychology, 32(3-4), 207–216.
              https://doi.org/10.1023/b:ajcp.0000004742.39858.c5
              </a>",
            "</div>"
          )),
          HTML(paste(
            "<div class='page-para' style='margin-top: 2rem;'>",
            "<h4>African Americans, Latinos, Asian Americans,
            and Pacific Islanders</h4>",
            "<h6>Location</h6>",
            "<p>Literature Review</p>",
            "<p>
           - Consent is an ongoing process, not a one time requirement<br/>
            - Include family and community in dialogue for certain groups<br/>
            - Retention strategies such as working with a community-based
            organisation and using same interviewing staff over time
              </p>",
            "<a href='https://doi.org/10.2105/ajph.2013.301706'
            target='_blank'
            >
              George, Sheba, et al. “A Systematic Review of Barriers and
              Facilitators to Minority Research Participation among African
              Americans, Latinos, Asian Americans, and Pacific Islanders.”
              American Journal of Public Health, vol. 104, no. 2, 2014, pp.
              e16–e31, https://doi.org/10.2105/ajph.2013.301706.
              </a>",
            "</div>"
          )),
          HTML(paste(
            "<div class='page-para' style='margin-top: 2rem;'>",
            "<h4>African American Women</h4>",
            "<h6>Location</h6>",
            "<p>Nashville, Tenessee</p>",
            "<a href='https://doi.org/10.1111/cts.12264'
            target='_blank'
            >
              Johnson, Davalynn A., et al. “Case Study: Community Engagement
              and Clinical Trial Success: Outreach to African American Women.”
              Clinical and Translational Science, vol. 8, no. 4, 9 Mar. 2015,
              pp.
              388–390, https://doi.org/10.1111/cts.12264. Accessed 7 Sept. 2020.
              </a>",
            "</div>"
          )),
          HTML(paste(
            "<div class='page-para' style='margin-top: 2rem;'>",
            "<h4>African American And Latina Women</h4>",
            "<h6>Location</h6>",
            "<p>South Florida</p>",
            "<a href='https://doi.org/10.1177/0193945906287215'
            target='_blank'
            >
              Alvarez, Roger A., et al. “Increasing Minority Research
              Participation through Community Organization Outreach.”
              Western Journal of Nursing Research, vol. 28, no. 5, Aug.
              2006, pp. 541–560, https://doi.org/10.1177/0193945906287215.
              </a>",
            "</div>"
          )),
          HTML(paste(
            "<div class='page-para' style='margin-top: 2rem;'>",
            "<h4>Hmong community</h4>",
            "<h6>Location</h6>",
            "<p>Portland and Salem, Oregon</p>",
            "<p>
            - Had a physical presence at the local community center, <br/>
            - Hosted an open community meeting at which study findings were
            disseminated. <br/>
            - Gave the option of giving interview in Hmong, and trained
            staff accordingly to accurately translate interview questions
              </p>",
            "<a href='https://doi.org/10.1177/1524839914561515'
            target='_blank'
            >
             Kue, Jennifer, et al. “Research Challenges and Lessons
             Learned from Conducting Community-Based Research with
             the Hmong Community.” Health Promotion Practice, vol.
             16, no. 3, Dec. 2014, pp. 411–418,
             https://doi.org/10.1177/1524839914561515.
              </a>",
            "</div>"
          )),
          HTML(paste(
            "<div class='page-para' style='margin-top: 2rem;'>",
            "<h4>Latino adolescents </h4>",
            "<h6>Location</h6>",
            "<p>Ann Arbor, Michigan and Philadelphia, Pennsylvania</p>",
            "<p>
            - Use of consistent bilingual personnel is contacting and
            administering questionnaires helped in building bond with
            a member of the team <br/>
            - Holding sessions on Saturday morning <br/>
            - Provided regular reminders before meetings <br/>
            - An infrastructure that mimics values of community such as
            school and family endorsement
              </p>",
            "<a href='https://doi.org/10.1111/j.1744-6155.2006.00076.x'
            target='_blank'
            >
              Villarruel, A. M., Jemmott, L. S., Jemmott, J. B., & Eakin,
              B. L. (2006). Recruitment and Retention of Latino Adolescents
              to a Research Study: Lessons Learned from a Randomized Clinical
              Trial. Journal for Specialists in Pediatric Nursing, 11(4),
              244–250. https://doi.org/10.1111/j.1744-6155.2006.00076.x
              </a>",
            "</div>"
          )),
          HTML(paste(
            "<div class='page-para' style='margin-top: 2rem;'>",
            "<h4>African-American adoloescents</h4>",
            "<h6>Location</h6>",
            "<p>Birmingham, Alabama </p>",
            "<p>
            - Socialisation with other teens <br/>
            - Creation of fun atmosphere through social activities
            between sessions, and food to allow engagement with peers <br/>
            - Monetary compensation  <br/>
            - Culturally relevant instructor
              </p>",
            "<a href='https://doi.org/10.1053/jpdn.2001.23151'
            target='_blank'
            >
             Jones, F. C., & Broome, M. E. (2001). Focus groups with African
             American adolescents: Enhancing recruitment and retention in
             intervention studies. Journal of Pediatric Nursing, 16(2),
             88–96. https://doi.org/10.1053/jpdn.2001.23151
              </a>",
            "</div>"
          )),
          HTML(paste(
            "<div class='page-para' style='margin-top: 2rem;'>",
            "<h4>Older African American rural people</h4>",
            "<h6>Location</h6>",
            "<p>Chapel Hill, North Carolina </p>",
            "<a href='https://doi.org/10.1177/0145721708325764'
            target='_blank'
            >
              Burns, D., Soward, A. C. M., Skelly, A. H., Leeman, J., &
              Carlson, J. (2008). Effective Recruitment and Retention
              Strategies for Older Members of Rural Minorities.
              The Diabetes Educator, 34(6), 1045–1052.
              https://doi.org/10.1177/0145721708325764
              </a>",
            "</div>"
          )),
          HTML(paste(
            "<div class='page-para' style='margin-top: 2rem;'>",
            "<h4>Latine youth</h4>",
            "<h6>Location</h6>",
            "<p>Santa Barbara, California</p>",
            "<p>
            - Key speakers who represented a range of constituencies,
            community members, youth themselves<br/>
            - Youth's voices also captures through multimedia display
            of those who participated in previous photovoice project<br/>
            - Small group reciprocal dialogue with a Latina woman
            as the scribe  <br/>
            - Storyboard method for brainstorming solutions to
            research question <br/>
            - Latino/a food served during forum, cultural music and
            performances, planned timings and limited transportation
            barriers <br/>
            - Raffle prizes
              </p>",
            "<a href='https://doi.org/10.1097/fch.0000000000000145'
            target='_blank'
            >
            Kia-Keating, M., Santacrose, D. E., Liu, S. R., & Adams,
            J. (2017). Using Community-Based Participatory Research
            and Human-Centered Design to Address Violence-Related
            Health Disparities Among Latino/a Youth. Family & Community
            Health, 40(2), 160–169.
            https://doi.org/10.1097/fch.0000000000000145
              </a>",
            "</div>"
          )),
          HTML(paste(
            "<div class='page-para' style='margin-top: 2rem;'>",
            "<h4>Refugee & migrant women of colour</h4>",
            "<h6>Location</h6>",
            "<p>Australia</p>",
            "<p>
            - More comfort in a mobile application vs using a
            laptop/computer<br/>
            - Use of a visual approach, using primarily graphics to
            allow those with low digital and English literacy to navigate,
            also to appropriately represent cultural diversity<br/>
            - Produce photo-based video content and PDFs, speaking slowly
            and by a migrant staff member <br/>
            - Having migrant staff member made participants feel like they
            had some level of control over project <br/>
            - Use of WhatsApp to share videos during pandemic,
            uploading tutorials on YouTube
              </p>",
            "<a href='https://doi.org/10.1145/3567554'
            target='_blank'
            >
            Hedditch, S., & Vyas, D. (2022). Design Justice in Practice.
            Proceedings of the ACM on Human-Computer Interaction,
            7(GROUP), 1–39. https://doi.org/10.1145/3567554
              </a>",
            "</div>"
          )),
          HTML(paste(
            "<div class='page-para' style='margin-top: 2rem;'>",
            "<h4>Indigenous </h4>",
            "<h6>Location</h6>",
            "<p>Australia</p>",
            "<p>
            - Most marginalised communities do not have platforms of
            their own, and must use external platforms that perpetuate
            digital colonization of language.<br/>
            - Use of native language and cultural context, politically
            aligns with Indigineous language authority<br/>
            - Creation of video materials by Indigineous community
            members sharing employability, literacy and numeracy skills,
            centering Indigineous perspective
              </p>",
            "<a href='https://doi.org/10.5334/jime.560'
            target='_blank'
            >
            Funk, J., & Guthadjaka, K. (2020). Indigenous Authorship
            on Open and Digital Platforms: Social Justice Processes
            and Potential. Journal of Interactive Media in Education,
            2020(1). https://doi.org/10.5334/jime.560
              </a>",
            "</div>"
          )),
          HTML(paste(
            "<div class='page-para' style='margin-top: 2rem;'>",
            "<h4>Latino</h4>",
            "<h6>Location</h6>",
            "<p>New York</p>",
            "<p>
            - Digitally recorded, semi-structured interviews uploaded
            on the website + users can make comments<br/>
            - Use of Vojo, a multilingual tool allowing users to share SMS,
            voice messages, and images. Allows users to remain anonymous<br/>
            - Increased use of social media and its power
              </p>",
            "<a
            href='https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2569484'
            target='_blank'
            >
            Mayorga, E. (2014, July). Toward Digital, Critical,
            Participatory Action Research: Lessons from the #Barrioedproj.
            Ssrn.com.
            https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2569484
              </a>",
            "</div>"
          )),
          HTML(paste(
            "<div class='page-para' style='margin-top: 2rem;'>",
            "<h4>Indigenous</h4>",
            "<h6>Location</h6>",
            "<p>Canada</p>",
            "<p>
            - Use of collaborative podcasting. <br/>
            - Decolonising research agenda, through storytelling,
            representing, reframing, and sharing<br/>
            - New methods that meaningfully engage Indigenous culture
            such as oral storytelling, and collective dialogue. <br/>
            - Previous successful use of photovoice, participatory photography,
            digital storytelling, audio documentary, and
            participatory video <br/>
            - Can be accessed on-demand, while completing other tasks,
            more accessible, less intrusive, less expertise needed
              </p>",
            "<a
            href='https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2569484'
            target='_blank'
            >
            Day, L., Cunsolo, A., Castleden, H., Martin, D., Hart,
            C. K., Anaviapik-Soucie, T., Russell, G. V., Paul, C.,
            Dewey, C., & Harper, S. L. (2017). The Expanding Digital
            Media Landscape of Qualitative and Decolonizing Research:
            Examining Collaborative Podcasting as a Research Method.
              </a>",
            "</div>"
          )),
          HTML(paste(
            "<div class='page-para' style='margin-top: 2rem;'>",
            "<h4>Latino</h4>",
            "<h6>Location</h6>",
            "<p>Boston</p>",
            "<p>
            - Use of PhotoVoice for Latinx adults <br/>
            - Photovoice facilitates the participation of marginalized
            communities in research. Challenges power dynamics and
            centers the participant<br/>
            - Dissemination of photovoice results with community centre.
              </p>",
            "<a
            href='https://doi.org/10.1186/s12889-023-14983-7'
            target='_blank'
            >
            Muroff, J., Do, D., Cristina Araujo Brinkerhoff, Chassler,
            D., Myrna Alfaro Cortes, Baum, M., Genessis Guzman-Betancourt,
            Reyes, D., López, L. M., Roberts, M., Diliana De Jesus, Stewart,
            E., & Linda Sprague Martinez. (2023). Nuestra Recuperación
            [Our Recovery]: using photovoice to understand the factors
            that influence recovery in Latinx populations. BMC Public Health,
            23(1). https://doi.org/10.1186/s12889-023-14983-7
              </a>",
            "</div>"
          ))
        ),
        accordionItem(
          title =
            HTML("<span>Educational Attainment Groups</span><br>
          <span style='font-size: 0.85rem;' class='text-lighter'>
          From the CCS Knowledge Map</span>"),
          status = "white",
          collapsed = TRUE,
          "To address the low representativeness among different educational
          attainment groups in discussions about climate change, targeted
          outreach strategies are crucial. Implement educational programs
          tailored to diverse educational levels, including workshops,
          seminars, and webinars that cater to varying degrees of literacy.
          Utilize accessible communication channels such as online platforms,
          podcasts, and community newsletters. Collaborate with educational
          institutions to integrate climate change education into curricula
          at all levels. Emphasize the relevance of climate issues to diverse
          educational backgrounds and highlight the role of informed
          decision-making in shaping sustainable futures. By tailoring messages
          to meet the specific needs and interests of varied educational
          attainment groups, we can bridge the gap and foster inclusive
          participation in climate change discourse.",
          class = "page-para"
        )
      ),
    ),
  )
}
