ui <- dashboardPage(
  
  skin = 'black',
  title = 'PatientProfile',
  
  # HEADER ------------------------------------------------------------------
  dashboardHeader(
    title = span(img(src = "PatientDashboard/abc.jpg", height = 35), "ABC Hospitals"),
    titleWidth = 300,
    dropdownMenu(
      type = "notifications",
      headerText = strong("HELP"),
      icon = icon("question"),
      badgeStatus = NULL,
      notificationItem(
        text = (steps$text[1]),
        icon = icon("spinner")
      ),
      notificationItem(
        text = steps$text[2],
        icon = icon("address-card")
      ),
      notificationItem(
        text = steps$text[3],
        icon = icon("calendar")
      ),
      notificationItem(
        text = steps$text[4],
        icon = icon("user-md")
      ),
      notificationItem(
        text = steps$text[5],
        icon = icon("ambulance")
      ),
      notificationItem(
        text = steps$text[6],
        icon = icon("flask")
      ),
      notificationItem(
        text = strong(steps$text[7]),
        icon = icon("exclamation")
      )
    ),
    tags$li(
      a(
        strong("ABOUT"),
        height = 40,
        href = "Paste About Link Here",
        title = "",
        target = "_blank"
      ),
      class = "dropdown"
    )
  ),
  
  # SIDEBAR -----------------------------------------------------------------

  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", icon = icon("th"), tabName = "widgets",
               badgeLabel = "new", badgeColor = "green")
      )
    ),
  
  # BODY --------------------------------------------------------------------
  
  dashboardBody(
    
    tags$head(
      tags$style(HTML("
      .table>tbody>tr>td, .table>tbody>tr>th, .table>tfoot>tr>td, .table>tfoot>tr>th, .table>thead>tr>td, .table>thead>tr>th {
        padding: 8px;
        line-height: 1.42857143;
        vertical-align: top;
        border: 0px; 
        font-family: 'Lato', sans-serif;
      }
    ")),
      tags$link(
        rel = "stylesheet", 
        type = "text/css", 
        href = "www/style.css")
    ),
    tags$link(
      rel = "preconnect",
      href="https://fonts.googleapis.com"
    ),
    tags$link(
      rel = "preconnect",
      href="https://fonts.gstatic.com",
      crossorigin = TRUE
    ),
    tags$link(
      href="https://fonts.googleapis.com/css2?family=Lato&display=swap", rel="stylesheet"
    ),
    
    useShinyjs(),
    # useShinyalert(),
    
    fluidRow(
      column(
        width = 12,
        bsButton("patients", 
                 label = "PATIENT DETAILS", 
                 icon = icon("user"), 
                 style = "success"),
        bsButton("scans", 
                 label = "SCANS", 
                 icon = icon("brain"), 
                 style = "success"),
        bsButton("diagnostics", 
                 label = "DIAGNOSTICS", 
                 icon = icon("flask", class = "flask-box"), 
                 style = "success"),
        bsButton("outcome", 
                 label = "OUTCOME", 
                 icon = icon("file-chart-column"), 
                 style = "success"),
        br(),
        br()
      )
    ),
    fluid_design("scans_panel", "box1", "box2", "box3", "box4"),
    fluid_design("diagnostics_panel", "box5", "box6", "box7", "box8"),
    fluid_design("outcome_panel", "box_los1", "box_los2", "box_los3", NULL),
    
    fluidRow(
      div(
        id = "patients_panel", 
        column(
          width = 12,
          uiOutput("box_pat")
        ),
        column(
          width = 6,
          uiOutput("box_pat2")
        ),
        column(
          width = 6,
          uiOutput("box_pat3")
        )
      )
    )
  )
  
  
)
  