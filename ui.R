ui <- dashboardPage(
  
  # HEADER ------------------------------------------------------------------
  dashboardHeader(
    title = span(img(src = "PatientDashboard/abc.jpg", height = 35), "ABC Hospitals"),
    # userOutput("user"),
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
  
  dashboardSidebar(disable = TRUE),
  
  # BODY --------------------------------------------------------------------
  
  dashboardBody(
    
    tags$head(
      tags$link(
        rel = "stylesheet", 
        type = "text/css", 
        href = "www/style.css")
    ),
    
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
                 style = "success")
      )
    )
  ),
  
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
        uiOutput("box_year")
      )
    )
  )
)
  