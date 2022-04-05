server <- function(input, output, session) {
  
  # use action buttons as tab selectors
  update_all <- function(x) {
    updateSelectInput(session, "tab",
                      choices = c("", "Patients", "Antimicrobial consumption", "Diagnostics", "Outcome"),
                      label = "",
                      selected = x
    )
  }
  
  observeEvent(input$patients, {
    update_all("Patients")
  })
  observeEvent(input$scans, {
    update_all("Medical Scans")
  })
  observeEvent(input$diagnostics, {
    update_all("Diagnostics")
  })
  observeEvent(input$outcome, {
    update_all("Outcome")
  })
  
  # DYNAMIC RENDER RULES ----------------------------------------------------
  
  observeEvent("", {
    show("patients_panel")
    hide("scans_panel")
    hide("diagnostics_panel")
    hide("outcome_panel")
  }, once = TRUE)
  
  observeEvent(input$patients, {
    show("patients_panel")
    hide("diagnostics_panel")
    hide("scans_panel")
    hide("outcome_panel")
  })
  observeEvent(input$scans, {
    show("scans_panel")
    hide("diagnostics_panel")
    hide("outcome_panel")
    hide("patients_panel")
  })
  observeEvent(input$diagnostics, {
    show("diagnostics_panel")
    hide("scans_panel")
    hide("outcome_panel")
    hide("patients_panel")
  })
  observeEvent(input$outcome, {
    show("outcome_panel")
    hide("diagnostics_panel")
    hide("scans_panel")
    hide("patients_panel")
  })
  
  # show active button with color
  
  observeEvent(input$tab, {
    x <- input$tab
    updateButton(session, "patients", style = {
      if (x == "Patients") {
        paste("warning")
      } else {
        paste("success")
      }
    })
    updateButton(session, "scans", style = {
      if (x == "Medical Scans") {
        paste("warning")
      } else {
        paste("success")
      }
    })
    updateButton(session, "diagnostics", style = {
      if (x == "Diagnostics") {
        paste("warning")
      } else {
        paste("success")
      }
    })
    updateButton(session, "outcome", style = {
      if (x == "Outcome") {
        paste("warning")
      } else {
        paste("success")
      }
    })
  })
  
  
  # UI - PATIENTS -----------------------------------------------------------
  
  output$box_pat <- renderUI({
    tabBox(
      id = "box_pat",
      width = NULL,
      height = 400,
      tabPanel(
        title = "PERSONAL DETAILS",
        htmlOutput("patients_total"),
        withSpinner(
          tableOutput("table_pat_all"),
          type = 4,
          color = "#d33724",
          size = 0.7
        )
      )
    )
  })
  
  output$box_pat2 <- renderUI({
    
    tabBox(
      id = "box_pat2",
      width = NULL,
      height = 400,
      tabPanel(
        title = "ALLERGIES",
        div(
          style = "position: absolute; left: 4em; bottom: 0.5em;",
          dropdown( 
            downloadButton(outputId = "down_allergies", label = "Download List"),
            size = "xs",
            icon = icon("download", class = "opt"), 
            up = TRUE
          )
        ),
        htmlOutput("allergies_table"),
        withSpinner(
          tableOutput("allergies_table"),
          type = 4,
          color = "#d33724",
          size = 0.7
        )
      )
    )
  })
  
  output$box_pat3 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        id = "box_pat3",
        width = NULL,
        height = 400,
        tabPanel(
          title = "PRESCRIPTIONS",
          div(
            style = "position: absolute; left: 4em; bottom: 0.5em;",
            dropdown( 
              downloadButton(outputId = "down_meds", label = "Download Prescription"),
              size = "xs",
              icon = icon("download", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            tableOutput("meds_table"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        )
      )
    )
  })
  
  
  userProfileTable <- function(sample_patient) {
    
    personal_details_ui <- tibble(
      c1 = c("Last Name", sample_patient$last, "", "Address", sample_patient$address, "", "", "Birth Date", sample_patient$birthdate),
      c2 = c("First Name", sample_patient$first, "", "", "", "", "", "", ""),
      c3 = c("Middle Name", sample_patient$maiden, "", "Phone Number", sample_patient$ssn, "", "", "Birth Place", sample_patient$birthplace),
      c4 = c("Salutaion", sample_patient$prefix, "", "Email", sample_patient$email, "", "", "", "")
    )
    
    as_hux(personal_details_ui, add_colnames = FALSE) -> ht1
    ht1 %>% 
      set_font(col = 1:4, row = 1:nrow(ht1), "Lato Bold") %>% 
      set_font_size(value = 30) %>% 
      set_text_color(col = 1:4, row = c(1, 4, 8), "grey") ->htt1 
    # merge_cells(2, 1:4) 
    
    
  }
  
  # table_pat_all <- reactive(
  #   table(
  #     htt1,
  #     rownames = FALSE,
  #     colnames = FALSE,
  #     options = list(
  #       dom = 'frtp',
  #       style = "bootstrap"
  #       # lengthMenu = c(seq(5, 150, 5))
  #     )
  #   )
  # )
  
  output$table_pat_all <- renderTable({
    htt1 <- userProfileTable(sample_patient)
    # table_pat_all()
  },
  colnames = FALSE,
  width = "100%")
  
  # FUNCTIONS ------------------------------------------------------------------
  
  allergiesTable <- function(allergies1) {
    allergies1 <- allergies1 %>% 
      clean_names()
    
    ht1 <- tibble(allergies1$description)
    ht1
  }
  
  prescriptionTable <- function(medications1){
    
    medications1 %>% 
      mutate(start = as.Date(as.character(start)), stop = as.Date(as.character(stop))) %>% 
      select(start, stop, description) %>% 
      arrange(start) -> ht3
    
    ht3 <- sapply(ht3, as.character)
    
    ht3[is.na(ht3)] <- ""
    ht3 <- ht3 %>% as_tibble()
  }
  
  # UI - PATIENTS - 2 ------------------------------------------------------------------
  allergies_table <- reactive(
    {
      ht2 <- allergiesTable(allergies1)
      validate(
        need(nrow(ht2) > 0, "No History Of Allergies.")
      )
      ht2 %>% as.tibble()
    }
  )
  
  output$allergies_table <- renderTable(
    allergies_table(),
    colnames = FALSE,
    width = "100%"
  )
  
  
  # UI - PATIENTS - 3 ------------------------------------------------------------------
  prescr_table <- reactive(
    {
      htt3 <- prescriptionTable(medications1)
      validate(
        need(nrow(htt3) > 0, "No Prescriptions.")
      )
      htt3
    }
  )
  
  output$meds_table <- renderTable(
    {
      prescr_table()
    },
    colnames = FALSE,
    width = "100%"
  )
  
  
  # UI - DIAGNOSTICS - 1 ------------------------------------------------------------------
  
  output$box5 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        id = "box5",
        width = NULL,
        height = 400,
        tabPanel(
          title = "Blood Pressure",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
            dropdown(
              radioGroupButtons(
                inputId = "box5.1",
                label = "Change time", 
                choiceNames = c("Year", "Quarter", "Month"), 
                choiceValues = c("year", "yearquarter_adm", "yearmonth_adm"), 
                selected = "year", 
                direction = "vertical"
              ),
              radioGroupButtons(
                inputId = "box5.2",
                label = "Change plot", 
                choiceNames = c("Count", "Proportion"), 
                choiceValues = c("dodge", "fill"), 
                selected = "dodge", 
                direction = "vertical"
              ),
              size = "xs",
              icon = icon("gear", class = "opt"), 
              up = TRUE
            )
          ),
          div(
            style = "position: absolute; left: 4em;bottom: 0.5em;",
            dropdown(
              downloadButton(outputId = "down_box_5", label = "Download plot"),
              size = "xs",
              icon = icon("download", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            plotlyOutput("plot_dia_adm", height = 300),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        ),
        div(
          style = "position: absolute; right: 0.5em; bottom: 0.5em;",
          conditionalPanel(
            "input.box5 == 'Blood Pressure'",
            actionBttn(
              inputId = "dia_adm",
              icon = icon("search-plus", class = "opt"),
              style = "fill",
              color = "danger",
              size = "xs"
            )
          )
        )
      )
    )
  })
  
  plot_blood_press <- reactive({
    data1 <- observations %>% 
      clean_names() %>% 
      mutate(value = coalesce(as.numeric(value), 0), date = as.Date(as.character(date), format = "%m/%d/%Y")) %>% 
      filter(description == "Systolic Blood Pressure" | description == "Diastolic Blood Pressure", 
             patient == "71949668-1c2e-43ae-ab0a-64654608defb") %>% 
      select(date, description, value)
    
    data <- data1
    
    dvbp_plot  <- ggplot()
    
    if(all(data1$value == 0)){
      
      # dvbp_plot <- plotly_empty(type = "scatter", mode = "markers") %>%
      #   config(
      #     displayModeBar = FALSE
      #   ) %>%
      #   layout(
      #     title = list(
      #       text = "No History of Blood Pressure",
      #       yref = "paper",
      #       y = 0.5
      #     )
      #   )
      
      text = paste("No History Of Blood Pressure.")
      dvbp_plot <- ggplot() + 
        annotate("text", x = 4, y = 25, size=6, label = text) + 
        theme_void()
      
    } else{
      
      
      # dvbp_plot <- dvbp_plot %>% 
      #   add_trace(y = ~value.x, name = "Diastolic Blood Pressure", mode = "lines+markers", marker = list(color = seq(80, 100), size = 10))
      # 
      # dvbp_plot <- dvbp_plot %>% 
      #   add_trace(y = ~value.y, name = "Systolic Blood Pressure", mode = "lines+markers", marker = list(color = seq(120, 150), size = 10))
      # 
      # dvbp_plot <- dvbp_plot %>% 
      #   layout(title = "Blood Pressure",
      #          xaxis = list(title = "Date"),
      #          yaxis = list(title = "(mmHg)"))
      
      dvbp_plot <- ggplot(data1, aes(x = date, y = value, color = description)) + 
        geom_line() + 
        theme_classic() + 
        geom_point(aes(color = description)) +
        ggtitle("Blood Pressure") +
        labs(color='Type') +
        xlab("Date") +
        ylab("(mm/Hg)") +
        scale_fill_discrete(labels=c("Control", "Treatment 1")) +
        theme(legend.key.size = unit(0.4, 'cm'))
    }
    
    dvbp_plot <- ggplot(data1, aes(x = date, y = value, color = description)) + 
      geom_line() + 
      theme_classic() + 
      geom_point(aes(color = description)) +
      ggtitle("Blood Pressure") +
      labs(color='Type') +
      xlab("Date") +
      ylab("(mm/Hg)") +
      scale_fill_discrete(labels=c("Control", "Treatment 1")) +
      theme(legend.key.size = unit(0.4, 'cm'))
    
    dvbp_plot %>% ggplotly()
  })
  
  
  observeEvent((input$dia_adm), {
    showModal(modalDialog(
      renderPlot({
        plot_blood_press() + theme(
          axis.title = element_text(size = 20),
          text = element_text(size = 20),
          plot.title = element_text(size = 26)
        )
      }, height = 600),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  
  
  output$plot_dia_adm <- renderPlotly({
    plot_blood_press()
  })

  
  # UI - DIAGNOSTICS - 2 ------------------------------------------------------------------
  
  output$box6 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        id = "box6",
        width = NULL,
        height = 400,
        tabPanel(
          title = "Body Mass Index",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
            dropdown(
              radioGroupButtons(
                inputId = "box6.1",
                label = "Select group", 
                choiceNames = c("Antimicrobial - Groups", "Antimicrobials", "Year", "Specialty", "Subspecialty", "Origin"),
                choiceValues = c("ab_group", "ab_type", "year", "specialty", "sub_specialty", "adm_route"), 
                selected = "year", 
                direction = "vertical"
              ),
              size = "xs",
              icon = icon("gear", class = "opt"), 
              up = TRUE
            )
          ),
          div(
            style = "position:absolute;left:4em;bottom: 0.5em;",
            dropdown(
              downloadButton(outputId = "down_box_7", label = "Download plot"),
              size = "xs",
              icon = icon("download", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            plotlyOutput("plot_bmi", height = 300),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        ),
        div(
          style = "position:absolute;right:0.5em;bottom: 0.5em;",
          conditionalPanel(
            "input.box6 == 'Body Mass Index'",
            actionBttn(
              inputId = "dia_perform",
              icon = icon("search-plus", class = "opt"),
              style = "fill",
              color = "danger",
              size = "xs"
            )
          )
        )
      )
    )
  })
  
  observeEvent((input$dia_perform), {
    showModal(modalDialog(
      renderPlotly({ 
        plot_bmi() + theme(
          axis.title = element_text(size = 20),
          text = element_text(size = 20),
          plot.title = element_text(size = 26)
        )
      }),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  
  plot_bmi <- reactive(
    {
      data <- observations %>% 
        clean_names() %>% 
        select(date, patient, description, value) %>%
        mutate(value = coalesce(as.numeric(value), 0), date = as.Date(as.character(date), format = "%m/%d/%Y")) %>% 
        filter(description == "Body Mass Index",  patient == "71949668-1c2e-43ae-ab0a-64654608defb")
      
      
      dvb_plot <- ggplot(data, aes(x = date, y = value)) + geom_line() +
        theme_classic() +
        geom_point(aes(color = value)) +
        ggtitle(paste0("Body Mass Index")) +
        scale_fill_manual(values = c("#a6a6a6", "#d1351b"), name = "Body Mass Index") +
        xlab("Date") + 
        # ylab(bquote((kg/m^2))) +
        ylab("kg/m<sup>2</sup>") +
        labs(colour = "BMI")
      
      
      dvb_plot %>% ggplotly()
      
      # dvb_plot <- plot_ly(data, x = ~date, y = ~value, type = 'scatter', mode = 'lines', name = "BMI") %>% 
      #   add_trace(y = ~value, mode = "markers", marker = list(color = seq(18.5, 30), size = 10)) %>% 
      #   layout(title = 'Body Mass Index',
      #          xaxis = list(title = 'Date'),
      #          yaxis = list (title = 'BMI'))
      
      # dvb_plot
    }
  )
  
  
  output$plot_bmi <- renderPlotly(
    plot_bmi()
  )
  
  
  # UI - DIAGNOSTICS - 3 ------------------------------------------------------------------
  
  output$box7 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        id = "box7",
        width = NULL,
        height = 400,
        tabPanel(
          title = "Blood Glucose", 
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
            dropdown(
              radioGroupButtons(
                inputId = "box7.0",
                label = "Select group", 
                choiceNames = c("All", "Antimicrobial - Groups", "Antimicrobials", "Year", "Gender", "Specialty", "Subspecialty", "Origin"),
                choiceValues = c("fullname", "ab_group", "ab_type", "year", "gender", "specialty", "sub_specialty", "adm_route"), 
                selected = "fullname", 
                direction = "vertical"
              ),
              size = "xs",
              icon = icon("gear", class = "opt"), 
              up = TRUE
            )
          ),
          div(
            style = "position: absolute; left: 4em; bottom: 0.5em;",
            dropdown(
              sliderInput(
                inputId = "box7.1",
                label = "Show top ...", 
                min = 0, 
                max = 50, 
                value = c(25), 
                step = 5
              ),
              size = "xs",
              icon = icon("search-plus", class = "opt"), 
              up = TRUE
            )
          ),
          div(
            style = "position: absolute; left: 7.5em; bottom: 0.5em;",
            dropdown(
              downloadButton(outputId = "down_box_micro", label = "Download plot"),
              size = "xs",
              icon = icon("download", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            plotlyOutput("plot_gluc", height = 300),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        ),
        div(
          style = "position:absolute;right:0.5em;bottom: 0.5em;",
          conditionalPanel(
            "input.box7 == 'Blood Glucose'",
            actionBttn(
              inputId = "micro_plus",
              icon = icon("search-plus", class = "opt"),
              style = "fill",
              color = "danger",
              size = "xs"
            )
          )
        )
      )
    )
  })
  
  observeEvent((input$micro_plus), {
    showModal(modalDialog(
      renderPlotly({
        plot_gluc() + theme(
          axis.title = element_text(size = 20),
          text = element_text(size = 20),
          plot.title = element_text(size = 26)
        )
      }, height = 600),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  
  
  plot_gluc <- reactive(
    {
      data <- observations %>% 
        clean_names() %>% 
        select(date, patient, description, value) %>%
        mutate(value = coalesce(as.numeric(value), 0),  date = as.Date(as.character(date), format = "%m/%d/%Y")) %>% 
        filter(description == "Glucose",  patient == "96b24072-e1fe-49cd-a22a-6dfb92c3994c")
      
      
      # dvg_plot <- plot_ly(data, x = ~date, y = ~value, mode = "lines+markers", name = "Blood Glucose") %>% 
      #   layout(title = 'Blood Glucose',
      #          xaxis = list(title = 'Date'),
      #          yaxis = list (title = 'Glucose (mg/dL)'))
      
      dvg_plot <- ggplot(data, aes(x = date, y = value)) + 
        geom_line() + 
        theme_classic() + 
        ggtitle("Blood Glucose") +
        labs(color='Glucose Level') +
        xlab("Date") +
        ylab("(mmol/L)") +
        scale_fill_discrete(labels=c("Control", "Treatment 1")) +
        theme(legend.key.size = unit(0.4, 'cm')) +
        geom_point(aes(color = value))
      
      dvg_plot <- dvg_plot %>% ggplotly()
      
      dvg_plot
    }
  )
  
  output$plot_gluc <- renderPlotly(
    plot_gluc()
  )
  
  
  
  # UI - DIAGNOSTICS - 4 ------------------------------------------------------------------
  
  # output$box8 <- renderUI({
  #   div(
  #     style = "position: relative",
  #     tabBox(
  #       id = "box8",
  #       width = NULL,
  #       height = 400,
  #       tabPanel(
  #         title = "Cholestrol", 
  #         div(
  #           style = "position: absolute; left: 0.5em; bottom: 0.5em;",
  #           dropdown(
  #             radioGroupButtons(
  #               inputId = "box8.0",
  #               label = "Select group", 
  #               choiceNames = c("All", "Antimicrobial - Groups", "Antimicrobials", "Year", "Gender", "Specialty", "Subspecialty", "Origin"),
  #               choiceValues = c("fullname", "ab_group", "ab_type", "year", "gender", "specialty", "sub_specialty", "adm_route"), 
  #               selected = "fullname", 
  #               direction = "vertical"
  #             ),
  #             size = "xs",
  #             icon = icon("gear", class = "opt"), 
  #             up = TRUE
  #           )
  #         ),
  #         div(
  #           style = "position: absolute; left: 4em; bottom: 0.5em;",
  #           dropdown(
  #             sliderInput(
  #               inputId = "box8.1",
  #               label = "Show top ...", 
  #               min = 0, 
  #               max = 50, 
  #               value = c(25), 
  #               step = 5
  #             ),
  #             size = "xs",
  #             icon = icon("search-plus", class = "opt"), 
  #             up = TRUE
  #           )
  #         ),
  #         div(
  #           style = "position: absolute; left: 17.5em; bottom: 0.5em;",
  #           dropdown(
  #             downloadButton(outputId = "down_box_micro", label = "Download plot"),
  #             size = "xs",
  #             icon = icon("download", class = "opt"), 
  #             up = TRUE
  #           )
  #         ),
  #         withSpinner(
  #           plotlyOutput("plot_chol", height = 300),
  #           type = 4,
  #           color = "#d33724",
  #           size = 0.7
  #         )
  #       ),
  #       div(
  #         style = "position:absolute;right:0.5em;bottom: 0.5em;",
  #         conditionalPanel(
  #           "input.box8 == 'Cholestrol'",
  #           actionBttn(
  #             inputId = "chol_plus",
  #             icon = icon("search-plus", class = "opt"),
  #             style = "fill",
  #             color = "danger",
  #             size = "xs"
  #           )
  #         )
  #       )
  #     )
  #   )
  #   
  # })
  # 
  # 
  # observeEvent((input$chol_plus), {
  #   showModal(modalDialog(
  #     renderPlotly({
  #       plot_chol() + theme(
  #         axis.title = element_text(size = 20),
  #         text = element_text(size = 20),
  #         plot.title = element_text(size = 26)
  #       )
  #     }, height = 600),
  #     easyClose = TRUE,
  #     size = "l",
  #     footer = NULL
  #   ))
  # })
  # 
  # plot_chol <- reactive(
  #   {
  #     data1 <- observations %>% 
  #       clean_names() %>% 
  #       mutate(value = coalesce(as.numeric(value), 0), date = as.Date(as.character(date), format = "%m/%d/%Y")) %>% 
  #       filter(description == "Total Cholesterol" | 
  #                description == "Low Density Lipoprotein Cholesterol" | 
  #                description == "High Density Lipoprotein Cholesterol", 
  #              patient == "71949668-1c2e-43ae-ab0a-64654608defb") %>% 
  #       select(date, description, value)
  #     
  #     
  #     if(all(data1$value == 0)){
  #       
  #       text = paste("No History Of Cholestrol.")
  #       dvchol_plot <- ggplot() + 
  #         annotate("text", x = 4, y = 25, size=6, label = text) + 
  #         theme_void()
  #       
  #     } else{
  #       
  #       dvchol_plot <- ggplot(data1, aes(x = date, y = value, color = description)) + 
  #         geom_line() + 
  #         theme_classic() + 
  #         ggtitle("Cholestrol") +
  #         labs(color='Type') +
  #         xlab("Date") +
  #         ylab("(mg/dL)") +
  #         scale_fill_discrete(labels=c("Control", "Treatment 1")) +
  #         theme(legend.key.size = unit(0.4, 'cm'))
  #     }
  #     
  #     dvchol_plot %>% ggplotly()
  #   }
  # )
  
  # UI - PATIENTS - 1 ------------------------------------------------------------------
  
  
  
  
  
  
  
  
}