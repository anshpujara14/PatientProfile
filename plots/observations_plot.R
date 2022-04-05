# DATE VS. BMI
observations %>% 
  clean_names() %>% 
  select(date, patient, description, value) %>%
  mutate(value = coalesce(as.numeric(value), 0)) %>% View()
  filter(description == "Body Mass Index", patient == "71949668-1c2e-43ae-ab0a-64654608defb")

date_vs_bmi <- function(observations){
  
  data <- observations %>% 
    clean_names() %>% 
    select(date, patient, description, value) %>%
    mutate(value = coalesce(as.numeric(value), 0), date = as.Date(as.character(date), format = "%m/%d/%Y")) %>% 
    filter(description == "Body Mass Index",  patient == "71949668-1c2e-43ae-ab0a-64654608defb")
  
  
  dvb_plot <- ggplot(data, aes(x = date, y = value)) + geom_line() +
    theme_classic() +
    geom_point(aes(color = value)) +
    # geom_point(aes(color = cut(data$value, c(18.5, 20, 25, 30)))) +
    ggtitle(
      paste0("Body Mass Index")) +
    scale_fill_manual(
      values = c("#a6a6a6", "#d1351b"),
      name = "Body Mass Index"
    ) +
    geom_col(position = input$box5.2,
             color = "black",
             alpha = 0.8)

  
  dvb_plot <- plot_ly(data, x = ~date, y = ~value, type = 'scatter', mode = 'lines', name = "BMI") %>% 
    add_trace(y = ~value, mode = "markers", marker = list(color = seq(18.5, 30), size = 10)) %>% 
    layout(title = 'Body Mass Index',
           xaxis = list(title = 'Date'),
           yaxis = list (title = 'BMI'))
  
  dvb_plot
}

# DATE VS. BP

date_vs_bp <- function(observations){
  
  # data_sbp <- observations %>% 
  #   clean_names() %>% 
  #   select(date, patient, description, value) %>%
  #   mutate(value = coalesce(as.numeric(value), 0), date = as.Date(as.character(date), format = "%m/%d/%Y")) %>% 
  #   filter(description == "Systolic Blood Pressure", patient == "71949668-1c2e-43ae-ab0a-64654608defb")
  # 
  # data_dbp <- observations %>% 
  #   clean_names() %>% 
  #   select(date, patient, description, value) %>%
  #   mutate(value = coalesce(as.numeric(value), 0), date = as.Date(as.character(date), format = "%m/%d/%Y")) %>% 
  #   filter(description == "Diastolic Blood Pressure", patient == "71949668-1c2e-43ae-ab0a-64654608defb")
  
  data1 <- observations %>% 
    clean_names() %>% 
    mutate(value = coalesce(as.numeric(value), 0), date = as.Date(as.character(date), format = "%m/%d/%Y")) %>% 
    filter(description == "Systolic Blood Pressure" | description == "Diastolic Blood Pressure", 
           patient == "71949668-1c2e-43ae-ab0a-64654608defb") %>% 
    select(date, description, value)
  
  # data <- data1
  
  dvbp_plot  <- ggplot()
  
  if(all(data$value == 0)){
    
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
      ggtitle("Blood Pressure") +
      labs(color='Type') +
      xlab("Date") +
      ylab("(mm/Hg)") +
      scale_fill_discrete(labels=c("Control", "Treatment 1")) +
      theme(legend.key.size = unit(0.4, 'cm'))
  }
  
  dvbp_plot
}


# DATE VS. GLUCOSE

date_vs_gluc <- function(observations) {
  
  data <- observations %>% 
    clean_names() %>% 
    select(date, patient, description, value) %>%
    mutate(value = coalesce(as.numeric(value), 0),  date = as.Date(as.character(date), format = "%m/%d/%Y")) %>% 
    filter(description == "Glucose",  patient == "96b24072-e1fe-49cd-a22a-6dfb92c3994c")
  
  
  dvg_plot <- plot_ly(data, x = ~date, y = ~value, mode = "lines+markers", name = "Blood Glucose") %>% 
    layout(title = 'Blood Glucose',
           xaxis = list(title = 'Date'),
           yaxis = list (title = 'Glucose (mg/dL)'))
  
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
date_vs_gluc(observations)

ggplotly(dvg_plot)

# DATE VS. CHOLESTROL

date_vs_cholest <- function(observations){
  
  data1 <- observations %>% 
    clean_names() %>% 
    mutate(value = coalesce(as.numeric(value), 0), date = as.Date(as.character(date), format = "%m/%d/%Y")) %>% 
    filter(description == "Total Cholesterol" | 
             description == "Low Density Lipoprotein Cholesterol" | 
             description == "High Density Lipoprotein Cholesterol", 
           patient == "71949668-1c2e-43ae-ab0a-64654608defb") %>% 
    select(date, description, value)
  
  
  if(all(data1$value == 0)){
    
    text = paste("No History Of Cholestrol.")
    dvchol_plot <- ggplot() + 
      annotate("text", x = 4, y = 25, size=6, label = text) + 
      theme_void()
    
  } else{
    
    dvchol_plot <- ggplot(data1, aes(x = date, y = value, color = description)) + 
      geom_line() + 
      theme_classic() + 
      ggtitle("Cholestrol") +
      labs(color='Type') +
      xlab("Date") +
      ylab("(mg/dL)") +
      scale_fill_discrete(labels=c("Control", "Treatment 1")) +
      theme(legend.key.size = unit(0.4, 'cm'))
  }
  
  dvchol_plot %>% ggplotly()
}