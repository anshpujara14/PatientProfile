sample_patient <- patients %>% 
  filter(patient == "dcaeb448-6a7d-4c64-9d43-0a9390d26013") %>% 
  mutate(email = "johndoe@xmail.com")

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
    set_text_color(col = 1:4, row = c(1, 4, 8), "grey") ->htt1 
    # merge_cells(2, 1:4) 
  
  
}


library(htmlTable)
htmlTable(htt1,
          align = paste(rep("l", ncol(htt1)), collapse = ""),
          rnames = FALSE, colnames = FALSE,
          css.cell = "")


# Allergies Table

allergies %>% 
  filter(PATIENT == 'dcaeb448-6a7d-4c64-9d43-0a9390d26013') -> allergies1

allergiesTable <- function(allergies1) {
  allergies1 <- allergies1 %>% 
    clean_names()
  
  ht2 <- tibble(allergies1$description)
  ht2
}

medications %>%
  clean_names() %>% 
  filter(patient == "dcaeb448-6a7d-4c64-9d43-0a9390d26013") -> medications1


prescriptionTable <- function(medications1){
  
  medications1 %>% 
    mutate(start = as.Date(as.character(start)), stop = as.Date(as.character(stop))) %>% 
    select(start, stop, description) %>% 
    arrange(start) -> ht3
  
  ht3 <- sapply(ht3, as.character)
  
  ht3[is.na(ht3)] <- ""
  ht3 <- ht3 %>% as.tibble()
}




ggplot(data.frame(x = factor(c(1:2, NA)), y = 1), aes(x, y)) +
  geom_point() +
  geom_blank() 


text = paste("No History Of Blood Pressure.")
ggplot() + 
  annotate("text", x = 4, y = 25, size=6, label = text) + 
  theme_void()

data


data1 <- observations %>% 
  clean_names() %>% 
  mutate(value = coalesce(as.numeric(value), 0), date = as.Date(as.character(date), format = "%m/%d/%Y")) %>% 
  filter(description == "Systolic Blood Pressure" | description == "Diastolic Blood Pressure", patient == "71949668-1c2e-43ae-ab0a-64654608defb") %>% 
  select(date, description, value)

ggplot(data1, aes(x = date, y = value, color = description)) + 
  geom_line() + 
  theme_classic() + 
  ggtitle("Blood Pressure") +
  labs(color='Type') +
  xlab("Date") +
  ylab("(mm/Hg)") +
  scale_fill_discrete(labels=c("Control", "Treatment 1")) +
  theme(legend.key.size = unit(0.4, 'cm'))
