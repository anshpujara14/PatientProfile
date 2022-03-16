
source('radar/dependencies.R')

lapply(required_packages, require, character.only = TRUE)

# DATA TRANSFORMATION AND NEW VARIABLES -----------------------------------

admission <- read_csv("radar/data/admissions.csv")
antimicrobial <- read_csv("radar/data/antimicrobials.csv")
microbiology <- read_csv("radar/data/microbiology.csv")

admission <- admission %>%
  mutate(year = year(adm_start_date),
         yearmonth_adm = as.yearmon(adm_start_date),
         yearquarter_adm = as.yearqtr(adm_start_date),
         LOS = as.integer(as.Date(adm_end_date) - as.Date(adm_start_date) + 1),
         age = as.integer(year(adm_start_date) - year(birth_date)))

# antimicrobial 

antimicrobial <- antimicrobial %>% 
  mutate_at(vars(contains("date")), as.Date)


# microbiology

microbiology <- microbiology %>%
  mutate_if(is.rsi.eligible, as.rsi) %>%
  mutate(mo = as.mo(mo)) %>%
  left_join(microorganisms %>% select(mo, fullname, family)) %>%
  mutate(yearmonth_test = as.yearmon(test_date),
         yearquarter_test = as.yearqtr(test_date))

microbiology <- microbiology %>%
  mutate(first_isolate = first_isolate(., col_mo = "mo", col_date = "test_date",col_patient_id = "id", col_specimen = "material"))


# Join datasets by overlaping time intervals

pat <- admission %>% as.data.table()
anti <- antimicrobial %>% as.data.table()
micro <- microbiology %>% as.data.table()

anti <- anti[pat, on = .(id, as.Date(ab_start_date) >= as.Date(adm_start_date), as.Date(ab_stop_date) <= as.Date(adm_end_date)), .(id, adm_id, as.Date(ab_start_date) = as.Date(x.ab_start_date), ab_stop_date = x.ab_stop_date, adm_start_date, adm_end_date, atc_code, ddd_per_day, ab_route), nomatch = 0L]

micro <- micro[
  pat,
  on = .(id, test_date >= adm_start_date, test_date <= adm_end_date),
  .(id, adm_id, test_date = x.test_date, test_number, adm_start_date, adm_end_date, material),
  nomatch = 0L
  ]

anti_first <- anti %>%
  group_by(id, adm_id) %>%
  summarise(min_ab_start = min(ab_start_date)) # first prescription date

bc_timing <- micro %>%
  left_join(anti_first) %>%
  filter(material == "blood") %>% 
  mutate(bc_timing = as.integer(test_date - min_ab_start)) %>%
  group_by(id, adm_id) %>%
  filter(bc_timing == min(bc_timing, na.rm = FALSE)) %>%
  ungroup() %>%
  select(id, adm_id, bc_timing) %>%
  distinct()

uc_timing <- micro %>%
  left_join(anti_first) %>%
  filter(material == "urine") %>% 
  mutate(uc_timing = as.integer(test_date - min_ab_start)) %>%
  group_by(id, adm_id) %>%
  filter(uc_timing == min(uc_timing, na.rm = FALSE)) %>%
  ungroup() %>%
  select(id, adm_id, uc_timing) %>%
  distinct()

admissions <- admissions %>%
  left_join(bc_timing) %>% 
  left_join(uc_timing)

microbiology <- microbiology %>%
  semi_join(micro)

microbiology <- microbiology %>%
  left_join(micro) %>%
  select(-c(specialty)) %>%
  left_join(admissions)

antimicrobial <- antimicrobial %>%
  semi_join(anti) %>%
  left_join(admissions)

antimicrobial <- antimicrobial %>%
  mutate(ab_days = as.integer(ab_stop_date - ab_start_date),
         ab_timing = as.integer(ab_start_date - adm_start_date),
         ddd_per_prescription = ddd_per_day*ab_days) %>%
  left_join(
    AMR::antibiotics %>%
      select(
        atc_code = atc, ab_type = name, ab_group = atc_group2
      ), by = "atc_code") %>%
  group_by(id, adm_id) %>%
  mutate(ddd_total = sum(ddd_per_prescription, na.rm = TRUE),
         ab_first = if_else(ab_start_date == min(ab_start_date, na.rm = TRUE), TRUE, FALSE)) %>%
  ungroup()

continuous_treatment_duration <- antimicrobial %>% as.data.table()

continuous_treatment_duration <-
  continuous_treatment_duration[, {
    ind <- rleid((ab_start_date - shift(ab_stop_date, fill = Inf)) > 0) == 1
    .(ab_start_cont = min(ab_start_date[ind]),
      ab_stop_cont  = max(ab_stop_date[ind]))}
    , by = c("id", "adm_id")] %>%
  .[, ab_days_all := as.integer(ab_stop_cont - ab_start_cont + 1)]

antimicrobial <- antimicrobial %>%
  left_join(continuous_treatment_duration)



# antimicrobial count for select input in ui.R

ab <- antimicrobial %>%
  filter(!is.na(ab_type)) %>%
  group_by(ab_type) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  filter(!is.na(ab_type)) %>%
  distinct()

ab_groups <- antimicrobial %>%
  filter(!is.na(ab_group)) %>%
  select(ab_group) %>%
  arrange(ab_group) %>%
  distinct()


update_ab <- antimicrobial %>%
  select(ab_type, ab_group) %>%
  distinct(.keep_all = TRUE)


# HELP & INTRO DATA ---------------------------------------------------------------

steps <- read.csv("radar/help.csv")
intro <- read.csv("radar/intro.csv")


# FLUID DESIGN FUNCTION ---------------------------------------------------

fluid_design <- function(id, w, x, y, z) {
  fluidRow(
    div(
      id = id,
      column(
        width = 6,
        uiOutput(w),
        uiOutput(y)
      ),
      column(
        width = 6,
        uiOutput(x),
        uiOutput(z)
      )
    )
  )
}
