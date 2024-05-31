library(tidyverse)
library(scales) # for time in X axis (duration histograms)
library(Multilada)

export_date <- "20240411"
scr_ids <- pull(read_csv("scr_pl.csv", col_select = 1, col_types = "c"))

connection <- multilada_connect("sw")
sw_ids <- RMariaDB::dbGetQuery(connection, "SELECT DISTINCT child.child_hash FROM child, `user` WHERE user.family_id = child.family_id AND user.test IS NULL")[[1]]
RMariaDB::dbDisconnect(connection)

# PABIQ processing:

# To prepare import uncomment:
# fm_variables(paste0("PABIQSCRPL_", export_date, ".csv"), lang = "pl", target_file = "pabiq_labels_cdi3pl.csv")

pabiq_data <- fm_read(paste0("PABIQSCRPL_", export_date, ".csv"),
                      "pabiq_labels_cdi3pl.csv", "pabiq_translations_cdi3pl.csv", "pl")

#CLT database mailing was on 2023-09-05
pabiq_data %>% mutate(source = case_when(id %in% scr_ids ~ "screentime",
                                         id %in% sw_ids ~ "SW",
                                         submission_pabiq > "2023-09-04" ~ "norming")) -> pabiq_data
pabiq_data %>% filter(! is.na(source)) -> pabiq_data

#Calculate age:
pabiq_data$age <- age_months(pabiq_data$birth_date, pabiq_data$submission_pabiq)


## Voivodeships

pabiq_data$region <- str_to_title(pabiq_data$region)

# Only once, if there is no voivodeships.csv file yet:
# write_csv(as.data.frame(sort(unique(pabiq_data$region))), "voivodeships.csv", col_names = FALSE)

read_delim("voivodeships.csv", col_names = "region", delim = "\n") %>%
  separate_wider_delim(region, "\t",
                       names = c("region", "voivodeship"),
                       too_few = "align_start") -> voivodeships
if("voivodeship" %in% colnames(pabiq_data)) {
  pabiq_data %>% select(- voivodeship) %>%
    left_join(voivodeships) -> pabiq_data
} else {
  left_join(pabiq_data, voivodeships) -> pabiq_data
}

# After updating pabiq data csv file:
# pabiq_data %>% select(region, voivodeship) %>%
#   distinct(region, .keep_all = TRUE) %>%
#   arrange(region, .locale = "pl") %>%
#   write_delim(file = "voivodeships.csv", delim = "\t", na = "", quote = "needed", col_names = FALSE)
# Check voivodeships.csv and loop back to importing from it again.

# Once we are happy with corrections:
ifelse(is.na(pabiq_data$voivodeship) | pabiq_data$voivodeship == "",
       pabiq_data$region, pabiq_data$voivodeship) -> pabiq_data$voivodeship


## Filtering

# The procedure below filters out some SW submissions,
# which are obvious self-corrections,
# suggesting it's too strict!

#Potential scam:
pabiq_data %>% group_by(id) %>%
  summarise(n = n()) %>%
  filter(n > 1) -> potential_scam

#Too many potential siblings:
pabiq_data %>% group_by(id) %>%
  summarise(n = n()) %>%
  filter(n > 4) -> scam
pabiq_data %>% filter(! id %in% scam$id) -> pabiq_data

#Inconsistent voivodeship:
pabiq_data %>% group_by(id) %>%
  reframe(n = n_distinct(voivodeship), submission_date = submission_pabiq, id = id,
            voivodeship = voivodeship) %>% filter(n > 1) -> fake_voivodeship
pabiq_data %>% filter(! id %in% fake_voivodeship$id) -> pabiq_data

#Inconsistent education:
pabiq_data %>% group_by(id) %>%
  reframe(n = n_distinct(caregiver1_ed), submission_date = submission_pabiq, id = id,
          caregiver1_relation = caregiver1_relation, caregiver1_ed = caregiver1_ed) %>%
  filter(n > 1) -> fake_education
pabiq_data %>% filter(! id %in% fake_education$id) -> pabiq_data

#Non-siblings:
pabiq_data %>% group_by(id) %>%
  reframe(age_difference = interval(min(birth_date), max(birth_date)) / months(1),
          submission_date = submission_pabiq, id = id, birth_date = birth_date) %>%
  filter(age_difference > 0 & age_difference < 10) -> fake_siblings
pabiq_data %>% filter(! id %in% fake_siblings$id) -> pabiq_data

#Left-overs:
pabiq_data %>% group_by(id) %>%
  reframe(n = n(), id = id, submission_date = submission_pabiq, birth_date = birth_date,
          age = age, sex = sex, voivodeship = voivodeship, caregiver1_ed = caregiver1_ed) %>%
  filter(n > 1) -> left_overs

# Nie można CDI wypełnić dwa razy z tego samego IP.
# Co robimy z wypełnieniami, którym towarzyszą wielokrotnie wypełnione pabiqi?
# 1. Wykluczamy wszystkie takie CDI.
# 2. Wykluczamy tylko te CDI, dla których wielokrotnie wypełnione z tego samego IP
# pabiqi sugerują nierzetelność (patrz kod wyżej), a pozostałym
# dopasowujemy wypełnienie pabiqa na podstawie submission date.
# 3. Nie wykluczamy żadnych CDI i dopasowujemy wypełnienie pabiqa
# na podstawie submission date.

# Powyższy kod realizuje opcję nr 2!


# CDI processing:

bind_rows(
  cdi_read("cdi", "cdi3-scr_pl"),
  cdi_read("cdi", "cdi3_pl")) %>% filter(id %in% pabiq_data$id) -> cdi_responses

#Empty alternatives filtering
empty_alternatives <- cdi_count_checkboxAlt(cdi_responses, "alternatives", answer = "none") %>%
  mutate(empty_alt = n == 16) %>% filter(empty_alt) %>% select(id)
# Dziesięć takich wypełnień, odpowiedzi w pabiqu sugerują
# rzetelność co najmniej dwóch pierwszych (jeszcze do sprawdzenia kiedyś),
# ale potencjalne problemy zdrowotne/rozwojowe, więc i tak można wykluczyć:
cdi_responses %>% filter(! id %in% empty_alternatives) -> cdi_responses

cdi_submissions(cdi_responses) %>%
  left_join(pabiq_data) -> data

# data %>% mutate(submission_pabiq = submission_pabiq - hours(2)) ->  # The two servers have time difference...
#   data
data %>% mutate(gap_pabiq = start_date - submission_pabiq) -> data
data %>% group_by(id) %>% reframe(submission_date = submission_pabiq, start_date, gap_pabiq, n = n()) %>%
  filter(n > 1) %>% select(! n) -> multiple_pabiqs

data %>% group_by(id) %>%
  filter(gap_pabiq > 0) %>%
  filter(gap_pabiq == min(gap_pabiq)) -> data

left_join(data, cdi_count_oneCheckboxGroup(cdi_responses, "word")) %>%
  rename(score_lexicon = n) %>% select(-type) -> data
left_join(data, cdi_count_checkboxAlt(cdi_responses, "alternatives")) %>%
  rename(score_alternatives = n) %>% select(-c(type, answer)) -> data
left_join(data, cdi_count_radio(cdi_responses, "yesNo")) %>%
  rename(score_direct = n) %>% select(-c(type, answer)) -> data


# Obvious test submissions
# (tu w przyszłości zmienić na bardziej robust check:
# wszystkie id, które nie pasują do jednego z dwóch wzorów):
data %>% filter(caregiver1_relation != "Test Karo") -> data

# Filling time histogram:
# data %>% filter(duration < "1 hour") %>% ggplot() +
#   geom_histogram(aes(duration), binwidth = 30, boundary = 1) +
#   labs(x = "Duration in minutes", y = "Number of submissions") +
#   scale_x_time(breaks = breaks_width("2 min", offset = "1 min"), labels = label_time("%M"))

# Możemy przyjąć jako kryterium odcięcia najkrótszy czas wypełnienia w SW:
data %>% ungroup() %>% filter(source == "SW") %>% summarise(min(duration)) %>% pull() -> min_time
data %>% filter(duration >= min_time) -> data


fct_collapse(data$voivodeship,
             "południowy" = c("Małopolskie", "Śląskie"),
             "północno-zachodni" = c("Wielkopolskie", "Zachodniopomorskie", "Lubuskie"),
             "południowo-zachodni" = c("Dolnośląskie", "Opolskie"),
             "północny" = c("Kujawsko-Pomorskie", "Pomorskie", "Warmińsko-Mazurskie"),
             "centralny" = c("Łódzkie", "Świętokrzyskie"),
             "wschodni" = c("Lubelskie", "Podkarpackie", "Podlaskie"),
             "mazowiecki" = "Mazowieckie",
             other_level = "zagranica"
) -> data$macroregion

# W poniższym potwierdzić klasyfikację (głównie "zawodowe"):
fct_collapse(data$caregiver1_ed,
             "podstawowe" = c("podstawowe", "zawodowe"),
             "średnie" = c("średnie", "niepełne wyższe"),
             "wyższe" = c("wyższe", "doktorat")
) -> data$caregiver1_ed

capture.output(
  table(data$caregiver1_ed, data$city_town_countryside, data$macroregion),
  file = "cdi3_norm_counts.txt")


# Consent form processing:

# To prepare import uncomment:
# fm_variables(paste0("IRMIK3_", export_date, ".csv"), lang = "pl", target_file = "consent_labels_cdi3pl.csv")

fm_read(paste0("IRMIK3_", export_date, ".csv"),
        "consent_labels_cdi3pl.csv", lang = "pl") %>%
  filter(submission_consent > "2023-09-04") %>% left_join(data) %>%
  mutate(gap_consent = submission_pabiq - submission_consent) %>%
  group_by(id) %>% mutate(attempts = n(), another_attempt = gap_consent < 0) %>%
  select(1:4 | last_col(3) : last_col()) -> consent_data
consent_data %>% group_by(id) %>%
  filter(! another_attempt) %>% mutate(paired = gap_consent == min(gap_consent)) %>%
  right_join(consent_data) -> consent_data

write_csv(consent_data, "consent_submissions.csv", na = "")

## POWYŻEJ ZAKOŃCZYŁEM

## Sprawdzić, jak duże są potrzebne liczebności w poszczególnych kratkach,
## bo może nie trzeba się przejmować...

## Zamiast wykstałcenia matki bierzemy wykształcenie osoby wypełniającej.
## Czy chcemy kontrolować osobę wypełniającą?
