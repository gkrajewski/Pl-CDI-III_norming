library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(data.table)
library(Multilada)


# PABIQ processing:

#Wybierz plik z danymi:
pabiq_data <- read.csv("PABIQSCRPL_20231207.csv")

#Wybierz plik z kluczami:
read.csv("pabiq_labels_cdi3pl.csv") %>%
  filter(Label != "") %>%
  filter(If.needed != "") -> keys

#Wybierz plik z tłumaczeniami
trans <- read.csv("pabiq_translations.csv")

#Wybierz kolumnę odpowiadającą właściwemu językowi:
trans <- trans %>% select(Translation, pl)
trans <- trans %>% rename(x = pl)

#Zmiana nazw kolumn
pabiq_data <- setnames(pabiq_data, keys$pl, keys$Label, skip_absent = T)
pabiq_data <- pabiq_data %>% select(any_of(keys$Label))

#Zmiana słów wewnątrz tabeli na tłumaczenia
trans <- trans %>% filter(trans$x != "")
for (i in 1:nrow(trans)){
  pabiq_data[pabiq_data == trans$x[i]] <- trans$Translation[i]
}

#Type conversion

for(type in keys$Type.import) {
  if(type == "") next
  if(substr(type, 1, 1) == "%") {
    pabiq_data[keys[keys$Type.import == type, "Label"]] <- lapply(
      pabiq_data[keys[keys$Type.import == type, "Label"]], function(x) as.Date(x, format=type)
    )
  } else {
    pabiq_data[keys[keys$Type.import == type, "Label"]] <- lapply(
      pabiq_data[keys[keys$Type.import == type, "Label"]], paste0("as.", type)
    )
  }
}


# ID selection:

cdi_submissions(cdi_read("cdi", "cdi3-scr_pl")) -> cdi_data

# CLT database mailing was on 2023-09-05
pabiq_data %>% filter(submission_date > "2023-09-04") %>%
  inner_join(cdi_data) -> data

# Norming sample variables:

## Age:
data$age <- age_months(data$birth_date, data$submission_date)

## Voivodeship
read_delim("voivodeships.csv", col_names = "region", delim = "\n") %>%
  separate_wider_delim(region, "\t",
                       names = c("region", "voivodeship"),
                       too_few = "align_start") -> voivodeships
data$region <- str_to_title(data$region)
left_join(data, voivodeships) -> data
data %>% select(region, voivodeship) %>%
  distinct(region, .keep_all = TRUE) %>%
  arrange(region) %>%
  unite("x", region, voivodeship, sep = "\t")
  write_delim(file = "regions.txt", delim = "\t", na = "", col_names = FALSE)
ifelse(is.na(data$voivodeship), data$region, data$voivodeship) -> data$voivodeship

write_csv(sort(unique(data$region)), "regions.txt")



table(data$village_city)
table(data$sex, data$age)

# Should we consider mother or the person filling in?
data$guardian1_relation
table(data$guardian1_ed)
