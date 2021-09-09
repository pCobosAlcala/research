### Paquetes ----
pacman::p_load(tidyverse, scales, ggrepel, ggtext, kableExtra)

## Setup
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Mac
Sys.setlocale("LC_ALL", "Spanish_Mexico.1252") # Windows
options(scipen=999) 

## Eliminar objetos
rm(list = ls())


### Datos----
base <- readxl::read_excel("1_data/db.xlsx", col_types = c("text", 
                                                           "text", "text", "text", "text", "text", 
                                                           "text", "date", "date", "date", "numeric", 
                                                           "numeric", "numeric", "numeric", "numeric", 
                                                           "numeric", "text", "numeric", "numeric", 
                                                           "text", "numeric", "text", "text", "text", 
                                                           "text", "text", "text", "text", "text", 
                                                           "text", "text", "text", "text", "text", 
                                                           "text", "text", "text"))


base = base %>% 
  mutate(count = str_replace_all(country, c("Trinidad and Tobago" = "Trinidad y Tobago",
                                            "Suriname" = "Surinam",
                                            "Saint Vincent and the Grenadines" = "San Vicente",
                                            "Saint Lucia" = "Santa Lucía", 
                                            "Saint Christopher and Nevis" = "San Cristóbal y Nieves",
                                            "Peru" = "Perú",
                                            "Panama" = "Panamá", 
                                            "Mexico" = "México",
                                            "Haiti" = "Haití",
                                            "Grenada" = "Granada", 
                                            "Dominican Republic" = "República Dominicana",
                                            "Brazil" = "Brasil",
                                            "Belize" = "Belice",
                                            "Antigua and Barbuda" = "Antigua y Barbuda"))) 


latam <- readxl::read_excel("1_data/db.xlsx", sheet = "countries") %>% 
  mutate(count = str_replace_all(country, c("Trinidad and Tobago" = "Trinidad y Tobago",
                                            "Suriname" = "Surinam",
                                            "Saint Vincent and the Grenadines" = "San Vicente",
                                            "Saint Lucia" = "Santa Lucía", 
                                            "Saint Christopher and Nevis" = "San Cristóbal y Nieves",
                                            "Peru" = "Perú",
                                            "Panama" = "Panamá", 
                                            "Mexico" = "México",
                                            "Haiti" = "Haití",
                                            "Grenada" = "Granada", 
                                            "Dominican Republic" = "República Dominicana",
                                            "Brazil" = "Brasil",
                                            "Belize" = "Belice",
                                            "Antigua and Barbuda" = "Antigua y Barbuda"))) %>% 
  mutate(a = str_replace_all(count, c( "Venezuela" = "**Venezuela**",
                                       "Uruguay" = "**Uruguay**",
                                       "Trinidad y Tobago" = "**Trinidad y Tobago**",
                                       "Surinam" = "**Surinam**",
                                       "Perú" = "**Perú**",
                                       "Paraguay" = "**Paraguay**",
                                       "Panamá" = "**Panamá**", 
                                       "México" = "**México**",
                                       "Haití" = "**Haití**",
                                       "Jamaica" = "**Jamaica**",
                                       "Honduras" = "**Honduras**",
                                       "Guyana" = "**Guyana**",
                                       "Guatemala" = "**Guatemala**",
                                       "El Salvador" = "**El Salvador**",
                                       "Ecuador" = "**Ecuador**",
                                       "República Dominicana" = "**República Dominicana**",
                                       "Costa Rica" = "**Costa Rica**",
                                       "Colombia" = "**Colombia**",
                                       "Chile" = "**Chile**",
                                       "Brasil" = "**Brasil**",
                                       "Bolivia" = "**Bolivia**",
                                       "Belice" = "**Belice**",
                                       "Barbados" = "**Barbados**",
                                       "Bahamas" = "**Bahamas**",
                                       "Argentina" = "**Argentina**")))


db = base %>% 
  left_join (latam, by = "count") %>% 
  mutate(country = str_replace_all(count, c( "Venezuela" = "**Venezuela**",
                                             "Uruguay" = "**Uruguay**",
                                             "Trinidad y Tobago" = "**Trinidad y Tobago**",
                                             "Surinam" = "**Surinam**",
                                             "Perú" = "**Perú**",
                                             "Paraguay" = "**Paraguay**",
                                             "Panamá" = "**Panamá**", 
                                             "México" = "**México**",
                                             "Haití" = "**Haití**",
                                             "Jamaica" = "**Jamaica**",
                                             "Honduras" = "**Honduras**",
                                             "Guyana" = "**Guyana**",
                                             "Guatemala" = "**Guatemala**",
                                             "El Salvador" = "**El Salvador**",
                                             "Ecuador" = "**Ecuador**",
                                             "República Dominicana" = "**República Dominicana**",
                                             "Costa Rica" = "**Costa Rica**",
                                             "Colombia" = "**Colombia**",
                                             "Chile" = "**Chile**",
                                             "Brasil" = "**Brasil**",
                                             "Bolivia" = "**Bolivia**",
                                             "Belice" = "**Belice**",
                                             "Barbados" = "**Barbados**",
                                             "Bahamas" = "**Bahamas**",
                                             "Argentina" = "**Argentina**")))

nic <- tibble(country = c("Nicaragua"),
              count = c("Nicaragua"))

nic2 <- tibble(country = c("Nicaragua"),
               horizontal = 0,
               new = 0,
               vertical = 0)

names = db %>% 
  select(country, count) %>% 
  rbind(nic)

## Análisis G1:G3----

glimpse(base)

# Número de programas
nrow(base)

# Número de países
base %>% 
  count(country) %>% 
  print(n = Inf)


# Tipo de innovación
db %>% 
  count(type_of_innovation)

# Países por tipo de innovación
db %>% 
  add_row(country = "Nicaragua") %>% 
  count(country, type_of_innovation) %>% 
  count(type_of_innovation) %>% 
  arrange(-n)

# Tipos de innovación por países
db %>% 
  add_row(country = "Nicaragua") %>%
  count(country, type_of_innovation) %>% 
  count(country) %>% 
  summarise(mean = mean(n))

# Tipo de beneficios
base %>% 
  count(type_of_measure)

# Países por tipos de beneficios
db %>% 
  add_row(country = "Nicaragua") %>% 
  count(country, type_of_measure) %>% 
  count(type_of_measure) %>% 
  arrange(-n)

# Medidas por país
db %>% 
  filter(type_of_measure == "cash transfer") %>% 
  add_row(country = "Nicaragua") %>%
  count(country) %>% 
  summarise(mean = mean(n))


# Tipos de beneficios por país
db %>% 
  add_row(country = "Nicaragua") %>%
  count(country, type_of_measure) %>% 
  count(country) %>% arrange(-n) %>% 
  print(n = Inf)

# Transferencias por tipo de innovación
db %>% 
  filter(type_of_measure == "cash transfer") %>% 
  count(type_of_innovation) %>% 
  arrange(-n)

# Países por transferencias por tipo de innovación
db %>% 
  filter(type_of_measure == "cash transfer") %>% 
  add_row(country = "Nicaragua") %>% 
  count(country, type_of_innovation) %>% 
  count(type_of_innovation) %>% 
  arrange(-n)

# Transferencias por país
db %>% 
  filter(type_of_measure == "cash transfer") %>% 
  add_row(country = "Nicaragua") %>%
  count(country) %>% 
  summarise(mean = mean(n))


## Análisis G4----
# Cobertura promedio entre países

db %>% 
  # filter(type_of_measure == "cash transfer") %>%
  mutate(coverage_precovid = case_when(is.na(coverage_precovid) ~ 0,
                                       T ~ coverage_precovid)) %>% 
  mutate(coverage_covid_announced = case_when(is.na(coverage_covid_announced) & is.numeric(coverage_covid_reached) ~ coverage_covid_reached,
                                              T ~ coverage_covid_announced)) %>% 
  mutate(coverage = coverage_covid_announced - coverage_precovid,
         cov_porc = (coverage_covid_announced - coverage_precovid)/population) %>% 
  filter(coverage != is.na(coverage),
         cov_porc != is.na(cov_porc)) %>% 
  group_by(country) %>% 
  mutate(ord = sum(cov_porc)) %>% 
  ungroup() %>% 
  add_row(country = "Nicaragua") %>% 
  mutate(ord = case_when(country == "Nicaragua" ~ 0,
                         T ~ ord)) %>% 
  # summarise(ord = mean(ord))
  summarise(ord = mean(ord)) 


# Cobertura promedio por país
db %>% 
  # filter(type_of_measure == "cash transfer") %>%
  mutate(coverage_precovid = case_when(is.na(coverage_precovid) ~ 0,
                                       T ~ coverage_precovid)) %>% 
  mutate(coverage_covid_announced = case_when(is.na(coverage_covid_announced) & is.numeric(coverage_covid_reached) ~ coverage_covid_reached,
                                              T ~ coverage_covid_announced)) %>% 
  mutate(coverage = coverage_covid_announced - coverage_precovid,
         cov_porc = (coverage_covid_announced - coverage_precovid)/population) %>% 
  filter(coverage != is.na(coverage),
         cov_porc != is.na(cov_porc)) %>% 
  group_by(country) %>% 
  mutate(ord = sum(cov_porc)) %>% 
  ungroup() %>% 
  group_by(country) %>% 
  summarise(ord = mean(ord)) %>% 
  arrange(-ord) %>% print(n = Inf)


# Cobertura promedio de la región
db %>% 
  # filter(type_of_measure == "cash transfer") %>%
  # filter(type_of_measure %in% c( "cash transfer", "voucher")) %>%
  mutate(coverage_precovid = case_when(is.na(coverage_precovid) ~ 0,
                                       T ~ coverage_precovid)) %>% 
  mutate(coverage_covid_announced = case_when(is.na(coverage_covid_announced) & is.numeric(coverage_covid_reached) ~ coverage_covid_reached,
                                              T ~ coverage_covid_announced)) %>% 
  mutate(coverage = coverage_covid_announced - coverage_precovid,
         cov_porc = (coverage_covid_announced - coverage_precovid)/population) %>% 
  filter(coverage != is.na(coverage),
         cov_porc != is.na(cov_porc)) %>% 
  summarise(cov = sum(coverage))


latam %>% 
  summarise(a = sum(population, na.rm = T))

# % población beneficiada en la región
210366512/642654750


# % de beneficios que fueron CTP o voucher
158225854/210366512

# % de beneficios que fueron CTP
148636249/210366512


# % población beneficiada con CTP o voucher
158225854/642654750

# % población beneficiada con CTP
148636249/642654750


latam %>% 
  summarise(pop = sum(population, na.rm = T))

148636249/642654750

210410926/642654750


# Cobertura promedio de cash transfer y voucher
db %>% 
  filter(type_of_measure %in% c( "cash transfer", "voucher")) %>% 
  mutate(coverage_precovid = case_when(is.na(coverage_precovid) ~ 0,
                                       T ~ coverage_precovid)) %>% 
  mutate(coverage_covid_announced = case_when(is.na(coverage_covid_announced) & is.numeric(coverage_covid_reached) ~ coverage_covid_reached,
                                              T ~ coverage_covid_announced)) %>% 
  mutate(coverage = coverage_covid_announced - coverage_precovid,
         cov_porc = (coverage_covid_announced - coverage_precovid)/population) %>% 
  filter(coverage != is.na(coverage),
         cov_porc != is.na(cov_porc)) %>% 
  group_by(country) %>% 
  mutate(ord = sum(cov_porc)) %>% 
  ungroup() %>% 
  summarise(ord = mean(ord))


## Análisis G5----

# Costo entre población
db %>% 
  filter(cost != is.na(cost)) %>% 
  group_by(country) %>% 
  summarise(a = sum(cost)/population) %>% 
  ungroup() %>% 
  count(country, a) %>% 
  arrange(a) %>% 
  select(-n) %>% 
  print(n = Inf)


db %>% 
  filter(cost != is.na(cost)) %>% 
  group_by(country) %>% 
  summarise(a = sum(cost)/population) %>% 
  ungroup() %>% 
  count(country, a) %>% 
  arrange(a) %>% 
  select(-n) %>% 
  summarise(a = mean(a))

# Costo entre población beneficiada
db %>% 
  mutate(coverage_precovid = case_when(is.na(coverage_precovid) ~ 0,
                                       T ~ coverage_precovid)) %>% 
  mutate(coverage_covid_announced = case_when(is.na(coverage_covid_announced) & is.numeric(coverage_covid_reached) ~ coverage_covid_reached,
                                              T ~ coverage_covid_announced)) %>% 
  mutate(coverage = coverage_covid_announced - coverage_precovid,
         cov_porc = (coverage_covid_announced - coverage_precovid)/population) %>% 
  filter(coverage != is.na(coverage),
         cov_porc != is.na(cov_porc)) %>% 
  group_by(country)  %>% 
  select(program_name, country, coverage, cost) %>% 
  group_by(country) %>% 
  summarise(cov = sum(coverage, na.rm = T),
            cost = sum(cost, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(monto = cost/cov) %>% 
  arrange(-monto) %>% 
  add_row(country = "Nicaragua")  %>% 
  mutate(monto = case_when(country == "Nicaragua" ~ 0,
                           T ~ monto)) %>% 
  arrange(-monto) %>% 
  select(country, monto, cov, cost) %>% 
  print(n = Inf)

db %>% 
  mutate(coverage_precovid = case_when(is.na(coverage_precovid) ~ 0,
                                       T ~ coverage_precovid)) %>% 
  mutate(coverage_covid_announced = case_when(is.na(coverage_covid_announced) & is.numeric(coverage_covid_reached) ~ coverage_covid_reached,
                                              T ~ coverage_covid_announced)) %>% 
  mutate(coverage = coverage_covid_announced - coverage_precovid,
         cov_porc = (coverage_covid_announced - coverage_precovid)/population) %>% 
  filter(coverage != is.na(coverage),
         cov_porc != is.na(cov_porc)) %>% 
  group_by(country)  %>% 
  select(program_name, country, coverage, cost) %>% 
  group_by(country) %>% 
  summarise(cov = sum(coverage, na.rm = T),
            cost = sum(cost, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(monto = cost/cov) %>% 
  arrange(-monto) %>% 
  add_row(country = "Nicaragua")  %>% 
  mutate(monto = case_when(country == "Nicaragua" ~ 0,
                           T ~ monto)) %>% 
  arrange(-monto) %>% 
  select(country, monto, cov, cost) %>% 
  summarise(mean = mean(monto))


## Análisis G6 y G7----

db %>% 
  # filter(type_of_measure %in% c("cash transfer", "voucher")) %>% 
  mutate_at(vars(new_info_deliver:previous_info_select), ~replace(., is.na(.), "ND")) %>% 
  mutate(deliver = case_when(new_info_deliver == "Yes" & previous_info_deliver == "Yes" ~ "Both",
                             new_info_deliver == "Yes" & previous_info_deliver == "ND" ~ "New",
                             new_info_deliver == "Yes" & previous_info_deliver == "No" ~ "New",
                             new_info_deliver == "ND" & previous_info_deliver == "Yes" ~ "Previous",
                             new_info_deliver == "ND" & previous_info_deliver == "ND" ~ "ND",
                             new_info_deliver == "ND" & previous_info_deliver == "No" ~ "ND",
                             new_info_deliver == "No" & previous_info_deliver == "Yes" ~ "Previous",
                             new_info_deliver == "No" & previous_info_deliver == "ND" ~ "ND",
                             new_info_deliver == "No" & previous_info_deliver == "No" ~ "ND")) %>% 
  mutate(select = case_when(new_information_select == "Yes" & previous_info_select == "Yes" ~ "Both",
                            new_information_select == "Yes" & previous_info_select == "ND" ~ "New",
                            new_information_select == "Yes" & previous_info_select == "No" ~ "New",
                            new_information_select == "ND" & previous_info_select == "Yes" ~ "Previous",
                            new_information_select == "ND" & previous_info_select == "ND" ~ "ND",
                            new_information_select == "ND" & previous_info_select == "No" ~ "ND",
                            new_information_select == "No" & previous_info_select == "Yes" ~ "Previous",
                            new_information_select == "No" & previous_info_select == "ND" ~ "ND",
                            new_information_select == "No" & previous_info_select == "No" ~ "ND")) %>% 
  count(deliver)

db %>% 
  mutate_at(vars(new_info_deliver:previous_info_select), ~replace(., is.na(.), "ND")) %>% 
  mutate(deliver = case_when(new_info_deliver == "Yes" & previous_info_deliver == "Yes" ~ "Both",
                             new_info_deliver == "Yes" & previous_info_deliver == "ND" ~ "New",
                             new_info_deliver == "Yes" & previous_info_deliver == "No" ~ "New",
                             new_info_deliver == "ND" & previous_info_deliver == "Yes" ~ "Previous",
                             new_info_deliver == "ND" & previous_info_deliver == "ND" ~ "ND",
                             new_info_deliver == "ND" & previous_info_deliver == "No" ~ "ND",
                             new_info_deliver == "No" & previous_info_deliver == "Yes" ~ "Previous",
                             new_info_deliver == "No" & previous_info_deliver == "ND" ~ "ND",
                             new_info_deliver == "No" & previous_info_deliver == "No" ~ "ND")) %>% 
  mutate(coverage_precovid = case_when(is.na(coverage_precovid) ~ 0,
                                       T ~ coverage_precovid)) %>% 
  mutate(select = case_when(new_information_select == "Yes" & previous_info_select == "Yes" ~ "Both",
                            new_information_select == "Yes" & previous_info_select == "ND" ~ "New",
                            new_information_select == "Yes" & previous_info_select == "No" ~ "New",
                            new_information_select == "ND" & previous_info_select == "Yes" ~ "Previous",
                            new_information_select == "ND" & previous_info_select == "ND" ~ "ND",
                            new_information_select == "ND" & previous_info_select == "No" ~ "ND",
                            new_information_select == "No" & previous_info_select == "Yes" ~ "Previous",
                            new_information_select == "No" & previous_info_select == "ND" ~ "ND",
                            new_information_select == "No" & previous_info_select == "No" ~ "ND")) %>% 
  mutate(coverage_covid_announced = case_when(is.na(coverage_covid_announced) & is.numeric(coverage_covid_reached) ~ coverage_covid_reached,
                                              T ~ coverage_covid_announced)) %>%
  mutate(coverage = coverage_covid_announced - coverage_precovid,
         cov_porc = (coverage_covid_announced - coverage_precovid)/population) %>% 
  filter(coverage != is.na(coverage),
         cov_porc != is.na(cov_porc)) %>% 
  group_by(country) %>% 
  mutate(ord = sum(cov_porc)) %>% 
  ungroup() %>% 
  select(country.x, cov_porc, select, deliver) %>% 
  count(country.x, deliver) %>% 
  filter(deliver %in% c("Previous", "Both")) %>% 
  count(country.x)


db %>% 
  mutate_at(vars(new_info_deliver:previous_info_select), ~replace(., is.na(.), "ND")) %>% 
  mutate(deliver = case_when(new_info_deliver == "Yes" & previous_info_deliver == "Yes" ~ "Both",
                             new_info_deliver == "Yes" & previous_info_deliver == "ND" ~ "New",
                             new_info_deliver == "Yes" & previous_info_deliver == "No" ~ "New",
                             new_info_deliver == "ND" & previous_info_deliver == "Yes" ~ "Previous",
                             new_info_deliver == "ND" & previous_info_deliver == "ND" ~ "ND",
                             new_info_deliver == "ND" & previous_info_deliver == "No" ~ "ND",
                             new_info_deliver == "No" & previous_info_deliver == "Yes" ~ "Previous",
                             new_info_deliver == "No" & previous_info_deliver == "ND" ~ "ND",
                             new_info_deliver == "No" & previous_info_deliver == "No" ~ "ND")) %>% 
  mutate(coverage_precovid = case_when(is.na(coverage_precovid) ~ 0,
                                       T ~ coverage_precovid)) %>% 
  mutate(select = case_when(new_information_select == "Yes" & previous_info_select == "Yes" ~ "Both",
                            new_information_select == "Yes" & previous_info_select == "ND" ~ "New",
                            new_information_select == "Yes" & previous_info_select == "No" ~ "New",
                            new_information_select == "ND" & previous_info_select == "Yes" ~ "Previous",
                            new_information_select == "ND" & previous_info_select == "ND" ~ "ND",
                            new_information_select == "ND" & previous_info_select == "No" ~ "ND",
                            new_information_select == "No" & previous_info_select == "Yes" ~ "Previous",
                            new_information_select == "No" & previous_info_select == "ND" ~ "ND",
                            new_information_select == "No" & previous_info_select == "No" ~ "ND")) %>% 
  mutate(coverage_covid_announced = case_when(is.na(coverage_covid_announced) & is.numeric(coverage_covid_reached) ~ coverage_covid_reached,
                                              T ~ coverage_covid_announced)) %>%
  mutate(coverage = coverage_covid_announced - coverage_precovid,
         cov_porc = (coverage_covid_announced - coverage_precovid)/population) %>% 
  filter(coverage != is.na(coverage),
         cov_porc != is.na(cov_porc)) %>% 
  group_by(country) %>% 
  mutate(ord = sum(cov_porc)) %>% 
  ungroup() %>% 
  select(country.x, coverage, select, deliver) %>% 
  group_by(select) %>% 
  summarise(cov_porc = sum(coverage)) %>% 
  ungroup() %>% 
  mutate(total = sum(cov_porc)) %>% 
  mutate(porc = cov_porc/total)

# Select
0.549  + 0.395 

# Deliver
0.440 +  0.445  

## Análisis G costo----

#Costo por país

db %>% 
  filter(cost != is.na(cost)) %>% 
  mutate(cost_porc = (cost/population)/gdp_percap) %>% 
  group_by(country) %>% 
  mutate(ord = sum(cost_porc)) %>% 
  ungroup() %>% 
  add_row(country = "Nicaragua") %>% 
  mutate(ord = case_when(country == "Nicaragua" ~ 0,
                         T ~ ord)) %>% 
  mutate(cost_porc = case_when(country == "Nicaragua" ~ 0,
                               T ~ cost_porc)) %>% 
  mutate(type_of_measure = case_when(country == "Nicaragua" ~ "voucher",
                                     T ~ type_of_measure)) %>% 
  group_by(country) %>% 
  summarise(a = sum(cost_porc),
            a = percent(a, accuracy = .0001)) %>% 
  arrange(a) %>% 
  print(n = Inf)


# Costo promedio

db %>% 
  filter(cost != is.na(cost)) %>% 
  mutate(cost_porc = (cost/population)/gdp_percap) %>% 
  group_by(country) %>% 
  mutate(ord = sum(cost_porc)) %>% 
  ungroup() %>% 
  add_row(country = "Nicaragua") %>% 
  mutate(ord = case_when(country == "Nicaragua" ~ 0,
                         T ~ ord)) %>% 
  mutate(cost_porc = case_when(country == "Nicaragua" ~ 0,
                               T ~ cost_porc)) %>% 
  mutate(type_of_measure = case_when(country == "Nicaragua" ~ "voucher",
                                     T ~ type_of_measure)) %>% 
  group_by(country) %>% 
  summarise(a = sum(cost_porc)) %>% 
  ungroup() %>% 
  summarise(a = mean(a))


# Duración

db %>%
  filter(type_of_measure == "cash transfer") %>% 
  filter(periodicity != "Daily",
         periodicity != "Weekly",
         periodicity != is.na(periodicity),
         periodicity != "Biweekly") %>% 
  summarise(a = mean(number_transfers, na.rm = T),
            b = median(number_transfers, na.rm = T))

# Monto promedio (considerando duración)
db %>% 
  mutate(a = case_when(is.na(amount_individual_covid) ~ 1,
                       T ~ 0)) %>% 
  filter(a == 0) %>% 
  filter(number_transfers != is.na(number_transfers)) %>% 
  mutate(amount_individual_precovid = case_when(is.na( amount_individual_precovid) ~ 0,
                                                T ~  amount_individual_precovid)) %>% 
  mutate(amount_individual_covid = (amount_individual_covid -  amount_individual_precovid)*number_transfers) %>% 
  group_by(country) %>% 
  summarise(a = mean(amount_individual_covid)) %>%
  arrange(-a) %>% 
  print(n = Inf)

db %>% 
  mutate(a = case_when(is.na(amount_individual_covid) ~ 1,
                       T ~ 0)) %>% 
  filter(a == 0) %>% 
  filter(number_transfers != is.na(number_transfers)) %>% 
  mutate(amount_individual_precovid = case_when(is.na( amount_individual_precovid) ~ 0,
                                                T ~  amount_individual_precovid)) %>% 
  mutate(amount_individual_covid = (amount_individual_covid -  amount_individual_precovid)*number_transfers) %>% 
  summarise(a = median(amount_individual_covid))

db %>% 
  mutate(a = case_when(is.na(amount_individual_covid) ~ 1,
                       T ~ 0)) %>% 
  filter(a == 0) %>% 
  filter(number_transfers != is.na(number_transfers)) %>% 
  
  mutate( amount_individual_precovid = case_when(is.na( amount_individual_precovid) ~ 0,
                                                 T ~  amount_individual_precovid)) %>% 
  mutate(amount_individual_covid = (amount_individual_covid -  amount_individual_precovid)*number_transfers) %>% 
  group_by(country) %>% 
  summarise(a = mean(amount_individual_covid)) %>% 
  ungroup()  %>% 
  arrange(a) %>% 
  print(n = Inf)


# Monto promedio (considerando apoyo único)
db %>% 
  mutate(a = case_when(is.na(amount_individual_covid) ~ 1,
                       T ~ 0)) %>% 
  filter(a == 0) %>% 
  mutate( amount_individual_precovid = case_when(is.na( amount_individual_precovid) ~ 0,
                                                 T ~  amount_individual_precovid)) %>% 
  mutate(amount_individual_covid = amount_individual_covid -  amount_individual_precovid) %>% 
  summarise(a = mean(amount_individual_covid))


db %>% 
  mutate(a = case_when(is.na(amount_individual_covid) ~ 1,
                       T ~ 0)) %>% 
  filter(a == 0) %>% 
  mutate( amount_individual_precovid = case_when(is.na( amount_individual_precovid) ~ 0,
                                                 T ~  amount_individual_precovid)) %>% 
  mutate(amount_individual_covid = amount_individual_covid -  amount_individual_precovid) %>% 
  summarise(a = median(amount_individual_covid))

db %>% 
  mutate(a = case_when(is.na(amount_individual_covid) ~ 1,
                       T ~ 0)) %>% 
  filter(a == 0) %>% 
  mutate( amount_individual_precovid = case_when(is.na( amount_individual_precovid) ~ 0,
                                                 T ~  amount_individual_precovid)) %>% 
  mutate(amount_individual_covid = amount_individual_covid -  amount_individual_precovid) %>% 
  group_by(country) %>% 
  summarise(a = mean(amount_individual_covid)) %>% 
  arrange(a) %>%
  print(n = Inf)


# Montos por tipo de medida (sin duración)
db %>% 
mutate(coverage_precovid = case_when(is.na(coverage_precovid) ~ 0,
                                     T ~ coverage_precovid)) %>% 
  mutate(coverage_covid_announced = case_when(is.na(coverage_covid_announced) & is.numeric(coverage_covid_reached) ~ coverage_covid_reached,
                                              T ~ coverage_covid_announced)) %>% 
  mutate(coverage = coverage_covid_announced - coverage_precovid,
         cov_porc = (coverage_covid_announced - coverage_precovid)/population) %>% 
  filter(coverage != is.na(coverage),
         cov_porc != is.na(cov_porc)) %>% 
  group_by(country)  %>% 
  select(program_name, country, coverage, cost) %>% 
  group_by(country) %>% 
  summarise(cov = sum(coverage, na.rm = T),
            cost = sum(cost, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(monto = cost/cov) %>% 
  arrange(-monto) %>% print(n = Inf)


# Montos por tipo de medida (con duración)
db %>% 
  mutate(a = case_when(is.na(amount_individual_covid) ~ 1,
                       T ~ 0)) %>% 
  filter(a == 0) %>% 
  filter(number_transfers != is.na(number_transfers)) %>% 
  
  mutate( amount_individual_precovid = case_when(is.na( amount_individual_precovid) ~ 0,
                                                 T ~  amount_individual_precovid)) %>% 
  mutate(amount_individual_covid = (amount_individual_covid -  amount_individual_precovid)*number_transfers) %>% 
  group_by(type_of_measure) %>% 
  summarise(a = mean(amount_individual_covid))



## Datos de cuántos países tenemos
db %>% 
  mutate( amount_individual_precovid = case_when(is.na( amount_individual_precovid) ~ 0,
                                                 T ~  amount_individual_precovid)) %>% 
  mutate(coverage = amount_individual_covid -  amount_individual_precovid,
         cov_porc = (amount_individual_covid - amount_individual_covid)*number_transfers) %>% 
  mutate(info_montos = cov_porc == 0 ) %>% 
  count(type_of_measure, info_montos) %>% 
  group_by(type_of_measure) %>% 
  mutate(total = sum(n)) %>% 
  ungroup() %>% 
  mutate(porcentaje = n/total) %>% 
  select(-total)


db %>% 
  mutate( amount_individual_precovid = case_when(is.na( amount_individual_precovid) ~ 0,
                                                 T ~  amount_individual_precovid)) %>% 
  mutate(coverage = amount_individual_covid -  amount_individual_precovid,
         cov_porc = (amount_individual_covid - amount_individual_covid) * number_transfers) %>% 
  mutate(info_montos = cov_porc == 0 ) %>% 
  count(type_of_innovation, info_montos) %>% 
  group_by(type_of_innovation) %>% 
  mutate(total = sum(n)) %>% 
  ungroup() %>% 
  mutate(porcentaje = n/total) %>% 
  select(-total)


db %>% 
  mutate(info_montos = case_when(is.na(cost) ~"No", 
                                 T ~ "Si")) %>% 
  count(type_of_measure, info_montos) %>% 
  group_by(type_of_measure) %>% 
  mutate(total = sum(n)) %>% 
  ungroup() %>% 
  mutate(porcentaje = n/total) %>% 
  select(-total)

db %>% 
  mutate(info_montos = case_when(is.na(cost) ~"No", 
                                 T ~ "Si")) %>% 
  count(type_of_innovation, info_montos) %>% 
  group_by(type_of_innovation) %>% 
  mutate(total = sum(n)) %>% 
  ungroup() %>% 
  mutate(porcentaje = n/total) %>% 
  select(-total)


base %>% 
  mutate(hecho = case_when(is.na(date_announcement) ~ date_announcement_official,
                           T ~ date_announcement)) %>% 
  mutate(hecho = floor_date(hecho, "1 month")) %>% 
  mutate(hecho = as_date(hecho)) %>% 
  count(hecho) 


### Visualización----

## G1. Tabla de beneficio e innovación----
base %>% 
  count(type_of_measure, type_of_innovation) %>% 
  pivot_wider(names_from = "type_of_innovation", values_from = "n") %>% 
  mutate_at(vars(horizontal:vertical), ~replace(., is.na(.),0)) %>% 
  pivot_longer(-type_of_measure,
               names_to = "type_of_innovation",
               values_to = "n") %>% 
  ggplot(aes(x = factor(type_of_innovation, order = T,
                        levels = c("new", "vertical", "horizontal")),
             y = factor(type_of_measure, order = T,
                        levels = c("tax reduction","utility waivers","wage subsidy","insurance","voucher","cash transfer")),
             fill = n)) + 
  geom_tile(color = "grey40") +
  scale_y_discrete(labels = c("Reducción\nde impuestos",
                              "Exención de\npago de servicios",
                              "Subsidio a\nsalarios",
                              "Seguro",
                              "Voucher",
                              "Transferencias")) +
  scale_x_discrete(labels = c("Nueva", "Exp.\nvertical", "Exp.\nhorizontal"), position = "top") +
  geom_text(aes(label = n)) +
  scale_fill_gradient(low = "white", high = "steelblue") + 
  labs(y = "Tipo de medida\n",
       x = "Tipo de innovación\n",
       fill = "Número de medidas")  +
  theme_minimal() +
  theme(legend.position = "bottom")


ggsave("3_viz/G1.jpg",
       width = 6,
       height = 4,
       dpi = 600)


## G2. Tabla de países y tipo de beneficio (con cobertura)----

juntar =  db %>% 
  count(type_of_measure, country)  %>% 
  pivot_wider(names_from = "country", values_from = "n") %>% 
  mutate_at(vars(`**Argentina**`:Cuba), ~replace(., is.na(.),0)) %>% 
  mutate(Nicaragua = 0) %>% 
  pivot_longer(-type_of_measure,
               names_to = "type_of_innovation",
               values_to = "n") %>% 
  rename(cuantos = n) %>% 
  mutate(id = str_c(type_of_measure, type_of_innovation, sep = ""))

#
db %>% 
  mutate(coverage_precovid = case_when(is.na(coverage_precovid) ~ 0,
                                       T ~ coverage_precovid)) %>% 
  mutate(coverage_covid_announced = case_when(is.na(coverage_covid_announced) & is.numeric(coverage_covid_reached) ~ coverage_covid_reached,
                                              T ~ coverage_covid_announced)) %>% 
  mutate(coverage = coverage_covid_announced - coverage_precovid,
         cov_porc = (coverage_covid_announced - coverage_precovid)/population) %>% 
  filter(coverage != is.na(coverage),
         cov_porc != is.na(cov_porc)) %>% 
  group_by(type_of_measure, country) %>% 
  summarise(coverage = sum(coverage),
            n = sum(cov_porc)) %>% 
  ungroup() %>% 
  select(-coverage) %>% 
  pivot_wider(names_from = "country", values_from = "n") %>% 
  mutate_at(vars(`**Argentina**`:`**Venezuela**`), ~replace(., is.na(.),0))  %>% 
  mutate(Nicaragua = 0) %>% 
  pivot_longer(-type_of_measure,
               names_to = "type_of_innovation",
               values_to = "n") %>% 
  mutate(id = str_c(type_of_measure, type_of_innovation, sep = "")) %>% 
  left_join(juntar) %>% 
  left_join(names, by = c("type_of_innovation" = "country")) %>% 
  
  ggplot(aes(y = fct_reorder(count, desc(count)),
             x = factor(type_of_measure, order = T,
                        levels = c("cash transfer",
                                   "voucher",
                                   "insurance",
                                   "wage subsidy",
                                   "utility waivers",
                                   "tax reduction")),
             fill = cuantos)) + 
  geom_tile(color = "grey40") +
  scale_x_discrete(labels = c("Transferencias", "Voucher", "Seguro", "Subsidio a\nsalarios", "Exención de\npago de servicios", "Reducción\nde impuestos"), position = "top") +
  geom_text(aes(label = cuantos)) + 
  scale_fill_continuous(low = "white", high = "steelblue", breaks = c(seq(0,12,4))) + 
  labs(y = NULL,
       x = "Tipo de medida\n",
       fill = "Número de medidas")  +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text.y = element_markdown())


ggsave("3_viz/G2.jpg",
       width = 8,
       height = 6,
       dpi = 600)


## G3. Tabla de países y tipo de innovación (con cobertura) -----

juntar = db %>% 
  select(-type_of_measure) %>% 
  mutate(type_of_measure = type_of_innovation) %>% 
  count(type_of_measure, country) %>% 
  pivot_wider(names_from = "type_of_measure", values_from = "n")  %>% 
  rbind(nic2) %>%
  mutate_at(vars(horizontal:vertical), ~replace(., is.na(.),0)) %>% 
  pivot_longer(-country,
               names_to = "type_of_innovation",
               values_to = "n") %>% 
  rename(type_of_measure = country) %>% 
  rename(cuantos = n) %>% 
  mutate(id = str_c(type_of_measure, type_of_innovation, sep = ""))

#
db %>% 
  mutate(coverage_precovid = case_when(is.na(coverage_precovid) ~ 0,
                                       T ~ coverage_precovid)) %>% 
  mutate(coverage_covid_announced = case_when(is.na(coverage_covid_announced) & is.numeric(coverage_covid_reached) ~ coverage_covid_reached,
                                              T ~ coverage_covid_announced)) %>% 
  mutate(coverage = coverage_covid_announced - coverage_precovid,
         cov_porc = (coverage_covid_announced - coverage_precovid)/population) %>% 
  filter(coverage != is.na(coverage),
         cov_porc != is.na(cov_porc)) %>% 
  group_by(type_of_innovation, country) %>% 
  summarise(coverage = sum(coverage),
            n = sum(cov_porc)) %>% 
  ungroup() %>% 
  select(-coverage)  %>% 
  pivot_wider(names_from = "type_of_innovation", values_from = "n")  %>% 
  rbind(nic2) %>%
  mutate_at(vars(horizontal:vertical), ~replace(., is.na(.),0)) %>% 
  pivot_longer(-country,
               names_to = "type_of_innovation",
               values_to = "n") %>% 
  rename(type_of_measure = country) %>% 
  mutate(id = str_c(type_of_measure, type_of_innovation, sep = "")) %>% 
  left_join(juntar) %>% 
  left_join(names, by = c("type_of_measure" = "country")) %>% 
  
  ggplot(aes(y = fct_reorder(count, desc(count)),
             x = factor(type_of_innovation, order = T,
                        levels = c("new", "vertical", "horizontal")),
             fill = cuantos)) + 
  geom_tile(color = "grey40") +
  scale_x_discrete(labels = c("Nueva", "Exp.\nvertical", "Exp.\nhorizontal"), position = "top") +
  geom_text(aes(label = cuantos)) +
  scale_fill_continuous(low = "white", high = "steelblue") + 
  labs(y = NULL,
       x = "Tipo de innovación\n",
       fill = "Cobertura")  +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text.y = element_markdown())


ggsave("3_viz/G3.jpg",
       width = 8,
       height = 6,
       dpi = 600)

## T2. 10 principales programas----

# Cobertura poblacional
db %>% 
  mutate(coverage_precovid = case_when(is.na(coverage_precovid) ~ 0,
                                       T ~ coverage_precovid)) %>% 
  mutate(coverage_covid_announced = case_when(is.na(coverage_covid_announced) & is.numeric(coverage_covid_reached) ~ coverage_covid_reached,
                                              T ~ coverage_covid_announced)) %>% 
  mutate(coverage = coverage_covid_announced - coverage_precovid,
         cov_porc = (coverage_covid_announced - coverage_precovid)/population) %>%
  filter(coverage != is.na(coverage),
         cov_porc != is.na(cov_porc)) %>% 
  mutate( amount_individual_precovid = case_when(is.na( amount_individual_precovid) ~ 0,
                                                 T ~  amount_individual_precovid)) %>% 
  mutate(amount_individual_covid = amount_individual_covid -  amount_individual_precovid) %>% 
  
  arrange(-cov_porc) %>% 
  # slice(2:6,8:11,) %>%
  slice(1:10) %>%
  select(country.x, program_name, type_of_measure, type_of_innovation, cov_porc, coverage, amount_individual_covid, number_transfers) %>% 
  mutate(cov_porc = percent(cov_porc, accuracy = .1)) %>% 
  mutate(type_of_measure = str_replace_all(type_of_measure, 
                                           c("voucher" = "Voucher",
                                             "cash transfer" = "Transferencias",
                                             "utility waivers" = "Exención de pago"))) %>% 
  mutate(type_of_innovation = str_replace_all(type_of_innovation, 
                                              c("new" = "Nueva"))) %>% 
  mutate(program_name = str_replace_all(program_name, 
                                        c("Exemption of the payment of electricity service" = "Exención de pago de electricidad", "Benefits in domestic electricity consumption" = "Exención de pago de electricidad"))) %>% 
  mutate(country.x = str_replace_all(country.x, c(
    "Peru" = "Perú",
    "Panama" = "Panamá", 
    "Brazil" = "Brasil",
    "Belize" = "Belice"))) %>% 
  mutate(coverage = comma(coverage)) %>%
  rename(País = country.x,
         Programa = program_name,
         Medida = type_of_measure,
         Innovación = type_of_innovation,
         Cobertura= cov_porc,
         `Beneficiarios directos` = coverage,
         `Monto adicional promedio (USD)`= amount_individual_covid,
         `Número de apoyos` = number_transfers) %>% 
  kable() %>% 
  kable_styling(font_size = 20)  


## G4. Cobertura agregada por país----

db %>% 
  mutate(coverage_precovid = case_when(is.na(coverage_precovid) ~ 0,
                                       T ~ coverage_precovid)) %>% 
  mutate(coverage_covid_announced = case_when(is.na(coverage_covid_announced) & is.numeric(coverage_covid_reached) ~ coverage_covid_reached,
                                              T ~ coverage_covid_announced)) %>% 
  mutate(coverage = coverage_covid_announced - coverage_precovid,
         cov_porc = (coverage_covid_announced - coverage_precovid)/population) %>% 
  filter(coverage != is.na(coverage),
         cov_porc != is.na(cov_porc)) %>% 
  group_by(country) %>% 
  mutate(ord = sum(cov_porc)) %>% 
  ungroup() %>% 
  add_row(count = "Nicaragua") %>% 
  mutate(ord = case_when(count == "Nicaragua" ~ 0,
                         T ~ ord)) %>% 
  mutate(cov_porc = case_when(count == "Nicaragua" ~ 0,
                              T ~ cov_porc)) %>% 
  mutate(type_of_measure = case_when(count == "Nicaragua" ~ "voucher",
                                     T ~ type_of_measure)) %>% 
  # glimpse()
  # summarise(ord = mean(ord))
  ggplot(aes(x = fct_reorder(count, ord),
             y = cov_porc,
             fill = factor(type_of_measure, order = T,
                           levels = c("cash transfer",
                                      "voucher",
                                      "insurance",
                                      "wage subsidy",
                                      "utility waivers",
                                      "tax reduction")))) +
  geom_col(color = "grey0") +
  geom_text(aes(x = fct_reorder(count, ord),
                y = ord,
                label = percent(ord, accuracy = .1)), hjust = -.2, size = 2.8) + 
  geom_hline(yintercept = 0.374, color = "grey40", size = .5,
             linetype = "dashed") +
  coord_flip() + 
  scale_y_continuous(label = percent_format(accuracy = 1),
                     breaks = c(seq(0,1.4,.2)), limits = c(0, 1.40)) +
  scale_fill_brewer(type = "seq", palette = "Set2", labels = c("Transferencias", "Voucher", "Seguro", "Subsidio a\nsalarios", "Exención de\npago de servicios", "Reducción\nde impuestos"), position = "right") +
  labs(x = NULL,
       y = "Cobertura\n",
       fill = "Tipo de medida")  +
  theme_minimal() +
  theme(axis.text.y = element_markdown(),
        legend.position=c(.8,.4))


ggsave("3_viz/G4.jpg",
       width = 8,
       height = 6,
       dpi = 600)

### G5. Montos----
db %>% 
  mutate(coverage_precovid = case_when(is.na(coverage_precovid) ~ 0,
                                       T ~ coverage_precovid)) %>% 
  mutate(coverage_covid_announced = case_when(is.na(coverage_covid_announced) & is.numeric(coverage_covid_reached) ~ coverage_covid_reached,
                                              T ~ coverage_covid_announced)) %>% 
  mutate(coverage = coverage_covid_announced - coverage_precovid,
         cov_porc = (coverage_covid_announced - coverage_precovid)/population) %>% 
  filter(coverage != is.na(coverage),
         cov_porc != is.na(cov_porc))  %>% 
  # select(program_name, country, cov_porc, cost) %>% 
  group_by(country, acronym) %>% 
  summarise(cov_porc = sum(cov_porc, na.rm = T),
            cov = sum(coverage, na.rm = T),
            cost = sum(cost, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(monto = cost/cov) %>% 
  arrange(-monto) %>% 
  add_row(country = "Nicaragua")  %>% 
  mutate(monto = case_when(country == "Nicaragua" ~ 0,
                           T ~ monto)) %>% 
  arrange(-monto)  %>% 
  select(country, acronym, monto, cov_porc, cov, cost) %>% 
  ggplot(aes(x = cov_porc,
             y = monto)) + 
  geom_vline(xintercept = 0.374, color = "grey40", size = .5,
             linetype = "dashed") +
  geom_hline(yintercept = 485, color = "grey40", size = .5,
             linetype = "dashed") +
  geom_point() + 
  geom_text_repel(aes(label = acronym)) + 
  scale_y_continuous(label = dollar_format()) +
  scale_x_continuous(label = label_percent()) + 
  labs(x = "\nCobertura como número agregado de\napoyos entregados entre población",
       y = "Costo entre\npersonas beneficiarias\n",
       fill = NULL)  +
  theme_minimal() +
  theme(axis.text.y = element_markdown(),
        legend.position=c(.8,.4), 
        text = element_text(size = 14))

ggsave("3_viz/G5.jpg",
       width = 8,
       height = 6)


### G6 Info para seleccionar----
db %>% 
  mutate_at(vars(new_info_deliver:previous_info_select), ~replace(., is.na(.), "ND")) %>% 
  mutate(select = case_when(new_information_select == "Yes" & previous_info_select == "Yes" ~ "Both",
                            new_information_select == "Yes" & previous_info_select == "ND" ~ "New",
                            new_information_select == "Yes" & previous_info_select == "No" ~ "New",
                            new_information_select == "ND" & previous_info_select == "Yes" ~ "Previous",
                            new_information_select == "ND" & previous_info_select == "ND" ~ "ND",
                            new_information_select == "ND" & previous_info_select == "No" ~ "ND",
                            new_information_select == "No" & previous_info_select == "Yes" ~ "Previous",
                            new_information_select == "No" & previous_info_select == "ND" ~ "ND",
                            new_information_select == "No" & previous_info_select == "No" ~ "ND")) %>% 
  mutate(coverage_precovid = case_when(is.na(coverage_precovid) ~ 0,
                                       T ~ coverage_precovid)) %>% 
  mutate(coverage_covid_announced = case_when(is.na(coverage_covid_announced) & is.numeric(coverage_covid_reached) ~ coverage_covid_reached,
                                              T ~ coverage_covid_announced)) %>% 
  mutate(coverage = coverage_covid_announced - coverage_precovid,
         cov_porc = (coverage_covid_announced - coverage_precovid)/population) %>% 
  filter(coverage != is.na(coverage),
         cov_porc != is.na(cov_porc)) %>% 
  group_by(country) %>% 
  mutate(ord = sum(cov_porc)) %>% 
  ungroup() %>% 
  add_row(count = "Nicaragua") %>% 
  mutate(ord = case_when(count == "Nicaragua" ~ 0,
                         T ~ ord)) %>% 
  mutate(cov_porc = case_when(count == "Nicaragua" ~ 0,
                              T ~ cov_porc)) %>% 
  mutate(type_of_measure = case_when(count == "Nicaragua" ~ "voucher",
                                     T ~ type_of_measure)) %>% 
  mutate(select = case_when(is.na(select) ~ "ND",
                            T ~ select)) %>% 
  # summarise(ord = mean(ord))
  ggplot(aes(x = fct_reorder(count, ord),
             y = cov_porc,
             fill = factor(select, order = T,
                           levels = c("Both", "Previous", "New", "ND")))) +
  geom_col(color = "grey0") +
  geom_text(aes(x = fct_reorder(count, ord),
                y = ord,
                label = percent(ord, accuracy = .1)), hjust = -.2, size = 2.8) + 
  geom_hline(yintercept = 0.374, color = "grey40", size = .5,
             linetype = "dashed") +
  coord_flip() + 
  scale_y_continuous(label = percent_format(accuracy = 1),
                     breaks = c(seq(0,1.4,.2)), limits = c(0, 1.40)) +
  scale_fill_brewer(type = "seq", palette = "Paired", labels = c("Previa y nueva",  "Solo previa","Solo nueva", "ND"), position = "right") +
  labs(x = NULL,
       y = "Cobertura\n",
       fill = "Información\npara seleccionar")  +
  theme_minimal() +
  theme(axis.text.y = element_markdown(),
        legend.position=c(.8,.4))


ggsave("3_viz/G6.jpg",
       width = 8,
       height = 6,
       dpi = 600)

### G7: Info para seleccionar----
db %>% 
  mutate_at(vars(new_info_deliver:previous_info_select), ~replace(., is.na(.), "ND")) %>%
  mutate(deliver = case_when(new_info_deliver == "Yes" & previous_info_deliver == "Yes" ~ "Both",
                             new_info_deliver == "Yes" & previous_info_deliver == "ND" ~ "New",
                             new_info_deliver == "Yes" & previous_info_deliver == "No" ~ "New",
                             new_info_deliver == "ND" & previous_info_deliver == "Yes" ~ "Previous",
                             new_info_deliver == "ND" & previous_info_deliver == "ND" ~ "ND",
                             new_info_deliver == "ND" & previous_info_deliver == "No" ~ "ND",
                             new_info_deliver == "No" & previous_info_deliver == "Yes" ~ "Previous",
                             new_info_deliver == "No" & previous_info_deliver == "ND" ~ "ND",
                             new_info_deliver == "No" & previous_info_deliver == "No" ~ "ND")) %>% 
  mutate(coverage_precovid = case_when(is.na(coverage_precovid) ~ 0,
                                       T ~ coverage_precovid)) %>% 
  mutate(coverage_covid_announced = case_when(is.na(coverage_covid_announced) & is.numeric(coverage_covid_reached) ~ coverage_covid_reached,
                                              T ~ coverage_covid_announced)) %>% 
  mutate(coverage = coverage_covid_announced - coverage_precovid,
         cov_porc = (coverage_covid_announced - coverage_precovid)/population) %>% 
  filter(coverage != is.na(coverage),
         cov_porc != is.na(cov_porc)) %>% 
  group_by(country) %>% 
  mutate(ord = sum(cov_porc)) %>% 
  ungroup() %>% 
  add_row(count = "Nicaragua") %>% 
  mutate(ord = case_when(count == "Nicaragua" ~ 0,
                         T ~ ord)) %>% 
  mutate(cov_porc = case_when(count == "Nicaragua" ~ 0,
                              T ~ cov_porc)) %>% 
  mutate(type_of_measure = case_when(count == "Nicaragua" ~ "voucher",
                                     T ~ type_of_measure)) %>% 
  mutate(deliver = case_when(is.na(deliver) ~ "ND",
                             T ~ deliver)) %>% 
  # summarise(ord = mean(ord))
  ggplot(aes(x = fct_reorder(count, ord),
             y = cov_porc,
             fill = factor(deliver, order = T,
                           levels = c("Both", "Previous", "New", "ND")))) +
  geom_col(color = "grey0") +
  geom_text(aes(x = fct_reorder(count, ord),
                y = ord,
                label = percent(ord, accuracy = .1)), hjust = -.2, size = 2.8) + 
  geom_hline(yintercept = 0.374, color = "grey40", size = .5,
             linetype = "dashed") +
  coord_flip() + 
  scale_y_continuous(label = percent_format(accuracy = 1),
                     breaks = c(seq(0,1.4,.2)), limits = c(0, 1.40)) +
  scale_fill_brewer(type = "seq", palette = "Paired", labels = c("Previa y nueva",  "Solo previa","Solo nueva", "ND"), position = "right") +
  labs(x = NULL,
       y = "Cobertura\n",
       fill = "Información\npara entregar")  +
  theme_minimal() +
  theme(axis.text.y = element_markdown(),
        legend.position=c(.8,.4))


ggsave("3_viz/G7.jpg",
       width = 8,
       height = 6,
       dpi = 600)


### G8 Fuentes de info----

l = latam %>% 
  # filter(country != "Venezuela",
  #        country != "Suriname",
  #        country != "Nicaragua",
  #        country != "Cuba",
  #        country != "Saint Lucia",
  #        country != "Saint Christopher and Nevis",
  #        country != "Antigua and Barbuda") %>% 
  select(a, rs_cob_porc, ctp_perc) %>% 
  mutate(padron_perc = NA_integer_) %>% 
  pivot_longer(-a,
               names_to = "var_num",
               values_to = "values_num") %>% 
  select(-a)

lat = latam %>% 
  # filter(country != "Venezuela",
  #        country != "Suriname",
  #        country != "Nicaragua",
  #        country != "Cuba",
  #        country != "Saint Lucia",
  #        country != "Saint Christopher and Nevis",
  #        country != "Antigua and Barbuda") %>% 
  select(a, count, registro, ctp, padron) %>% 
  pivot_longer(registro:padron,
               names_to = "var_col",
               values_to = "values_col") %>% 
  cbind(l) 

lat = as_tibble(lat)

latam %>% 
  mutate(rs_cob_porc = case_when(is.na(rs_cob_porc) ~ 0,
                                 T ~ rs_cob_porc)) %>% 
  summarise(a = mean(rs_cob_porc, na.rm = T))


lat %>% 
  add_row(a = "Nicaragua", count = "Nicaragua", var_col = "registro", values_col = 0, var_num = "rs_cob_porc", values_num = NA_integer_) %>%
  add_row(a = "Nicaragua", count = "Nicaragua",  var_col = "ctp", values_col = 0, var_num = "ctp_perc", values_num = NA_integer_) %>%
  add_row(a = "Nicaragua", count = "Nicaragua", var_col = "padron", values_col = 0, var_num = "padron_perc", values_num = NA_integer_)  %>% 
  # mutate(country = str_replace_all(country, c("Saint Vincent and the Grenadines" = "Saint Vincent"))) %>% 
  ggplot() + 
  geom_tile(aes(x = factor(var_col, order = T,
                           levels = c("registro", "ctp", "padron")),
                y = fct_reorder(count, desc(count)),
                fill = factor(values_col)),
            color = "grey0", size = .7) +
  geom_text(aes(x = factor(var_col, order = T,
                           levels = c("registro", "ctp", "padron")),
                label = percent(values_num, 0.1),
                y = fct_reorder(count, desc(count))),
            color = "grey0",
            fontface = "bold",
            size = 2, show.legend = F) +
  scale_fill_manual(values = c("white", "grey70"), 
                    labels = c("No", "Sí")) +
  scale_x_discrete(labels = c("Registro\nsocial\n",
                              "Mayor programa\nde transferencias\nexistente",
                              "Registro\núnico de\nbeneficiarios"), position = "top") +
  labs (x = "",
        y = "",
        fill = "Existencia") + 
  theme_minimal() + 
  theme(legend.position = "right",
        axis.text.y = element_markdown())

ggsave("3_viz/G8.png",
       width = 6,
       height = 5,
       dpi = 600)

## G9. Inversión----

## Por tipo de beneficio
db %>% 
  filter(cost != is.na(cost)) %>% 
  mutate(cost_porc = (cost/population)/gdp_percap) %>% 
  group_by(country) %>% 
  mutate(ord = sum(cost_porc)) %>% 
  ungroup() %>% 
  add_row(count = "Nicaragua") %>% 
  mutate(ord = case_when(count == "Nicaragua" ~ 0,
                         T ~ ord)) %>% 
  mutate(cost_porc = case_when(count == "Nicaragua" ~ 0,
                               T ~ cost_porc)) %>% 
  mutate(type_of_measure = case_when(count == "Nicaragua" ~ "voucher",
                                     T ~ type_of_measure)) %>% 
  # count(country) %>% print(n = Inf)
  # filter(country != "**Venezuela**") %>%
  # group_by(country) %>%
  # summarise(a = sum(cost_porc)) %>%
  # ungroup() %>%
  # summarise(a = mean(a, na.rm = T))
  ggplot(aes(x = fct_reorder(count, ord),
             y = cost_porc,
             fill = factor(type_of_measure, order = T,
                           levels = c("cash transfer",
                                      "voucher",
                                      "insurance",
                                      "wage subsidy",
                                      "utility waivers",
                                      "tax reduction")),
             group = country)) +
  geom_col(color = "grey0", size = .4) + 
  geom_text(aes(y = ord,
                label = percent(ord, accuracy = .1)), hjust = -0.25) + 
  coord_flip() + 
  geom_hline(yintercept = .0145,
             color = "grey40", size = 1, linetype = "dashed") +
  scale_y_continuous(label = percent_format(accuracy = 1),
                     breaks = c(seq(0,02,.01)), limits = c(0,.085)) +
  scale_fill_brewer(type = "seq", palette = "Set2", labels = c("Transferencias", "Voucher", "Seguro", "Subsidio a\nsalarios", "Exención\nde servicios", "Reducción\nde impuestos"), position = "right") +
  labs(x = NULL,
       y = "Inversión entre PIB\n",
       fill = "Tipo de medida")  +
  theme_minimal() +
  theme(legend.position = "right", axis.text.y = element_markdown())

ggsave("3_viz/G9.jpg",
       width = 8,
       height = 6,
       dpi = 600)

