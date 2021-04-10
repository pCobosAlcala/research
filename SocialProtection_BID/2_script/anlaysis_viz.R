### Paquetes ----
pacman::p_load(tidyverse, scales, ggrepel, ggtext, kableExtra, lubridate)

## Setup
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Mac
Sys.setlocale("LC_ALL", "Spanish_Mexico.1252") # Windows
options(scipen=999) 

## Eliminar objetos
rm(list = ls())


### Datos----
base <- readxl::read_excel("1_data/base.xlsx", col_types = c("text", 
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


latam <- readxl::read_excel("1_data/base.xlsx", sheet = "countries") %>% 
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



### Análisis inicial----
glimpse(base)

# Número de programas
nrow(base)

# Número de países
base %>% 
  count(country) %>% 
  print(n = Inf)


# Tipo de beneficios
base %>% 
  count(type_of_measurement)

# Tipo de innovación
base %>% 
  count(type_of_innovation)

# Monto promedio por duración
db %>% 
  mutate(a = case_when(is.na(amount_individual_covid) ~ 1,
                       T ~ 0)) %>% 
  filter(a == 0) %>% 
  filter(number_transfers != is.na(number_transfers)) %>% 
  mutate( amount_individual_precovid = case_when(is.na( amount_individual_precovid) ~ 0,
                                                 T ~  amount_individual_precovid)) %>% 
  mutate(amount_individual_covid = (amount_individual_covid -  amount_individual_precovid)*number_transfers) %>% 
  summarise(a = mean(amount_individual_covid))


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
  ungroup() %>% 
  summarise(a = mean(a))


# Monto mediana
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


# Monto promedio
db %>% 
  mutate(a = case_when(is.na(amount_individual_covid) ~ 1,
                       T ~ 0)) %>% 
  filter(a == 0) %>% 
  mutate( amount_individual_precovid = case_when(is.na( amount_individual_precovid) ~ 0,
                                                 T ~  amount_individual_precovid)) %>% 
  mutate(amount_individual_covid = amount_individual_covid -  amount_individual_precovid) %>% 
  group_by(country) %>% 
  summarise(a = mean(amount_individual_covid)) %>% 
  ungroup() %>% 
  summarise(a = mean(a))

db %>% 
  mutate(a = case_when(is.na(amount_individual_covid) ~ 1,
                       T ~ 0)) %>% 
  filter(a == 0) %>% 
  mutate( amount_individual_precovid = case_when(is.na( amount_individual_precovid) ~ 0,
                                                 T ~  amount_individual_precovid)) %>% 
  mutate(amount_individual_covid = amount_individual_covid -  amount_individual_precovid) %>% 
  group_by(country) %>% 
  summarise(a = mean(amount_individual_covid)) %>% 
  ungroup() %>% 
  summarise(a = median(a))


# Montos por tipo de medida
db %>% 
  mutate(a = case_when(is.na(amount_individual_covid) ~ 1,
                       T ~ 0)) %>% 
  filter(a == 0) %>% 
  mutate( amount_individual_precovid = case_when(is.na( amount_individual_precovid) ~ 0,
                                                 T ~  amount_individual_precovid)) %>% 
  mutate(amount_individual_covid = amount_individual_covid -  amount_individual_precovid) %>% 
  group_by(type_of_measurement) %>% 
  summarise(a = mean(amount_individual_covid))


# Montos por tipo de medida y duración
db %>% 
  mutate(a = case_when(is.na(amount_individual_covid) ~ 1,
                       T ~ 0)) %>% 
  filter(a == 0) %>% 
  filter(number_transfers != is.na(number_transfers)) %>% 
  
  mutate( amount_individual_precovid = case_when(is.na( amount_individual_precovid) ~ 0,
                                                 T ~  amount_individual_precovid)) %>% 
  mutate(amount_individual_covid = (amount_individual_covid -  amount_individual_precovid)*number_transfers) %>% 
  group_by(type_of_measurement) %>% 
  summarise(a = mean(amount_individual_covid))


# Cobertura promedio de cash transfer
db %>% 
  filter(type_of_measurement == "cash transfer") %>% 
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


# Cobertura promedio de cash transfer y voucher
db %>% 
  filter(type_of_measurement %in% c( "cash transfer", "voucher")) %>% 
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


## Datos de cuántos países tenemos
db %>% 
  mutate( amount_individual_precovid = case_when(is.na( amount_individual_precovid) ~ 0,
                                                 T ~  amount_individual_precovid)) %>% 
  mutate(coverage = amount_individual_covid -  amount_individual_precovid,
         cov_porc = (amount_individual_covid - amount_individual_covid)*number_transfers) %>% 
  mutate(info_montos = cov_porc == 0 ) %>% 
  count(type_of_measurement, info_montos) %>% 
  group_by(type_of_measurement) %>% 
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
  count(type_of_measurement, info_montos) %>% 
  group_by(type_of_measurement) %>% 
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





### Vizualización----

## G1. Tabla de beneficio e innovación----
base %>% 
  count(type_of_measurement, type_of_innovation) %>% 
  pivot_wider(names_from = "type_of_innovation", values_from = "n") %>% 
  mutate_at(vars(horizontal:vertical), ~replace(., is.na(.),0)) %>% 
  pivot_longer(-type_of_measurement,
               names_to = "type_of_innovation",
               values_to = "n") %>% 
  ggplot(aes(x = factor(type_of_innovation, order = T,
                        levels = c("new", "vertical", "horizontal")),
             y = factor(type_of_measurement, order = T,
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
  labs(y = "Tipo de beneficio\n",
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
  count(type_of_measurement, country)  %>% 
  pivot_wider(names_from = "country", values_from = "n") %>% 
  mutate_at(vars(`**Argentina**`:Cuba), ~replace(., is.na(.),0)) %>% 
  mutate(Nicaragua = 0) %>% 
  pivot_longer(-type_of_measurement,
               names_to = "type_of_innovation",
               values_to = "n") %>% 
  rename(cuantos = n) %>% 
  mutate(id = str_c(type_of_measurement, type_of_innovation, sep = ""))

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
  group_by(type_of_measurement, country) %>% 
  summarise(coverage = sum(coverage),
            n = sum(cov_porc)) %>% 
  ungroup() %>% 
  select(-coverage) %>% 
  pivot_wider(names_from = "country", values_from = "n") %>% 
  mutate_at(vars(`**Argentina**`:`**Venezuela**`), ~replace(., is.na(.),0))  %>% 
  mutate(Nicaragua = 0) %>% 
  pivot_longer(-type_of_measurement,
               names_to = "type_of_innovation",
               values_to = "n") %>% 
  mutate(id = str_c(type_of_measurement, type_of_innovation, sep = "")) %>% 
  left_join(juntar) %>% 
  left_join(names, by = c("type_of_innovation" = "country")) %>% 

  ggplot(aes(y = fct_reorder(type_of_innovation, desc(count)),
             x = factor(type_of_measurement, order = T,
                        levels = c("cash transfer",
                                   "voucher",
                                   "insurance",
                                   "wage subsidy",
                                   "utility waivers",
                                   "tax reduction")),
             fill = n)) + 
  geom_tile(color = "grey40") +
  scale_x_discrete(labels = c("Transferencias", "Voucher", "Seguro", "Subsidio a\nsalarios", "Exención de\npago de servicios", "Reducción\nde impuestos"), position = "top") +
  geom_text(aes(label = cuantos)) + 
  scale_fill_continuous(low = "white", high = "steelblue",
                        label = percent_format()) + 
  labs(y = NULL,
       x = "Tipo de medida\n",
       fill = "Cobertura")  +
  theme_minimal() +
  theme(legend.position = "right", axis.text.y = element_markdown())


ggsave("3_viz/G2.jpg",
       width = 8,
       height = 6,
       dpi = 600)


## G3. Tabla de países y tipo de innovación (con cobertura) -----

juntar = db %>% 
  select(-type_of_measurement) %>% 
  mutate(type_of_measurement = type_of_innovation) %>% 
  count(type_of_measurement, country) %>% 
  pivot_wider(names_from = "type_of_measurement", values_from = "n")  %>% 
  rbind(nic2) %>%
  mutate_at(vars(horizontal:vertical), ~replace(., is.na(.),0)) %>% 
  pivot_longer(-country,
               names_to = "type_of_innovation",
               values_to = "n") %>% 
  rename(type_of_measurement = country) %>% 
  rename(cuantos = n) %>% 
  mutate(id = str_c(type_of_measurement, type_of_innovation, sep = ""))

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
  rename(type_of_measurement = country) %>% 
  mutate(id = str_c(type_of_measurement, type_of_innovation, sep = "")) %>% 
  left_join(juntar) %>% 
  left_join(names, by = c("type_of_measurement" = "country")) %>% 
  
  ggplot(aes(y = fct_reorder(type_of_measurement, desc(count)),
             x = factor(type_of_innovation, order = T,
                        levels = c("new", "vertical", "horizontal")),
             fill = n)) + 
  geom_tile(color = "grey40") +
  scale_x_discrete(labels = c("Nueva", "Exp.\nvertical", "Exp.\nhorizontal"), position = "top") +
  geom_text(aes(label = cuantos)) +
  scale_fill_continuous(low = "white", high = "steelblue",
                        label = percent_format()) + 
  labs(y = NULL,
       x = "Tipo de innovación\n",
       fill = "Cobertura")  +
  theme_minimal() +
  theme(legend.position = "right", axis.text.y = element_markdown())


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
  slice(2:6,8:11) %>% 
  # slice(4:8,10:14) %>%
  select(country.x, program_name, type_of_measurement, type_of_innovation, cov_porc, coverage, amount_individual_covid, number_transfers) %>% 
  mutate(cov_porc = percent(cov_porc, accuracy = .1)) %>% 
  mutate(type_of_measurement = str_replace_all(type_of_measurement, 
                                           c("voucher" = "Voucher",
                                             "cash transfer" = "Transferencias",
                                             "utility waivers" = "Exención de pago"))) %>% 
  mutate(type_of_innovation = str_replace_all(type_of_innovation, 
                                              c("new" = "Nueva"))) %>% 
  mutate(program_name = str_replace_all(program_name, 
                                              c("Exemption of the payment of electricity service" = "Exención de pago de electricidad"))) %>% 
  mutate(country.x = str_replace_all(country.x, c(
                                            "Peru" = "Perú",
                                            "Panama" = "Panamá", 
                                            "Brazil" = "Brasil",
                                            "Belize" = "Belice"))) %>% 
  mutate(coverage = comma(coverage)) %>%
    rename(País = country.x,
         Programa = program_name,
         Medida = type_of_measurement,
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
  add_row(country = "Nicaragua") %>% 
  mutate(ord = case_when(country == "Nicaragua" ~ 0,
                         T ~ ord)) %>% 
  mutate(cov_porc = case_when(country == "Nicaragua" ~ 0,
                         T ~ cov_porc)) %>% 
  mutate(type_of_measurement = case_when(country == "Nicaragua" ~ "voucher",
                              T ~ type_of_measurement)) %>% 
  # summarise(ord = mean(ord))
  ggplot(aes(x = fct_reorder(country, ord),
             y = cov_porc,
             fill = factor(type_of_measurement, order = T,
                           levels = c("cash transfer",
                                      "voucher",
                                      "insurance",
                                      "wage subsidy",
                                      "utility waivers",
                                      "tax reduction")))) +
  geom_col(color = "grey0") +
  geom_text(aes(x = fct_reorder(country, ord),
                y = ord,
                label = percent(ord, accuracy = .1)), hjust = -.2, size = 2.8) + 
  geom_hline(yintercept = 0.365, color = "grey40", size = .5,
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

## G5. Inversión----

## Por tipo de beneficio
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
  mutate(type_of_measurement = case_when(country == "Nicaragua" ~ "voucher",
                                         T ~ type_of_measurement)) %>% 
  # group_by(country) %>%
  # summarise(a = sum(cost_porc)) %>%
  # ungroup() %>%
  # summarise(a = mean(a))
  ggplot(aes(x = fct_reorder(country, ord),
             y = cost_porc,
             fill = factor(type_of_measurement, order = T,
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
  geom_hline(yintercept = .0142,
             color = "grey40", size = 1, linetype = "dashed") +
  scale_y_continuous(label = percent_format(accuracy = 1),
                     breaks = c(seq(0,02,.01)), limits = c(0,.085)) +
  scale_fill_brewer(type = "seq", palette = "Set2", labels = c("Transferencias", "Voucher", "Seguro", "Subsidio a\nsalarios", "Exención\nde servicios", "Reducción\nde impuestos"), position = "right") +
  labs(x = NULL,
       y = "Inversión entre PIB\n",
       fill = "Tipo de medida")  +
  theme_minimal() +
  theme(legend.position = "right", axis.text.y = element_markdown())

ggsave("3_viz/G5.jpg",
       width = 8,
       height = 6,
       dpi = 600)



## G6. Fechas----

# Tipo de beneficio
base %>% 
  mutate(hecho = case_when(is.na(date_announcement) ~ date_announcement_official,
                           T ~ date_announcement)) %>% 
  mutate(hecho = floor_date(hecho, "1 month")) %>% 
  mutate(hecho = as_date(hecho)) %>% 
  count(hecho, type_of_measurement) %>% 
  ggplot(aes(x = hecho,
             y = n,
             fill = factor(type_of_measurement, order = T,
                           levels = c("cash transfer",
                                      "voucher",
                                      "insurance",
                                      "wage subsidy",
                                      "utility waivers",
                                      "tax reduction")))) +
  scale_x_date(date_breaks = "1 month",  date_labels = "%b")  +
  annotate("text",
           x = as.Date("2020-12-17"),
           y = 60,
           label = "2021",
           color = "grey0",
           size = 3.3,
           hjust = -.5,
           fontface = "bold") +
  scale_x_date(date_breaks = "1 month",  date_labels = "%b")  +
  annotate("text",
           x = as.Date("2020-12-17"),
           y = 60,
           label = "2020",
           color = "grey0",
           size = 3.3,
           hjust = 1.5,
           fontface = "bold") +
  geom_vline(xintercept = as.Date("2020-12-17"), color = "grey40", size = .5,
             linetype = "dashed") +
  geom_col(col = "grey0") +
  scale_fill_brewer(type = "seq", palette = "Set2", labels = c("Transferencias", "Voucher", "Seguro", "Subsidio a\nsalarios", "Exención de\npago de servicios", "Reducción\nde impuestos"), position = "right") +
  labs(y = NULL,
       x = "\nMes del primer anuncio de las medidas\n",
       fill = "Tipo de medida")  +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = .5),
        panel.grid.major.x = element_line(size = .5),
        panel.grid.minor.x = element_blank(),
        plot.caption = element_markdown(hjust = 0))


ggsave("3_viz/G6.jpg",
       width = 6,
       height = 4,
       dpi = 600)


### G7 y G8. Información previa y nueva----

# Cash transfer y voucher
db %>% 
  filter(type_of_measurement %in% c("cash transfer", "voucher")) %>% 
  select(type_of_measurement, new_info_deliver, new_information_select, previous_info_deliver, previous_info_select) %>%
  mutate_at(vars(new_info_deliver:previous_info_select), ~replace(., is.na(.), "Sin info.")) %>% 
  pivot_longer(new_info_deliver:previous_info_select,
               names_to = "var",
               values_to = "values") %>% 
  group_by(var, values) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(var) %>% 
  mutate(total = n()) %>% 
  mutate(porc = n / 134) %>% 
  ungroup() %>% 
  mutate(var = factor(var, order = T,
                      levels = c("previous_info_select",
                                 "new_information_select",
                                 "previous_info_deliver",
                                 "new_info_deliver"))) %>% 
  ggplot(aes(x = var, 
             y = porc,
             fill = values)) +
  geom_col(position = "fill", alpha = 0.7) +
  geom_text(aes(label = percent(porc, accuracy = 0.1)), position = position_fill(vjust = 0.5), size = 2.8) + 
  scale_x_discrete(labels = c("Previa para\nseleccionar",
                              "Nueva para\nseleccionar",
                              "Previa para\napoyar",
                              "Nueva para\napoyar"), position = "top") +
  scale_fill_brewer(type = "qual", palette = "Dark2", labels = c("No", "ND","Sí")) +
  scale_y_continuous(label = percent_format(accuracy= 1)) + 
  theme_minimal() +
  labs(x = "",
       y = NULL,
       fill = "Uso de información") + 
  theme(legend.position = "bottom",
        axis.text.x = element_markdown(size = 10))


ggsave("3_viz/G7.jpg",
       width = 6,
       height = 4,
       dpi = 600)



# Sin cash transfer ni voucher
db %>% 
  filter(type_of_measurement != "cash transfer",
         type_of_measurement != "voucher") %>% 
  
  select(type_of_measurement, new_info_deliver, new_information_select, previous_info_deliver, previous_info_select) %>%
  mutate_at(vars(new_info_deliver:previous_info_select), ~replace(., is.na(.), "Sin info.")) %>% 
  pivot_longer(new_info_deliver:previous_info_select,
               names_to = "var",
               values_to = "values") %>% 
  group_by(var, values) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(var) %>% 
  mutate(total = n()) %>% 
  mutate(porc = n / 65) %>% 
  ungroup() %>% 
  mutate(var = factor(var, order = T,
                      levels = c("previous_info_select",
                                 "new_information_select",
                                 "previous_info_deliver",
                                 "new_info_deliver"))) %>% 
  ggplot(aes(x = var, 
             y = porc,
             fill = values)) +
  geom_col(position = "fill", alpha = 0.7) +
  geom_text(aes(label = percent(porc, accuracy = 0.1)), position = position_fill(vjust = 0.5), size = 2.8) + 
  scale_x_discrete(labels = c("Previa para\nseleccionar",
                              "Nueva para\nseleccionar",
                              "Previa para\napoyar",
                              "Nueva para\napoyar"), position = "top") +
  scale_fill_brewer(type = "qual", palette = "Dark2", labels = c("No", "ND","Sí")) +
  scale_y_continuous(label = percent_format(accuracy= 1)) + 
  theme_minimal() +
  labs(x = "",
       y = NULL,
       fill = "Uso de información") + 
  theme(legend.position = "bottom",
        axis.text.x = element_markdown(size = 10))


ggsave("3_viz/G8.jpg",
       width = 6,
       height = 4,
       dpi = 600)
