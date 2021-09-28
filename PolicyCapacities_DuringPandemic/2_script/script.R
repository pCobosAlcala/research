### Paquetes ----
pacman::p_load(tidyverse, scales, ggrepel, ggtext)

## Setup
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Mac
Sys.setlocale("LC_ALL", "Spanish_Mexico.1252") # Windows
options(scipen=999) 

## Eliminar objetos
rm(list = ls())


### Data----

## Principal databases
base <- readxl::read_excel("1_data/db.xlsx", sheet = "responses_covid")
latam <- readxl::read_excel("1_data/db.xlsx", sheet = "countries")
latam2 <- readxl::read_excel("1_data/db.xlsx", sheet = "ctp")

## Joining databases

# base = All CTPs
base = base %>% 
  filter(type_of_measure %in% c("cash transfer", "voucher")) %>% 
  filter(country != "Venezuela",
         country != "Suriname",
         country != "Nicaragua",
         country != "Cuba",
         country != "Saint Lucia",
         country != "Saint Christopher and Nevis") 

# cov = Coverage by country during COVID
cov = base %>% 
  left_join(latam, by = "country") %>% 
  mutate(coverage_precovid = case_when(is.na(coverage_precovid) ~ 0,
                                       T ~ coverage_precovid)) %>% 
  mutate(coverage_covid_announced = case_when(is.na(coverage_covid_announced) & is.numeric(coverage_covid_reached) ~ coverage_covid_reached,
                                              T ~ coverage_covid_announced)) %>% 
  mutate(coverage = coverage_covid_announced - coverage_precovid,
         cov_porc = (coverage_covid_announced - coverage_precovid)/population,
         cov_t = cov_porc*number_transfers) %>% 
  filter(coverage != is.na(coverage)) %>% 
  group_by(country) %>% 
  summarise(cov = sum(coverage, na.rm = T),
            cov_porc = sum(cov_porc),
            cov_t = sum(cov_t, na.rm = T)) %>% 
  ungroup() 

# ind = Ongoing CTPs
ind = latam2 %>% 
  mutate(value = 2020-starting_year) %>% 
  select(country, value, starting_year)


ind = latam2 %>% 
  filter(country != "Venezuela",
         country != "Suriname",
         country != "Nicaragua",
         country != "Cuba",
         country != "Saint Lucia",
         country != "Saint Christopher and Nevis") %>% 
  select(country, starting_year) %>% 
  mutate(year = 2020 - starting_year) %>% 
  select(country, year)

# ba = All countries' information
ba = latam %>% 
  filter(country != "Venezuela",
         country != "Suriname",
         country != "Nicaragua",
         country != "Cuba",
         country != "Saint Lucia",
         country != "Saint Christopher and Nevis") %>% 
  left_join(cov, by = "country") %>% 
  left_join(ind, by = "country") %>% 
  mutate(info = registro + padron + interoperabilidad) %>% 
  mutate_at(vars(cov, cov_porc, year, rs_cob_porc), ~replace(., is.na(.),0))

b = ba %>% 
  select(country, year)

### General statistics----

# Number of countries
nrow(ba)

# Number of measures
nrow(base) 


# Number of measures by country
base %>% 
  filter(type_of_innovation == "horizontal") %>% 
  count(country) %>% 
  # arrange(-n) %>% 
  # summarise(n = sum(n))
  print(n = Inf)

# Past experience description
ba %>% 
  select(country, year) %>% 
  # filter(year != 0) %>%
  # summarise(a = mean(year))
  # arrange(year) %>%
  print(n = Inf)

# Preparedness description
ba %>% 
  select(country, rs_cob_porc) %>% 
  # filter(rs_cob_porc != 0) %>%
  arrange(-rs_cob_porc) %>% 
  print(n = Inf)

# Ongoing CTP description
ba %>% 
  select(country, ctp_perc) %>% 
  # filter(rs_cob_porc != 0) %>%
  arrange(-ctp_perc) %>% 
  print(n = Inf)


# Coverage during covid
ba %>% 
  select(country, cov) %>% 
  # filter(cov != 0) %>% 
  arrange(-cov) %>% 
  print(n = Inf)


# Coverage during covid
ba %>% 
  select(country, cov_porc) %>% 
  # filter(cov != 0) %>% 
  arrange(-cov_porc) %>% 
  # summarise(mean = mean(cov_porc))
  print(n = Inf)


ba %>% 
  summarise(a = mean(cov_porc))

# coverage during covid
ba %>% 
  select(country, cov_t) %>% 
  # filter(cov != 0) %>% 
  arrange(-cov_t) %>% 
  print(n = Inf)

# Use of info.
base %>% 
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
  mutate(coverage_precovid = case_when(is.na(coverage_precovid) ~ 0,
                                       T ~ coverage_precovid)) %>% 
  mutate(coverage_covid_announced = case_when(is.na(coverage_covid_announced) & is.numeric(coverage_covid_reached) ~ coverage_covid_reached,
                                              T ~ coverage_covid_announced)) %>% 
  left_join(latam) %>% 
  left_join(ind) %>% 
  mutate(coverage = coverage_covid_announced - coverage_precovid,
         cov_porc = (coverage_covid_announced - coverage_precovid)/population) %>% 
  group_by(country) %>% 
  mutate(ord = sum(cov_porc)) %>% 
  ungroup() %>% 
  filter(type_of_innovation == "new") %>%
  count(select) %>% 
  mutate(total = sum(n))

(31+40)/95
(14+28)/95


# Programs in countries implementing CTP
base %>% 
  left_join(ind) %>% 
  filter(year != is.na(year)) %>% 
  filter(type_of_innovation == "new") %>% nrow()

# Average number of years implementing a CTP. Countries with vertical expansion

base %>% 
  filter(type_of_innovation == "vertical") %>% 
  left_join(ba) %>% 
  count(country, year) %>% 
  summarise(mean = mean(year))


### G1: past experience----

ba %>% 
  mutate(country = str_replace_all(country, c("Saint Vincent and the Grenadines" = "Saint Vincent"))) %>% 
  left_join(latam2) %>% 
  ggplot(aes(x = fct_reorder(country, year),
             y = year)) + 
  geom_col(fill = "grey70", color = "white") + 
  geom_text(aes(label = year), y = 0, hjust = -.25, size = 3.2, fontface = "bold") + 
  geom_text(aes(label = name), y = 4, hjust = 0,  size = 2.5, fontface = "bold") + 
  
  coord_flip() + 
  scale_y_continuous(limits = c(0, 41)) + 
  labs (x = NULL,
        y = "\nYears implementing the longest running CTP",
        fill = "Percentage/existence") + 
  theme_minimal() + 
  theme(legend.position = "right")

ggsave("3_viz/final/g1.png",
       width = 6,
       height = 5,
       dpi = 600)


### G2: preparedness----
l = latam %>% 
  filter(country != "Venezuela",
         country != "Suriname",
         country != "Nicaragua",
         country != "Cuba",
         country != "Saint Lucia",
         country != "Saint Christopher and Nevis") %>% 
  select(country, rs_cob_porc, ctp_perc) %>% 
  mutate(padron_perc = NA_integer_) %>% 
  pivot_longer(-country,
               names_to = "var_num",
               values_to = "values_num") %>% 
  select(-country)

lat = latam %>% 
  filter(country != "Venezuela",
         country != "Suriname",
         country != "Nicaragua",
         country != "Cuba",
         country != "Saint Lucia",
         country != "Saint Christopher and Nevis") %>% 
  select(country, registro, ctp, padron) %>% 
  pivot_longer(-country,
               names_to = "var_col",
               values_to = "values_col") %>% 
  cbind(l) 

lat = as.tibble(lat)

latam %>% 
  mutate(rs_cob_porc = case_when(is.na(rs_cob_porc) ~ 0,
                                 T ~ rs_cob_porc)) %>% 
  summarise(a = mean(rs_cob_porc, na.rm = T))


lat %>% 
  mutate(country = str_replace_all(country, c("Saint Vincent and the Grenadines" = "Saint Vincent"))) %>% 
  ggplot() + 
  geom_tile(aes(x = factor(var_col, order = T,
                           levels = c("registro", "ctp", "padron")),
                y = fct_reorder(country, desc(country)),
                fill = factor(values_col)),
            color = "grey0", size = .7) +
  geom_text(aes(x = factor(var_col, order = T,
                           levels = c("registro", "ctp", "padron")),
                label = percent(values_num, 0.1),
                y = fct_reorder(country, desc(country))),
            color = "grey0",
            fontface = "bold",
            size = 2, show.legend = F) +
  scale_fill_manual(values = c("white", "grey70"), 
                    labels = c("No", "Yes")) +
  scale_x_discrete(labels = c("Social\nregistry",
                              "Ongoing\nCTP",
                              "Single\nbeneficiary\nregistry"), position = "top") +
  labs (x = "",
        y = "",
        fill = "Existence") + 
  theme_minimal() + 
  theme(legend.position = "right")

ggsave("3_viz/final/g2.png",
       width = 6,
       height = 5,
       dpi = 600)


### G3: coverage----

ba

base %>% 
  filter(country != "Venezuela",
         country != "Suriname",
         country != "Nicaragua",
         country != "Cuba",
         country != "Saint Lucia",
         country != "Saint Christopher and Nevis") %>% 
  filter(type_of_measure %in% c("cash transfer", "voucher")) %>% 
  left_join(latam) %>% 
  mutate(country = str_replace_all(country, c(" and the Grenadines" = ""))) %>% 
  mutate(coverage_precovid = case_when(is.na(coverage_precovid) ~ 0,
                                       T ~ coverage_precovid)) %>% 
  mutate(coverage_covid_announced = case_when(is.na(coverage_covid_announced) & is.numeric(coverage_covid_reached) ~ coverage_covid_reached,
                                              T ~ coverage_covid_announced)) %>% 
  mutate(coverage = coverage_covid_announced - coverage_precovid,
         cov_porc = (coverage_covid_announced - coverage_precovid)/population)  %>% 
  filter(coverage != is.na(coverage),
         cov_porc != is.na(cov_porc))  %>% 
  group_by(country) %>% 
  mutate(ord = sum(cov_porc)) %>% 
  ungroup()  %>% 
  # group_by(country) %>% 
  # summarise(ord = sum(cov_porc)) %>% 
  # ungroup() %>% 
  # summarise(ord = mean(ord))
  ggplot(aes(x = fct_reorder(country, ord),
             y = cov_porc)) +
  geom_col(color = "grey0", fill = "grey70") +
  geom_text(aes(x = fct_reorder(country, ord),
                y = ord,
                label = percent(ord, digits = 1)), hjust = -.2, size = 2.8) +
  geom_hline(yintercept = 0.238, color = "grey40", size = .5,
             linetype = "dashed") +
  coord_flip() + 
  scale_y_continuous(label = percent_format(accuracy = 1),
                     breaks = c(seq(0,1.1,.2)), limits = c(0, 1.10)) +
  labs(x = NULL,
       y = "\nCoverage",
       fill = "Tipo de medida")  +
  theme_minimal() +
  theme(axis.text.y = element_markdown(),
        legend.position=c(.8,.4))


ggsave("3_viz/final/g3.jpg",
       width = 8,
       height = 6,
       dpi = 600)

### G4: 3 variables----

ba %>% 
  mutate(country = str_replace_all(country, c("Saint Vincent and the Grenadines" = "Saint Vincent"))) %>% 
  select(country, rs_cob_porc, cov_porc) %>% 
  mutate(a = case_when(rs_cob_porc > cov_porc ~ 1,
                       T ~ 0)) %>% 
  pivot_longer(rs_cob_porc:cov_porc,
               names_to = "var",
               values_to = "values") %>% 
  left_join(b)   %>%
  mutate(year = case_when(country == "Saint Vincent" ~ 0,
                          T ~ year)) %>% 
  ggplot() + 
  geom_point(aes(y = fct_reorder(country, desc(country)),
                 x = values,
                 color = var,
                 shape = var)) + 
  geom_line(aes(y = fct_reorder(country, desc(country)),
                x = values,
                linetype = factor(a)), color = "grey40") + 
  geom_text(aes(y = fct_reorder(country, desc(country)), 
                label = year,
                x = year/39),
            size = 3, fontface = "bold", col = "grey0") + 
  annotate("segment", x = 1, xend = 1, y = "Dominica", yend = "Colombia", colour = "grey0", size = .4, alpha = 1, arrow = arrow(length = unit(2, "mm"))) + 
  annotate("text", x = 1,  y = "Dominican Republic", colour = "grey0", size = 2.4, label = "Past\nexperience") + 
  scale_x_continuous(label = percent_format()) + 
  scale_color_manual(label = c("CTPs", "Social registry"), values = c("grey10", "grey60")) + 
  scale_shape_discrete(label = c("CTPs", "Social registry")) + 
  scale_linetype_discrete(label = c("CTPs > social registry", "Social registry > CTPs")) + 
  labs(y = NULL,
       x = "Coverage (%)",
       color = NULL,
       shape = NULL,
       linetype  = NULL) + 
  theme_minimal() 


ggsave("3_viz/final/g4.png",
       width = 6,
       height = 5,
       dpi = 600)



### G5: coverage & type of innovation----

base %>% 
  left_join(latam) %>% 
  mutate(coverage_precovid = case_when(is.na(coverage_precovid) ~ 0,
                                       T ~ coverage_precovid)) %>% 
  mutate(coverage_covid_announced = case_when(is.na(coverage_covid_announced) & is.numeric(coverage_covid_reached) ~ coverage_covid_reached,
                                              T ~ coverage_covid_announced)) %>% 
  mutate(coverage = coverage_covid_announced - coverage_precovid,
         cov_porc = (coverage_covid_announced - coverage_precovid)/population)  %>% 
  filter(coverage != is.na(coverage),
         cov_porc != is.na(cov_porc))  %>% 
  group_by(country) %>% 
  mutate(ord = sum(cov_porc)) %>% 
  ungroup() %>% 
  left_join(ind)  %>% 
  mutate(country = str_replace_all(country, c(" and the Grenadines" = ""))) %>% 
  ggplot(aes(x = fct_reorder(country, ord),
             y = cov_porc,
             fill = type_of_innovation)) +
  geom_col(color = "grey0") +
  geom_text(aes(x = fct_reorder(country, ord),
                y = ord,
                label = percent(ord, digits = 1)), hjust = -.2, size = 2.8) + 
  geom_text(aes(x = fct_reorder(country, ord),
                label = year),
            y = -.022, inherit.aes = F) + 
  geom_hline(yintercept = 0.238, color = "grey40", size = .5,
             linetype = "dashed") +
  coord_flip() + 
  scale_y_continuous(label = percent_format(accuracy = 1),
                     breaks = c(seq(0,1.1,.2)), limits = c(0, 1.10)) +
  scale_fill_grey(labels = c("Horizontal", "New", "Vertical")) +
  labs(x = NULL,
       y = "\nCoverage",
       fill = "Type of innovation")  +
  theme_minimal() +
  theme(legend.position=c(.8,.4))


ggsave("3_viz/final/g5.jpg",
       width = 8,
       height = 6,
       dpi = 600)

### G6: information to deliver----

base %>% 
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
  mutate(coverage_precovid = case_when(is.na(coverage_precovid) ~ 0,
                                       T ~ coverage_precovid)) %>% 
  mutate(coverage_covid_announced = case_when(is.na(coverage_covid_announced) & is.numeric(coverage_covid_reached) ~ coverage_covid_reached,
                                              T ~ coverage_covid_announced)) %>% 
  left_join(latam) %>% 
  left_join(ind) %>% 
  mutate(coverage = coverage_covid_announced - coverage_precovid,
         cov_porc = (coverage_covid_announced - coverage_precovid)/population) %>% 
  filter(coverage != is.na(coverage),
         cov_porc != is.na(cov_porc))  %>% 
  # filter(type_of_innovation == "new") %>% 
  # count(select)
  group_by(country) %>% 
  mutate(ord = sum(cov_porc)) %>% 
  ungroup() %>% 
  mutate(country = str_replace_all(country, c(" and the Grenadines" = ""))) %>% 
  ggplot(aes(x = fct_reorder(country, ord),
             y = cov_porc,
             fill = factor(select, order = T,
                           c("Previous", "Both", "New", "ND")))) + 
  geom_col(color = "grey0") +
  geom_text(aes(x = fct_reorder(country, ord),
                y = ord,
                label = percent(ord, digits = 1)), hjust = -.2, size = 2.8) + 
  geom_text(aes(x = fct_reorder(country, ord),
                label = year),
            y = -.022, inherit.aes = F) + 
  geom_hline(yintercept = 0.238, color = "grey40", size = .5,
             linetype = "dashed") +
  coord_flip() + 
  scale_y_continuous(label = percent_format(accuracy = 1),
                     breaks = c(seq(0,1.2,.2)), limits = c(0, 1.20)) +
  scale_fill_manual(values = c("grey0", "grey40", "grey80", "white"),labels = c("Only previous", "Both previous & new", "Only new", "ND"),
                    position = "right") +
  labs(x = NULL,
       y = "Coverage\n",
       fill = "Use of information\nto select beneficiaries")  +
  theme_minimal() +
  theme(axis.text.y = element_markdown(),
        legend.position=c(.8,.4))


ggsave("3_viz/final/g6.jpg",
       width = 8,
       height = 6,
       dpi = 600)



### G7: information to deliver----

base %>% 
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
  mutate(coverage_precovid = case_when(is.na(coverage_precovid) ~ 0,
                                       T ~ coverage_precovid)) %>% 
  mutate(coverage_covid_announced = case_when(is.na(coverage_covid_announced) & is.numeric(coverage_covid_reached) ~ coverage_covid_reached,
                                              T ~ coverage_covid_announced)) %>% 
  left_join(latam) %>% 
  left_join(ind) %>% 
  mutate(coverage = coverage_covid_announced - coverage_precovid,
         cov_porc = (coverage_covid_announced - coverage_precovid)/population) %>% 
  filter(coverage != is.na(coverage),
         cov_porc != is.na(cov_porc)) %>% 
  group_by(country) %>% 
  mutate(ord = sum(cov_porc)) %>% 
  ungroup() %>% 
  # filter(type_of_innovation == "new") %>%
  # count(deliver)
  mutate(country = str_replace_all(country, c(" and the Grenadines" = ""))) %>% 
  ggplot(aes(x = fct_reorder(country, ord),
             y = cov_porc,
             fill = factor(deliver, order = T,
                           c("Previous", "Both", "New", "ND")))) + 
  geom_col(color = "grey0") +
  geom_text(aes(x = fct_reorder(country, ord),
                y = ord,
                label = percent(ord, digits = 1)), hjust = -.2, size = 2.8) + 
  geom_text(aes(x = fct_reorder(country, ord),
                label = year),
            y = -.022, inherit.aes = F) + 
  geom_hline(yintercept = 0.238, color = "grey40", size = .5,
             linetype = "dashed") +
  coord_flip() + 
  scale_y_continuous(label = percent_format(accuracy = 1),
                     breaks = c(seq(0,1.2,.2)), limits = c(0, 1.20)) +
  scale_fill_manual(values = c("grey0", "grey40", "grey80", "white"),labels = c("Only previous", "Both previous & new", "Only new", "ND")) +
  
  labs(x = NULL,
       y = "Coverage\n",
       fill = "Use of information\nto deliver the transfer")  +
  theme_minimal() +
  theme(axis.text.y = element_markdown(),
        legend.position=c(.8,.4))

ggsave("3_viz/final/g7.jpg",
       width = 8,
       height = 6,
       dpi = 600)

