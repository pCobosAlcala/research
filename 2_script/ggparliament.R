### Paquetes ----
pacman::p_load(tidyverse, scales, ggtext, ggparliament)

## Setup
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Mac
Sys.setlocale("LC_ALL", "Spanish_Mexico.1252") # Windows
options(scipen=999) 

## Eliminar objetos
rm(list = ls())


### Data----

brks <- c(0, 1929, 1946, 1965, 1981, 1997, 2020)

## Población
pob <- read_csv("1_data/pob_mit_proyecciones.csv", 
                locale = locale(encoding = "ISO-8859-1")) %>% 
  janitor::clean_names()

pob = pob %>% 
  filter(entidad == "República Mexicana") %>% 
  filter(ano == 2018) %>% 
  group_by(edad) %>% 
  summarise(sum = sum(poblacion)) %>% 
  ungroup %>% 
  filter(edad >= 21)

#
pob_d = pob %>% 
  rename(n = sum,
         anos = edad) %>% 
  mutate(anos = as.numeric(anos),
         anos = 2020-anos) %>% 
  mutate(cut = cut(anos,
                   breaks = brks,
                   right = F)) %>% 
  group_by(cut) %>% 
  summarise(seats = sum(n)) %>% 
  ungroup() %>% 
  mutate(a = sum(seats),
         seats = round((seats/a)*500)) %>% 
  mutate(seats = case_when(seats == 41 ~ 42,
                           T ~ seats)) %>% 
  select(-a)

#
pob <- read_csv("1_data/pob_mit_proyecciones.csv", 
                locale = locale(encoding = "ISO-8859-1")) %>% 
  janitor::clean_names()

pob = pob %>% 
  filter(entidad == "República Mexicana") %>% 
  filter(ano == 2018) %>% 
  group_by(edad) %>% 
  summarise(sum = sum(poblacion)) %>% 
  ungroup %>% 
  filter(edad >= 25)

pob_s = pob %>% 
  rename(n = sum,
         anos = edad) %>% 
  mutate(anos = as.numeric(anos),
         anos = 2020-anos) %>% 
  mutate(cut = cut(anos,
                   breaks = brks,
                   right = F)) %>%
  group_by(cut) %>% 
  summarise(seats = sum(n)) %>% 
  ungroup() %>% 
  mutate(a = sum(seats),
         seats = round((seats/a)*128)) %>% 
  mutate(seats = case_when(seats == 0 ~ 1,
                           T ~ seats)) %>% 
  select(-a)


#
pob_t = pob %>% 
  rename(n = sum,
         anos = edad) %>% 
  mutate(anos = as.numeric(anos),
         anos = 2020-anos) %>% 
  mutate(cut = cut(anos,
                   breaks = brks,
                   right = F)) %>%
  group_by(cut) %>% 
  summarise(seats = sum(n)) %>% 
  ungroup() %>% 
  mutate(a = sum(seats),
         # seats = (seats/a)*628)
         seats = round((seats/a)*628)) %>% 
  mutate(seats = case_when(seats == 27 ~ 26,
                           T ~ seats)) %>% 
  select(-a)

## Dips
diputades <- readxl::read_excel("1_data/diputades.xlsx", sheet = "por_edad") %>% 
  janitor::clean_names()

dips = diputades %>% 
  filter(anos != "Total") %>% 
  select(-total) %>% 
  pivot_longer(-anos, names_to = "var", values_to = "values") %>% 
  group_by(anos) %>% 
  summarise(n = sum(values, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(anos = as.numeric(anos),
         anos = 2020-anos) %>% 
  mutate(cut = cut(anos,
                   breaks = brks,
                   right = F)) %>%
  group_by(cut) %>% 
  summarise(seats = sum(n)) %>% 
  ungroup()

## sens
senadores <- readxl::read_excel("1_data/senadores.xlsx") %>% 
  janitor::clean_names()

sens = senadores %>% 
  mutate(anos = lubridate::year(fecha_de_nacimiento)) %>% 
  count(anos) %>% 
  mutate(cut = cut(anos,
                   breaks = brks,
                   right = F)) %>%
  group_by(cut) %>% 
  summarise(seats = sum(n)) %>% 
  ungroup()



### diputades mínimo 21 años
### senadores mínimo 25 años


### Data----

pob_d
pob_s
pob_t
dips
sens


glimpse(pob)
glimpse(diputades)
glimpse(senadores)

year
country
house
party_long
party_short
seats
colour


brks <- c(0, 1929, 1946, 1965, 1981, 1997, 2020)


###

us_senate = dips %>% 
  mutate(year = 2018,
         country = "Mexico",
         house = "Diputados",
         party_long = case_when(cut == "[1.93e+03,1.95e+03)" ~ "**Silent** (1929-1945)",
                                 cut == "[1.95e+03,1.96e+03)" ~ "**Boomers** (1946-1964)",
                                 cut == "[1.96e+03,1.98e+03)" ~ "**Gen-X** (1965-1980)",
                                 cut == "[1.98e+03,2e+03)" ~ "**Milenials** (1981-1996)"),
         party_short = case_when(cut == "[1.93e+03,1.95e+03)" ~ "Silent",
                            cut == "[1.95e+03,1.96e+03)" ~ "Boomers",
                            cut == "[1.96e+03,1.98e+03)" ~ "Gen-X",
                            cut == "[1.98e+03,2e+03)" ~ "Milenials"),
         seats = seats, 
         government = 1,
         colour = case_when(cut == "[1.93e+03,1.95e+03)" ~ "#d95f02",
                            cut == "[1.95e+03,1.96e+03)" ~ "#7570b3",
                            cut == "[1.96e+03,1.98e+03)" ~ "#e7298a",
                            cut == "[1.98e+03,2e+03)" ~ "#66a61e")) %>% 
  select(year, country, house, party_long, party_short, seats, government, colour)

us_senate = as_data_frame(us_senate)

us_senate <- us_senate %>%
  filter(country == "Mexico" &
           year == 2018 &
           house == "Diputados")

us_senate <- parliament_data(
  election_data = us_senate,
  type = "semicircle",
  parl_rows = 10,
  party_seats = us_senate$seats)

ggplot(us_senate, aes(x, y, colour = party_long)) +
  geom_parliament_seats() + 
  geom_parliament_bar(colour = colour, party = party_long) + 
  labs(colour = "Generación (año de nacimiento)", 
       title = "**Generación de representantes en la Cámara de Diputades de México**",
       subtitle = "LXIV Legislatura a finales de 2020",
       caption = "Datos otorgados por solicitud de información | Elaborado por @pCobosAlcala") +
  scale_colour_manual(values = us_senate$colour,
                      limits = us_senate$party_long) + 
  theme_ggparliament(legend = T) +
  guides(colour = guide_legend(override.aes = list(size = 8))) + 
  theme(plot.caption = element_text(hjust = 0),
        legend.text = element_markdown(size = 14),
        legend.position = "right",
        plot.title = element_markdown(size = 17))

ggsave("3_viz/gens/1.jpg",
       width = 8,
       height = 4.5,
       dpi = 600)




## Sens
us_senate = sens %>% 
  mutate(year = 2018,
         country = "Mexico",
         house = "Diputados",
         party_long = case_when(cut == "[0,1.93e+03)" ~ "**Gran generación** (1901-1928)",
                                cut == "[1.93e+03,1.95e+03)" ~ "**Silent** (1929-1945)",
                                cut == "[1.95e+03,1.96e+03)" ~ "**Boomers** (1946-1964)",
                                cut == "[1.96e+03,1.98e+03)" ~ "**Gen-X** (1965-1980)",
                                cut == "[1.98e+03,2e+03)" ~ "**Milenials** (1981-1996)"),
         party_short = case_when(cut == "[0,1.93e+03)" ~ "Gran generación (1901-1928)",
                                 cut == "[1.93e+03,1.95e+03)" ~ "Silent",
                                 cut == "[1.95e+03,1.96e+03)" ~ "Boomers",
                                 cut == "[1.96e+03,1.98e+03)" ~ "Gen-X",
                                 cut == "[1.98e+03,2e+03)" ~ "Milenials"),
         seats = seats, 
         government = 1,
         colour = case_when(cut == "[0,1.93e+03)" ~ "#1b9e77",
                            cut == "[1.93e+03,1.95e+03)" ~ "#d95f02",
                            cut == "[1.95e+03,1.96e+03)" ~ "#7570b3",
                            cut == "[1.96e+03,1.98e+03)" ~ "#e7298a",
                            cut == "[1.98e+03,2e+03)" ~ "#66a61e")) %>% 
  select(year, country, house, party_long, party_short, seats, government, colour)

us_senate = as_data_frame(us_senate)

us_senate <- us_senate %>%
  filter(country == "Mexico" &
           year == 2018 &
           house == "Diputados")

us_senate <- parliament_data(
  election_data = us_senate,
  type = "semicircle",
  parl_rows = 4,
  party_seats = us_senate$seats)

ggplot(us_senate, aes(x, y, colour = party_long)) +
  geom_parliament_seats() + 
  geom_parliament_bar(colour = colour, party = party_long) + 
  labs(colour = "Generación (año de nacimiento)", 
       title = "**Generación de representantes en la Cámara de Senadores de México**",
       subtitle = "LXIV Legislatura a finales de 2020",
       caption = "Datos otorgados por solicitud de información | Elaborado por @pCobosAlcala") +
  scale_colour_manual(values = us_senate$colour,
                      limits = us_senate$party_long) + 
  theme_ggparliament(legend = T) +
  guides(colour = guide_legend(override.aes = list(size = 8))) + 
  theme(plot.caption = element_text(hjust = 0),
        legend.text = element_markdown(size = 14),
        legend.position = "right",
        plot.title = element_markdown(size = 17))

ggsave("3_viz/gens/2.jpg",
       width = 8.5,
       height = 4,
       dpi = 600)


## Dips pob
us_senate = pob_d %>% 
  mutate(year = 2018,
         country = "Mexico",
         house = "Diputados",
         party_long = case_when(cut == "[0,1.93e+03)" ~ "**Gran generación** (1901-1928)",
                                cut == "[1.93e+03,1.95e+03)" ~ "**Silent** (1929-1945)",
                                cut == "[1.95e+03,1.96e+03)" ~ "**Boomers** (1946-1964)",
                                cut == "[1.96e+03,1.98e+03)" ~ "**Gen-X** (1965-1980)",
                                cut == "[1.98e+03,2e+03)" ~ "**Milenials** (1981-1996)",
                                cut == "[2e+03,2.02e+03)" ~ "**Gen-Z** (>1996)"),
         party_short = case_when(cut == "[0,1.93e+03)" ~ "Gran generación (1901-1928)",
                                 cut == "[1.93e+03,1.95e+03)" ~ "Silent",
                                 cut == "[1.95e+03,1.96e+03)" ~ "Boomers",
                                 cut == "[1.96e+03,1.98e+03)" ~ "Gen-X",
                                 cut == "[1.98e+03,2e+03)" ~ "Milenials",
                                 cut == "[2e+03,2.02e+03)" ~ "**Gen-Z** (>1996)"),
         seats = seats, 
         government = 1,
         colour = case_when(cut == "[0,1.93e+03)" ~ "#1b9e77",
                            cut == "[1.93e+03,1.95e+03)" ~ "#d95f02",
                            cut == "[1.95e+03,1.96e+03)" ~ "#7570b3",
                            cut == "[1.96e+03,1.98e+03)" ~ "#e7298a",
                            cut == "[1.98e+03,2e+03)" ~ "#66a61e",
                            cut == "[2e+03,2.02e+03)" ~ "#ffd92f")) %>% 
  select(year, country, house, party_long, party_short, seats, government, colour)

us_senate = as_data_frame(us_senate)

us_senate <- us_senate %>%
  filter(country == "Mexico" &
           year == 2018 &
           house == "Diputados")

us_senate <- parliament_data(
  election_data = us_senate,
  type = "semicircle",
  parl_rows = 10,
  party_seats = us_senate$seats)

ggplot(us_senate, aes(x, y, colour = party_long)) +
  geom_parliament_seats() + 
  geom_parliament_bar(colour = colour, party = party_long) + 
  labs(colour = "Generación (año de nacimiento)", 
       title = "**Generación de pertenencia de representantes en la Cámara de Diputades de México<br>si fuera representativa de la población elegible en 2018**",
       # subtitle = "LXIV Legislatura al inicio",
       caption = "Datos otorgados por solicitud de información | Elaborado por @pCobosAlcala") +
  scale_colour_manual(values = us_senate$colour,
                      limits = us_senate$party_long) + 
  theme_ggparliament(legend = T) +
  guides(colour = guide_legend(override.aes = list(size = 8))) + 
  theme(plot.caption = element_text(hjust = 0),
        legend.text = element_markdown(size = 14),
        legend.position = "right",
        plot.title = element_markdown(size = 19))

ggsave("3_viz/gens/3.jpg",
       width = 12,
       height = 5.5,
       dpi = 600)


## Dips pob
us_senate = pob_s %>% 
  mutate(year = 2018,
         country = "Mexico",
         house = "Diputados",
         party_long = case_when(cut == "[0,1.93e+03)" ~ "**Gran generación** (1901-1928)",
                                cut == "[1.93e+03,1.95e+03)" ~ "**Silent** (1929-1945)",
                                cut == "[1.95e+03,1.96e+03)" ~ "**Boomers** (1946-1964)",
                                cut == "[1.96e+03,1.98e+03)" ~ "**Gen-X** (1965-1980)",
                                cut == "[1.98e+03,2e+03)" ~ "**Milenials** (1981-1996)",
                                cut == "[2e+03,2.02e+03)" ~ "**Gen-Z** (>1996)"),
         party_short = case_when(cut == "[0,1.93e+03)" ~ "Gran generación (1901-1928)",
                                 cut == "[1.93e+03,1.95e+03)" ~ "Silent",
                                 cut == "[1.95e+03,1.96e+03)" ~ "Boomers",
                                 cut == "[1.96e+03,1.98e+03)" ~ "Gen-X",
                                 cut == "[1.98e+03,2e+03)" ~ "Milenials",
                                 cut == "[2e+03,2.02e+03)" ~ "**Gen-Z** (>1996)"),
         seats = seats, 
         government = 1,
         colour = case_when(cut == "[0,1.93e+03)" ~ "#1b9e77",
                            cut == "[1.93e+03,1.95e+03)" ~ "#d95f02",
                            cut == "[1.95e+03,1.96e+03)" ~ "#7570b3",
                            cut == "[1.96e+03,1.98e+03)" ~ "#e7298a",
                            cut == "[1.98e+03,2e+03)" ~ "#66a61e",
                            cut == "[2e+03,2.02e+03)" ~ "#ffd92f")) %>% 
  select(year, country, house, party_long, party_short, seats, government, colour)

us_senate = as_data_frame(us_senate)

us_senate <- us_senate %>%
  filter(country == "Mexico" &
           year == 2018 &
           house == "Diputados")

us_senate <- parliament_data(
  election_data = us_senate,
  type = "semicircle",
  parl_rows = 4,
  party_seats = us_senate$seats)

ggplot(us_senate, aes(x, y, colour = party_long)) +
  geom_parliament_seats() + 
  geom_parliament_bar(colour = colour, party = party_long) + 
  labs(colour = "Generación (año de nacimiento)", 
       title = "**Generación de pertenencia de representantes en la Cámara de Senadores de México<br>si fuera representativa de la población elegible en 2018**",
       # subtitle = "LXIV Legislatura al inicio\n",
       caption = "Datos otorgados por solicitud de información | Elaborado por @pCobosAlcala") +
  scale_colour_manual(values = us_senate$colour,
                      limits = us_senate$party_long) + 
  theme_ggparliament(legend = T) +
  guides(colour = guide_legend(override.aes = list(size = 8))) + 
  theme(plot.caption = element_text(hjust = 0),
        legend.text = element_markdown(size = 14),
        legend.position = "right",
        plot.title = element_markdown(size = 17))

ggsave("3_viz/gens/4.jpg",
       width = 10,
       height = 5,
       dpi = 600)




pob25 = pob %>% 
  filter(edad >= 25)


pob21 = pob %>% 
  filter(edad >= 21)


dip = diputades %>% 
  filter(anos != "Total") %>% 
  select(-total) %>% 
  pivot_longer(-anos, names_to = "var", values_to = "values") %>% 
  group_by(anos) %>% 
  summarise(n = sum(values, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(anos = as.numeric(anos),
         anos = 2020-anos)


sen = senadores %>% 
  mutate(anos = lubridate::year(fecha_de_nacimiento)) %>% 
  count(anos)


pob25 = pob25 %>% 
  mutate(gen = "pob25")

pob21 = pob25 %>% 
  mutate(gen = "pob21")

p = rbind(pob25, pob21)

p = p %>% 
  rename(anos = edad,
         n = sum)


###

pob_d = pob_d %>% 
  mutate(gen = "pob_d")
pob_s = pob_s %>% 
  mutate(gen = "pob_s")
sens = sens %>% 
  mutate(gen = "pob_e")
dips = dips %>% 
  mutate(gen = "dips")

base = rbind(pob_d, pob_s, sens, dips)

base %>% 
  group_by(gen) %>% 
  mutate(total = sum(seats)) %>% 
  ungroup() %>% 
  mutate(porc = seats / total) %>% 
  select(cut, gen, porc) %>% 
  ggplot(aes(x = gen,
             y = porc, fill = cut)) + 
  geom_col() + 
  geom_text(aes(label = percent(porc, accuracy = 0.1)), position = position_fill(vjust = 0.5), size = 2.6) +
  scale_x_discrete(labels = c("Diputades\n2020", "Diputades\nsi fuera\nrepresentativa",
                              "Senadores\n2020", "Senadores\nsi fuera\nrepresentativa")) +
  scale_y_continuous(label = percent_format()) + 
  scale_fill_manual(values = c("#1b9e77","#d95f02", "#7570b3", "#e7298a","#66a61e","#ffd92f"), labels = c("**Gran generación** (1901-1928)", "**Silent** (1929-1945)", "**Boomers** (1946-1964)", "**Gen-X** (1965-1980)", "**Milenials** (1981-1996)", "**Gen-Z** (>1996)")) + 
  labs(colour = "Generación (año de nacimiento)", 
       title = "**Generación de representantes en México**",
       fill = "Generación (año de nacimiento)",
       x = "",
       y = NULL,
       subtitle = "LXIV Legislatura a finales de 2020",
       caption = "La representatividad considera que para ser representante hay que tener 21 años cumplidos el día de la elección (25 para el Senado).\n\nDatos otorgados por solicitud de información | Elaborado por @pCobosAlcala") +
  theme_minimal() + 
  guides(colour = guide_legend(override.aes = list(size = 8))) + 
  theme(plot.caption = element_text(hjust = 0),
        legend.text = element_markdown(size = 14),
        legend.position = "right",
        plot.title = element_markdown(size = 20))


ggsave("3_viz/gens/5.jpg",
       width = 8,
       height = 6,
       dpi = 600)

