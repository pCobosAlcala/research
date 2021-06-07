### Paquetes ----
pacman::p_load(tidyverse, scales, ggrepel)

## Setup
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Mac
Sys.setlocale("LC_ALL", "Spanish_Mexico.1252") # Windows
options(scipen=999) 

## Eliminar objetos
rm(list = ls())


### Data----
base <- readxl::read_excel("1_data/elec_cdmx.xlsx")


base %>%
  select(-aaa) %>% 
  mutate(dif = porc_2021 - porc_2018) %>% 
  mutate(dif2 = porc_2021>porc_2018) %>% 
  mutate(gano = str_c(gano_2018, gano_2021)) %>% 
  select(-gano_2018, -gano_2021) %>% 
  pivot_longer(porc_2018:porc_2021,
               names_to ="var",
               values_to = "values") %>% 
  ggplot(aes(x = var,
             y = values,
             col = dif2,
             group = demarcacion)) + 
  geom_line() + 
  geom_point() + 
  geom_text_repel(aes(label = demarcacion)) +
  theme_minimal()
  scale_color_manual(values = c("red4", "steelblue1", "red2", "steelblue4"))


base %>% 
  mutate(dif = porc_2021 - porc_2018) %>% 
  mutate(gano = str_c(gano_2018, gano_2021)) %>% 
  ggplot(aes(x = fct_reorder(demarcacion,dif), y = dif, fill = gano)) + 
  geom_col() + 
  coord_flip() +
  scale_fill_manual(values = c("steelblue4", "red1", "steelblue2", "red4"))


base %>% 
  mutate(gano = str_c(gano_2018, gano_2021)) %>% 
  select(demarcacion, porc_2018, porc_2021, gano) %>% 
  ggplot(aes(x = porc_2018,
             y = porc_2021,
             color = factor(gano,
                            order = T,
                            levels = c("sisi",
                                       "nosi",
                                       "sino",
                                       "nono")))) +
  geom_abline(intercept = 0) + 
  geom_point() + 
  geom_text_repel(aes(label = demarcacion), show.legend = F, size = 3.5) + 
  annotate("segment", x = .23, xend = .17, y = .2, yend = .25, colour = "grey0", size = 2, alpha = 1, arrow = arrow(length = unit(4, "mm"))) + 
  annotate("segment", x = .23, xend = .27, y = .2, yend = .17, colour = "grey0", size = 2, alpha = 1, arrow = arrow(length = unit(4, "mm"))) + 
  annotate("text", x = .18,  y = .29, colour = "grey0", size =3, label = "Más % en 2021\nque en 2018") +
  annotate("text", x = .22,  y = .15, colour = "grey0", size = 3, label = "Más % en 2018\nque en 2021") +
  scale_color_manual(values = c("brown4", "brown1", "steelblue2", "steelblue4"),
                     labels = c("Ganó 2018-Ganó 2021",
                                "Perdió 2018-Ganó 2021",
                                "Ganó 2018-Perdió 2021",
                                "Perdió 2018-Perdió 2021")) +
  scale_x_continuous(label = percent_format(accuracy = 1), limits = c(.15,.6)) + 
  scale_y_continuous(label = percent_format(accuracy = 1), limits = c(.15,.6)) + 
  theme_minimal() +
  labs(title = "Diferencia de % de votos por Morena y coalición",
       subtitle = "Alcaldías de la CDMX, 2018 vs. 2021",
       x = "\n% de Morena y coalición en 2018",
       y = "% de Morena y coalición en 2021\n",
       col = "Morena...",
       caption = "Nota: los resultados de 2021 son con el PREP al 98%\n\nElaborado por @pCobosAlcala") +
  theme(plot.title = element_text(size = 22),
        legend.position = "top",
    title = element_text(size = 14, face = "bold"),
        
        plot.caption = element_text(hjust = 0)) +
  guides(colour = guide_legend(override.aes = list(size = 4)),
         shape = guide_legend(override.aes = list(size = 4)))


ggsave("3_viz/elec.jpg",
       width = 8.7,
       height = 6,
       dpi = 600)



