library(cowplot)
library(RColorBrewer)
library(openxlsx)
library(readxl)
library(here)
library(dplyr)


Siktdyp_rogaland <- read_xlsx(here("sikt", "sikt_rogaland_2019_2021.xlsx")) %>% 
  mutate(across(Dato, ~ as.Date(as.character(.), format = '%Y-%m-%d'))) %>% 
  tidyr::separate(Dato, c("year", "month", "day"), remove = FALSE) 

Siktdyp_hardangerfjorden<- read_xlsx(here("sikt", "sikt_hardangerfjorden_2017_2021.xlsx")) %>% 
  mutate(across(Dato, ~ as.Date(as.character(.), format = '%Y-%m-%d'))) %>% 
  tidyr::separate(Dato, c("year", "month", "day"), remove = FALSE) 

Siktdyp_sognefjorden <- read_xlsx(here("sikt", "sikt_sognefjorden_2017_2021.xlsx")) %>% 
  mutate(across(Dato, ~ as.Date(as.character(.), format = '%Y-%m-%d'))) %>% 
  tidyr::separate(Dato, c("year", "month", "day"), remove = FALSE) 




Siktdyp_VR48 <- Siktdyp_rogaland %>% dplyr::filter(Stasjon == "VR48") %>% 
  complete(year, nesting(month, day, Stasjon), fill = list(Siktdyp = 0))

Siktdyp_VT8 <- Siktdyp_rogaland %>% dplyr::filter(Stasjon == "VT8") %>% 
  complete(year, nesting(month, day, Stasjon), fill = list(Siktdyp = 0))

Siktdyp_VR49 <- Siktdyp_rogaland %>% dplyr::filter(Stasjon == "VR49") %>% 
  complete(year, nesting(month, day, Stasjon), fill = list(Siktdyp = 0))



Siktdyp_VT16 <- Siktdyp_sognefjorden %>% dplyr::filter(Stasjon == "VT16") %>% 
  complete(year, nesting(month, day, Stasjon), fill = list(Siktdyp = 0))

Siktdyp_VT74 <- Siktdyp_hardangerfjorden %>% dplyr::filter(Stasjon == "VT74") %>% 
  complete(year, nesting(month, day, Stasjon), fill = list(Siktdyp = 0))

Siktdyp_VT79 <- Siktdyp_sognefjorden %>% dplyr::filter(Stasjon == "VT79") %>% 
  complete(year, nesting(month, day, Stasjon), fill = list(Siktdyp = 0))

Siktdyp_VT53 <- Siktdyp_hardangerfjorden %>% dplyr::filter(Stasjon == "VT53") %>% 
  complete(year, nesting(month, day, Stasjon), fill = list(Siktdyp = 0))

Siktdyp_VT70 <- Siktdyp_hardangerfjorden %>% dplyr::filter(Stasjon == "VT70") %>% 
  complete(year, nesting(month, day, Stasjon), fill = list(Siktdyp = 0))




Siktdyp_VR48_plot <- ggplot(Siktdyp_VR48, aes(x = month, y = Siktdyp, fill = year))+
  geom_bar(stat = "identity", position = "dodge")+
  labs(y="Siktdyp", x = "M?ned", fill = "?r", title = "Siktdyp VR48")+
  scale_y_reverse(name = "Siktdyp_VR48") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "grey"))+
  scale_x_discrete(labels = c("Januar", "Februar", "Mars", "April", "Mai", "Juni", "Juli", "August", "September", "Oktober", "November", "Desember"))+
  guides(fill = guide_legend(nrow = 1))+
  scale_fill_brewer(palette = "Paired")
Siktdyp_VR48_plot

ggsave("VR48_hjelmelandsfjorden_Siktdyp_test.png", width = 8, height = 3, dpi=500)

Siktdyp_VT8_plot <- ggplot(Siktdyp_VT8, aes(x = month, y = Siktdyp, fill = year))+
  geom_bar(stat = "identity", position = "dodge")+
  labs(y="Siktdyp", x = "M?ned", fill = "?r", title = "Siktdyp VT8")+
  scale_y_reverse(name = "Siktdyp (m)") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "grey"))+
  scale_x_discrete(labels = c("Januar", "Februar", "Mars", "April", "Mai", "Juni", "Juli", "August", "September", "Oktober", "November", "Desember"))+
  guides(fill = guide_legend(nrow = 1))+
  scale_fill_brewer(palette = "Paired")
Siktdyp_VT8_plot

ggsave("VT8_Siktdyp.png", width = 8, height = 3, dpi=500)

Siktdyp_VR49_plot <- ggplot(Siktdyp_VR49, aes(x = month, y = Siktdyp, fill = year))+
  geom_bar(stat = "identity", position = "dodge")+
  labs(x = "M?ned", fill = "?r", title = "Siktdyp VR49")+
  scale_y_reverse(name = "Siktdyp (m)") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "grey"))+
  scale_x_discrete(labels = c("Januar", "Februar", "Mars", "April", "Mai", "Juni", "Juli", "August", "September", "Oktober", "November", "Desember"))+
  guides(fill = guide_legend(nrow = 1))+
  scale_fill_brewer(palette = "Paired")
Siktdyp_VR49_plot

ggsave("VR49_Siktdyp.png", width = 8, height = 3, dpi=500)

Siktdyp_VR49_plot <- ggplot(Siktdyp_VR49, aes(x = month, y = Siktdyp, fill = year))+
  geom_bar(stat = "identity", position = "dodge")+
  labs(x = "M?ned", fill = "?r", title = "Siktdyp VR49")+
  scale_y_reverse(name = "Siktdyp (m)") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "grey"))+
  scale_x_discrete(labels = c("Januar", "Februar", "Mars", "April", "Mai", "Juni", "Juli", "August", "September", "Oktober", "November", "Desember"))+
  guides(fill = guide_legend(nrow = 1))+
  scale_fill_brewer(palette = "Paired")
Siktdyp_VR49_plot

ggsave("VR49_Siktdyp.png", width = 8, height = 3, dpi=500)

Siktdyp_VR49_plot <- ggplot(Siktdyp_VR49, aes(x = month, y = Siktdyp, fill = year))+
  geom_bar(stat = "identity", position = "dodge")+
  labs(x = "M?ned", fill = "?r", title = "Siktdyp VR49")+
  scale_y_reverse(name = "Siktdyp (m)") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "grey"))+
  scale_x_discrete(labels = c("Januar", "Februar", "Mars", "April", "Mai", "Juni", "Juli", "August", "September", "Oktober", "November", "Desember"))+
  guides(fill = guide_legend(nrow = 1))+
  scale_fill_brewer(palette = "Paired")
Siktdyp_VR49_plot

ggsave("VR49_Siktdyp.png", width = 8, height = 3, dpi=500)

Siktdyp_VT16_plot <- ggplot(Siktdyp_VT16, aes(x = month, y = Siktdyp, fill = year))+
  geom_bar(stat = "identity", position = "dodge")+
  labs(x = "M?ned", fill = "?r", title = "Siktdyp VT16")+
  scale_y_reverse(name = "Siktdyp (m)") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "grey"))+
  scale_x_discrete(labels = c("Januar", "Februar", "Mars", "April", "Mai", "Juni", "Juli", "August", "September", "Oktober", "November", "Desember"))+
  guides(fill = guide_legend(nrow = 1))+
  scale_fill_brewer(palette = "Paired")
Siktdyp_VT16_plot

ggsave("VT16_Siktdyp.png", width = 8, height = 3, dpi=500)
   
Siktdyp_VT79_plot <- ggplot(Siktdyp_VT79, aes(x = month, y = Siktdyp, fill = year))+
  geom_bar(stat = "identity", position = "dodge")+
  labs(x = "M?ned", fill = "?r", title = "Siktdyp VT79")+
  scale_y_reverse(name = "Siktdyp (m)") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "grey"))+
  scale_x_discrete(labels = c("Januar", "Februar", "Mars", "April", "Mai", "Juni", "Juli", "August", "September", "Oktober", "November", "Desember"))+
  guides(fill = guide_legend(nrow = 1))+
  scale_fill_brewer(palette = "Paired")
Siktdyp_VT79_plot

ggsave("VT79_Siktdyp.png", width = 8, height = 3, dpi=500)


Siktdyp_VT70_plot <- ggplot(Siktdyp_VT70, aes(x = month, y = Siktdyp, fill = year))+
  geom_bar(stat = "identity", position = "dodge")+
  labs(x = "M?ned", fill = "?r", title = "Siktdyp VT70")+
  scale_y_reverse(name = "Siktdyp (m)") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "grey"))+
  scale_x_discrete(labels = c("Januar", "Februar", "Mars", "April", "Mai", "Juni", "Juli", "August", "September", "Oktober", "November", "Desember"))+
  guides(fill = guide_legend(nrow = 1))+
  scale_fill_brewer(palette = "Paired")
Siktdyp_VT70_plot

ggsave("VT70_Siktdyp.png", width = 8, height = 3, dpi=500)


Siktdyp_VT53_plot <- ggplot(Siktdyp_VT53, aes(x = month, y = Siktdyp, fill = year))+
  geom_bar(stat = "identity", position = "dodge")+
  labs(x = "M?ned", fill = "?r", title = "Siktdyp VT53")+
  scale_y_reverse(name = "Siktdyp (m)") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "grey"))+
  scale_x_discrete(labels = c("Januar", "Februar", "Mars", "April", "Mai", "Juni", "Juli", "August", "September", "Oktober", "November", "Desember"))+
  guides(fill = guide_legend(nrow = 1))+
  scale_fill_brewer(palette = "Paired")
Siktdyp_VT53_plot

ggsave("VT53_Siktdyp.png", width = 8, height = 3, dpi=500)

Siktdyp_VT74_plot <- ggplot(Siktdyp_VT74, aes(x = month, y = Siktdyp, fill = year))+
  geom_bar(stat = "identity", position = "dodge")+
  labs(x = "M?ned", fill = "?r", title = "Siktdyp VT74")+
  scale_y_reverse(name = "Siktdyp (m)") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "grey"))+
  scale_x_discrete(labels = c("Januar", "Februar", "Mars", "April", "Mai", "Juni", "Juli", "August", "September", "Oktober", "November", "Desember"))+
  guides(fill = guide_legend(nrow = 1))+
  scale_fill_brewer(palette = "Paired")
Siktdyp_VT74_plot

ggsave("VT74_Siktdyp.png", width = 8, height = 3, dpi=500)





