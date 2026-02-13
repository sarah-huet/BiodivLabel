# A plot to synthesize results for the public presentation of the collective expertise
# HUET Sarah
# 25-03-31



# comp_CONTRA-BVIAS_v1 ----

library(readxl)
results_comparison_CONTRA_BVIAS <- read_excel("C:/Users/srhuet/Nextcloud/Etude biodivlabel/Colloque/Sous-groupes de présentation/Pistes Méthodos/old/results_comparison_CONTRA-BVIAS.xlsx",sheet = "Feuille3")

library(dplyr)
library(tidyr)
tmp_plot_data <- results_comparison_CONTRA_BVIAS %>%
  mutate(CONTRA = CONTRA) %>%
  pivot_longer(.,cols = c("CONTRA","BVIAS_ha","BVIAS_kcal"),
               names_to = "method",values_to = "values") %>%
  mutate(plot_x = paste0(method,"_",Produit,"_",label)) %>%
  mutate(
    sd = case_when(
      method == "BVIAS_ha" ~ sd_ha,
      method == "BVIAS_t" ~ sd_t,
      method == "BVIAS_kcal" ~ sd_kcal,
      .default = NA
    )
  ) %>% filter(!(Produit %in% c("Prairies de plaine")))

library(ggplot2)
library(ggrepel)


tmp_plot_data %>%
  filter(!is.na(values)) %>%
  ggplot() +
  geom_point(aes(x = plot_x, y = values,colour = Produit),size = 7) +
  geom_errorbar(aes(x = plot_x, ymin = values - sd, ymax = values + sd,colour = Produit),
                linewidth=1,width = 0.5,position = position_dodge(width = 0.75)) +
  #geom_line(aes(x = plot_x, y = values,group = label), colour = "#999999",linewidth = 2) +
  #geom_line(aes(x = plot_x, y = values,group = c(Produit)), colour = "#999999",linewidth = 2) +
  #geom_text(aes(x = plot_x, y = 1.2, label = label), angle = 90, hjust = 1) +
  #geom_text_repel(aes(x = plot_x, y = values,label = label),                  angle = -90,size = 5, point.padding = 2)+
  # theme
  #scale_y_continuous(c(0,1)) +
  theme_light() +
  theme(#axis.text = element_blank(),
    axis.text.x = element_text(angle = 90),
    axis.title = element_blank(),
    strip.text = element_blank(),
    legend.position = "bottom") +
  scale_color_brewer(palette = "RdBu", direction = 1) +
  facet_wrap(vars(method),scales = "free")

# comp_CONTRA-BVIAS_v2 ----

library(readxl)
results_comparison_CONTRA_BVIAS <- read_excel("C:/Users/srhuet/Nextcloud/Etude biodivlabel/Colloque/Sous-groupes de présentation/Pistes Méthodos/old/results_comparison_CONTRA-BVIAS.xlsx",sheet = "Feuille3")

library(dplyr)
library(tidyr)
tmp_plot_data <- results_comparison_CONTRA_BVIAS %>%
  mutate(CONTRA = CONTRA/100,
         BVIAS_ha = 1 - BVIAS_ha,
         BVIAS_t = 1 - BVIAS_t) %>%
  pivot_longer(.,cols = c("CONTRA","BVIAS_ha","BVIAS_kcal"),
               names_to = "method",values_to = "values") %>%
  mutate(plot_x = paste0(method,"_",Produit,"_",label)) %>%
  mutate(
    sd = case_when(
      method == "BVIAS_ha" ~ sd_ha,
      method == "BVIAS_t" ~ sd_t,
      .default = NA
    )
  ) #%>% filter(!(Produit %in% c("Prairies de plaine")))

library(ggplot2)
library(ggrepel)


tmp_plot_data %>%
  filter(!is.na(values)) %>%
  ggplot() +
  geom_point(aes(x = plot_x, y = values,colour = Produit),size = 7) +
  geom_errorbar(aes(x = plot_x, ymin = values - sd, ymax = values + sd,colour = Produit),
                linewidth=1,width = 0.5,position = position_dodge(width = 0.75)) +
  #geom_line(aes(x = plot_x, y = values,group = label), colour = "#999999",linewidth = 2) +
  #geom_line(aes(x = plot_x, y = values,group = c(Produit)), colour = "#999999",linewidth = 2) +
  #geom_text(aes(x = plot_x, y = 1.2, label = label), angle = 90, hjust = 1) +
  #geom_text_repel(aes(x = plot_x, y = values,label = label),                  angle = -90,size = 5, point.padding = 2)+
  # theme
  scale_y_continuous(c(0,1)) +
  theme_light() +
  theme(#axis.text = element_blank(),
    axis.text.x = element_text(angle = 90),
    axis.title = element_blank(),
    strip.text = element_blank(),
    legend.position = "bottom") +
  scale_color_brewer(palette = "RdBu", direction = 1) +
  facet_wrap(vars(method),scales = "free")



