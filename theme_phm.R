library(tidyverse)
library(ggthemes)
library(baseph)
library(ggsci)


theme_phm <- function(base_size = 11, base_family = "", legend = "right") {
  theme_light() %+replace%
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title.x = element_text(size = 12),
      legend.title = element_text(size = 12),
      axis.title.y = element_text(
        size = 12,
        angle = 90,
        vjust = .5
      ),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      legend.position = legend
    )
}

patients |>
  drop_na(admission) |>
  ggplot() +
  aes(x = admission, y = igs2, fill = admission) +
  geom_violin() +
  labs(title = "Test pour un beau dessin") +
  geom_boxplot(width = 0.2, fill = "grey90") +
#  scale_fill_atlassian() +
  scale_y_continuous(limits = c(0, 115), breaks = seq(0, 110, 20), expand = 0) +
  theme_phm(legend = "top")


patients |>
  mutate(igs2 = cut(igs2, seq(0, 100, 10))) |>
  ggplot() +
  aes(fill = sexe, y = igs2) +
  geom_diverging() +
  theme_light() +
  scale_x_continuous(breaks = c(-2, 2), labels = c("Femmes", "Hommes"))
