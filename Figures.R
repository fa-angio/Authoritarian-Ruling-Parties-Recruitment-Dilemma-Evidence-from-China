library(ggplot2)
library(dplyr)
library(ggrepel)
library(hrbrthemes) # for theme_ipsum
library(here)
library(wesanderson)
## PLOTS!

# Party - State convergence

stateparty <- database_state_party
value <- as.factor(stateparty$value)
year <- as.factor(stateparty$year)
year1 <- stateparty$year

urban <- stateparty %>%
  filter(value %in% c( "labor workers", "white collars"))


urb <- urban %>%
  ggplot(., aes(x = partypercent, y = statepercent,  color = value, shape = value)) +
  geom_point() +
  geom_smooth(aes(color = value, fill = value), method = "lm") +
  scale_y_continuous(breaks = seq(0, 100, 5)) +
  scale_x_continuous(breaks = seq(0, 100, 5)) +
  geom_label_repel(aes(label = year,
                       fill = (value)), color = 'white',
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  geom_segment(aes(x = 15, y = 60, xend = 8, yend = 52),
               arrow = arrow(length = unit(0.5, "cm")),
               colour = "#F8766D") +
  geom_segment(aes(x = 25, y = 25, xend = 34, yend = 35),
               arrow = arrow(length = unit(0.5, "cm")),
               colour = "#00BFC4") +
  labs(y = "% Chinese Workforce", x = "% CCP membership") +
  theme_light() +
 theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15),
        legend.position="bottom")

ggsave(filename = here("Figures/Fig1.pdf"),
       dpi = "retina",
       width = 8,
       height = 6)

## Figure 2
CCP <-  CCP_members_2000_2015

p2 <- ggplot(CCP, aes(fill=decision, y=value, x=Year)) + 
  geom_bar(position="stack", stat="identity") +
  scale_y_continuous(breaks = seq(0, 24000, 1000)) +
  scale_x_continuous(breaks = seq(2000, 2015, 1)) +
  scale_fill_manual(values = wes_palette("Cavalcanti1", n = 2)) +
  labs(y = "Refused and Accepted (in thousands) to the CCP", x = "Years") +
  theme_light() +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15),
        legend.position="bottom") +
  guides(fill = guide_legend(title="Legend", reverse=TRUE))

ggsave(filename = here("Figures/Fig2.pdf"),
       dpi = "retina",
       width = 8,
       height = 6)