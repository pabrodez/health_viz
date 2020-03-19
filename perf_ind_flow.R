##########################################################################################
# This script generates alluvial plots from the performance indicators spreadsheet
# It sources ./perf_ind/perf_ind_script.R and saves the plots
# in ./perf_ind
# It also sources ./flexdashboard/bpt19.R to allow merging of BPT and performance datasets
# It also contains other ideas for visualization
#########################################################################################

#### libraries ####
library(tidyverse)
library(lubridate)
library(Cairo)
library(glue)
library(scales)
library(patchwork)
library(ggalluvial) 
library(ggTimeSeries)
library(ggdark)
library(ggthemes)
library(ggrepel)
library(ggforce)
library(ggtext)  ## devtools::install_github("clauswilke/ggtext")
library(cowplot)
library(rlang) 
library(rcartocolor)
library(forcats)
library(survival)
library(survminer)
library(ggridges)

#### source ./perf_ind/perf_ind_script.R, ./flexdashboard/bpt19.R and perf_ind_1418_script.R ####
source("./perf_ind/perf_ind_script.R")
source("./flexdashboard/bpt19.R")
source("./perf_ind/perf_ind_1418_script.R")

#### merge BPT and performance indicators data ####
perf_bpt <- inner_join(perf_df, select(bpt_df, submissionid = sub_id, tariff_loss), by = "submissionid")

#### theme session #####
bckgr <- "#252a32"
compl <- "#617a89"
white <- "#CCCCCC"

theme_session <- 
  theme_fivethirtyeight() + 
  theme(
    panel.grid.major = element_blank(),
    plot.caption = element_text(hjust = .89, margin = margin(t = 1, unit = "cm")),
    plot.subtitle = element_text(hjust = .18),
    plot.title = element_text(hjust = .15)
  ) +
  theme(
    plot.margin = unit(c(1,-1.5,1,1), "cm"),
    plot.background = element_rect(fill = bckgr),
    panel.background = element_rect(fill = bckgr),
    legend.background =  element_rect(fill = "transparent"),
    legend.key = element_blank(),
    text = element_text(color = white))

#### patient flow diagrams ####

# all patients
flow_count <- flow_df %>%
  count(iss_band, previous_hospital, transfer, went_ed, ward_1, ward_2, name = "freq")

flow_all <- ggplot(
  data = flow_count,
  aes(
    y = freq,
    axis1 = previous_hospital,
    axis2 = transfer,
    axis3 = went_ed,
    axis4 = ward_1,
    axis5 = ward_2
  )
) +
  geom_flow(fill = "white") +
  geom_stratum(fill = compl) +
  scale_x_discrete(limits = c("Previous\nhospital", "Transfer", "ED", "Ward 1", "Ward 2")) +
  scale_y_continuous(limits = c(0, sum(flow_count$freq)), breaks = c(0, sum(flow_count$freq))) +
  geom_text(
    stat = "stratum",
    label.strata = TRUE,
    min.height = 3,
    color = white
  ) +
  labs(
    title = "Patient flow",
    caption = "Major Trauma Team @ SRFT",
    subtitle = paste(
      "Patients discharged from",
      min(perf_df$discharge_date),
      "to",
      max(perf_df$discharge_date)
    )
  ) +
  theme_session +
  theme(
    axis.text.x = element_text(face = "bold", margin = margin(t = -.5, unit = "cm")),
    axis.text.y.left = element_text(margin = margin(r = -3, unit = "cm"))
  )

# png(filename = paste0("./perf_ind/flow_all.png"), 
#     res = 350, width = 35, height = 25, units = "cm", type = "cairo-png")
# 
# flow_all
# 
# dev.off()
# ggsave("./perf_ind/flow_all.png", height = 25, width = 25, units = "cm", dpi = "retina")

# previous hosp and transfer
flow_1_2 <- ggplot(data = flow_count, aes(y = freq, axis1 = previous_hospital, axis2 = transfer)) +
  geom_flow(fill = "white", width = 1/3) +
  geom_stratum(fill = compl, width = 1/3) +
  scale_x_discrete(limits = c("Previous\nhospital", "Transfer")) +
  scale_y_continuous(limits = c(0, sum(flow_count$freq)), breaks = c(0, sum(flow_count$freq))) +
  geom_text(stat = "stratum", label.strata = TRUE , 
            min.height = 5, color = white) +
  labs(title = "Patient flow", caption = "Major Trauma Team @ SRFT", 
       subtitle = paste("Patients discharged from", min(perf_df$discharge_date), "to", max(perf_df$discharge_date))) + 
  theme_session +
  theme(axis.text.x = element_text(face = "bold", margin = margin(t = -.5, unit = "cm")),
        axis.text.y.left = element_text(margin = margin(r = -3, unit = "cm")))

# png(filename = paste0("./perf_ind/flow_1_2.png"), 
#     res = 350, width = 35, height = 25, units = "cm", type = "cairo-png")
# 
# flow_1_2
# 
# dev.off()
# ggsave("./perf_ind/flow_1_2.png", height = 25, width = 25, units = "cm", dpi = "retina")

# transfer and ed
flow_2_3 <-
  ggplot(data = flow_count, aes(y = freq, axis1 = transfer, axis2 = went_ed)) +
  geom_flow(fill = "white", width = 1/3) +
  geom_stratum(fill = compl, width = 1/3) +
  scale_x_discrete(limits = c("Transfer", "ED")) +
  scale_y_continuous(limits = c(0, sum(flow_count$freq)), breaks = c(0, sum(flow_count$freq))) +
  geom_text(stat = "stratum",
            label.strata = TRUE ,
            color = white) +
  labs(
    title = "Patient flow",
    caption = "Major Trauma Team @ SRFT",
    subtitle = paste(
      "Patients discharged from",
      min(perf_df$discharge_date),
      "to",
      max(perf_df$discharge_date)
    )
  ) +
  theme_session +
  theme(
    axis.text.x = element_text(face = "bold", margin = margin(t = -.5, unit = "cm")),
    axis.text.y.left = element_text(margin = margin(r = -3, unit = "cm"))
  )

# png(filename = paste0("./perf_ind/flow_2_3.png"), 
#     res = 350, width = 35, height = 25, units = "cm", type = "cairo-png")
# 
# flow_2_3
# 
# dev.off()
# ggsave("./perf_ind/flow_2_3.png", height = 25, width = 25, units = "cm", dpi = "retina")

# ed and ward 1
flow_3_4 <-
  ggplot(data = flow_count, aes(y = freq, axis1 = went_ed, axis2 = ward_1)) +
  geom_flow(fill = "white", width = 1/3) +
  geom_stratum(fill = compl, width = 1/3) +
  scale_x_discrete(limits = c("ED", "Ward 1")) +
  scale_y_continuous(limits = c(0, sum(flow_count$freq)), breaks = c(0, sum(flow_count$freq))) +
  geom_text(stat = "stratum",
            label.strata = TRUE ,
            color = white) +
  labs(
    title = "Patient flow",
    caption = "Major Trauma Team @ SRFT",
    subtitle = paste(
      "Patients discharged from",
      min(perf_df$discharge_date),
      "to",
      max(perf_df$discharge_date)
    )
  ) +
  theme_session +
  theme(
    axis.text.x = element_text(face = "bold", margin = margin(t = -.5, unit = "cm")),
    axis.text.y.left = element_text(margin = margin(r = -3, unit = "cm"))
  )

# png(filename = paste0("./perf_ind/flow_3_4.png"), 
#     res = 350, width = 35, height = 25, units = "cm", type = "cairo-png")
# 
# flow_3_4
# 
# dev.off()
# ggsave("./perf_ind/flow_3_4.png", height = 25, width = 25, units = "cm", dpi = "retina")

# ward 1 and ward 2
flow_4_5 <-
  ggplot(data = flow_count, aes(y = freq, axis1 = ward_1, axis2 = ward_2)) +
  geom_flow(fill = "white", width = 1/3) +
  geom_stratum(fill = compl, width = 1/3) +
  scale_x_discrete(limits = c("Ward 1", "Ward 2")) +
  scale_y_continuous(limits = c(0, sum(flow_count$freq)), breaks = c(0, sum(flow_count$freq))) +
  geom_text(
    stat = "stratum",
    label.strata = TRUE ,
    color = white,
    min.height = 3
  ) +
  labs(
    title = "Patient flow",
    caption = "Major Trauma Team @ SRFT",
    subtitle = paste(
      "Patients discharged from",
      min(perf_df$discharge_date),
      "to",
      max(perf_df$discharge_date)
    )
  ) +
  theme_session +
  theme(
    axis.text.x = element_text(face = "bold", margin = margin(t = -.5, unit = "cm")),
    axis.text.y.left = element_text(margin = margin(r = -3, unit = "cm"))
  )

# png(filename = paste0("./perf_ind/flow_4_5.png"), 
#     res = 350, width = 35, height = 25, units = "cm", type = "cairo-png")
# 
# flow_4_5
# 
# dev.off()
# ggsave("./perf_ind/flow_4_5.png", height = 25, width = 25, units = "cm", dpi = "retina")

# grouped by ISS band
flow_iss <- ggplot(
  data = flow_count,
  aes(
    y = freq,
    axis1 = previous_hospital,
    axis2 = transfer,
    axis3 = went_ed,
    axis4 = ward_1,
    axis5 = ward_2
  )
) +
  geom_flow(aes(fill = iss_band)) +
  geom_stratum(fill = compl) +
  scale_x_discrete(limits = c("Previous\nhospital", "Transfer", "ED", "Ward 1", "Ward 2")) +
  scale_y_continuous(limits = c(0, sum(flow_count$freq)), breaks = c(0, sum(flow_count$freq))) +
  geom_text(
    stat = "stratum",
    label.strata = TRUE,
    min.height = 3,
    color = white
  ) +
  scale_fill_wsj() +
  labs(
    title = "Patient flow",
    caption = "Major Trauma Team @ SRFT",
    subtitle = paste(
      "Patients discharged from",
      min(perf_df$discharge_date),
      "to",
      max(perf_df$discharge_date)
    ),
    fill = "ISS band"
  ) +
  theme_session +
  theme(
    axis.text.x = element_text(face = "bold", margin = margin(t = -.5, unit = "cm")),
    axis.text.y.left = element_text(margin = margin(r = -3, unit = "cm")),
    legend.position = "bottom"
  )

# png(filename = paste0("./perf_ind/flow_iss.png"), 
#     res = 350, width = 35, height = 25, units = "cm", type = "cairo-png")
# 
# flow_iss
# 
# dev.off()
# ggsave("./perf_ind/flow_iss.png", height = 25, width = 25, units = "cm", dpi = "retina")

# arrange flow diagrams in a single page
title_plot <- ggplot() + 
  labs(title = "Patient flow", 
       subtitle = paste("Patients discharged from", min(perf_df$discharge_date), "to", max(perf_df$discharge_date))) + 
  theme_session +
  theme(plot.title = element_text(hjust = .5, margin = margin(t = 1.5, unit = "cm")), 
        plot.subtitle = element_text(hjust = .5, margin = margin(b = 1, unit = "cm")))

top_plot <- flow_all + labs(title = NULL, subtitle = NULL, caption = NULL) + theme(axis.text.y.left = element_blank(), axis.text.x = element_text(margin = margin(t = 2, unit = "cm")))

middle_plot <- plot_grid(flow_1_2 + labs(title = NULL, subtitle = NULL, caption = NULL) + theme_session + theme(axis.text.y.left = element_blank(), plot.margin = margin(0, 0, 10, 0), axis.text.x = element_text(margin = margin(t = -10))), 
                         flow_2_3 + labs(title = NULL, subtitle = NULL, caption = NULL) + theme_session + theme(axis.text.y.left = element_blank(), plot.margin = margin(0, 0, 10, 0), axis.text.x = element_text(margin = margin(t = -10))), 
                         flow_3_4 + labs(title = NULL, subtitle = NULL, caption = NULL) + theme_session + theme(axis.text.y.left = element_blank(), plot.margin = margin(0, 0, 10, 0), axis.text.x = element_text(margin = margin(t = -10))), 
                         flow_4_5 + labs(title = NULL, subtitle = NULL, caption = NULL) + theme_session + theme(axis.text.y.left = element_blank(), plot.margin = margin(0, 0, 10, 0), axis.text.x = element_text(margin = margin(t = -10))), 
                         nrow = 2, align = "v")

bottom_plot <- flow_iss + labs(title = NULL, subtitle = NULL, caption = NULL) + theme_session + theme(axis.text.y.left = element_blank(), axis.text.x = element_blank())

caption_plot <- ggplot() + labs(caption = "Major Trauma Team @ SRFT") + theme_session + theme(plot.caption = element_text(margin = margin(t = 1, b = 1, unit = "cm")))

all_and_iss <- plot_grid(title_plot, top_plot, bottom_plot, caption_plot, nrow = 4, align = "v", rel_heights = c(.25, 2, 2, .25)) + theme(plot.margin = margin(l = -2.5, b = 0, unit = "cm"))
broken_down <- plot_grid(title_plot, middle_plot, caption_plot, nrow = 3, rel_heights = c(.1, 1.5, .2))

# save all patients flow and all patients by ISS band 
# png(filename = paste0("./perf_ind/flow_all_iss.png"), 
#     res = 350, width = 25, height = 35, units = "cm", type = "cairo-png")
# all_and_iss
# dev.off()

# save flow broken down by steps
# png(filename = paste0("./perf_ind/flow_brdown.png"), 
#     res = 350, width = 35, height = 35, units = "cm", type = "cairo-png")
# broken_down
# dev.off()


#### idea for patient flow viz ####
solarized_plus <- c("#fdf6e3", solarized_pal()(8))

wards_curve_plot <- flow_df %>% 
   select(ward_1, ward_2) %>% 
   group_by_all() %>% 
   summarise(n = n()) %>% 
   ungroup() %>%
   ggplot() +
   geom_curve(aes(x = "", y = ward_1, 
                  xend = ward_2, yend = "",
                  size = n, color = ward_1, alpha = .1),
              curvature = -0.15,
              alpha = 0.5) +
  scale_size(range = c(.1, 5)) +
   scale_y_discrete(expand = c(0 ,0)) +
   scale_x_discrete(expand = expand_scale(add = c(0, 1))) +
   scale_color_manual(values = solarized_plus) +
   labs(title = "Volume of internal transfers", 
        subtitle = paste("\nFrom", min(perf_df$discharge_date), "to", max(perf_df$discharge_date), "¦ Width of line represents volume of patients"), 
        y = "First ward", x = "Second ward",
        color = "Count",
        caption = "Source: TARN database | Graphics: Major Trauma Team @ SRFT") +
   theme_session + 
   theme(
     legend.position = "none",
     plot.margin = unit(c(1, .5, .5, 1),"cm"),
     plot.title = element_text(hjust = 0, margin = margin(t = 0)),
     plot.subtitle = element_text(margin = margin(0, 0, 30, 0), hjust = 0, lineheight = 1.5),
     plot.caption = element_text(margin = margin(50, 0, 0, 0), face = "plain", hjust = .9),
     axis.title = element_text(),
     axis.title.x  = element_text(margin = margin(20, 0, 0, 0)),
     axis.title.y  = element_text(margin = margin(0, 20, 0, 0)),
     text = element_text(size = 14)
   )

# png(filename = paste0("./perf_ind/wards_curve_plot.png"), 
#     res = 350, width = 25, height = 35, units = "cm", type = "cairo-png")
# wards_curve_plot
# dev.off()

#### idea for LoS by month ####
los_bars_plot <- perf_df %>% 
  select(discharge_month, los) %>% 
  mutate(los = factor(los, levels = seq(-10, max(perf_df$los)))) %>% 
  group_by(discharge_month, los) %>%
  summarise(n = n()) %>% 
  ungroup() %>% 
  ggplot(aes(x = los, y = n, fill = n)) +
  geom_bar(stat = "identity", width = .9) +
  scale_fill_continuous_tableau(labels = function(x) floor(x)) +
  coord_polar() +
  ylim(c(-5, NA)) +
  facet_wrap(vars(discharge_month), ncol = 2) +
  geom_text(aes(x = .5, y = -5, label = discharge_month), family = "sans", color = white) +
  labs(title = "Length of stay distribution by month",
       caption = "Source: TARN database | Graphics: Major Trauma Team @ SRFT") +
  theme_session +
  theme(text = element_text(size = 14),
        axis.text.y = element_blank(),
        strip.text = element_blank(),
        strip.background = element_blank(),
        legend.title = element_text(vjust = .8),
        legend.key.width = unit(.8, "cm"),
        plot.margin = unit(c(1, .5, .5, 1),"cm"),
        plot.title = element_text(hjust = 0, margin = margin(t = 0, b = 1, unit = "cm")),
        plot.caption = element_text(margin = margin(50, 0, 0, 0), face = "plain", hjust = .9)) 

# png(filename = paste0("./perf_ind/los_bars_plot.png"), 
#     res = 350, width = 21, height = 29, units = "cm", type = "cairo-png")
# los_bars_plot
# dev.off()

#### second idea for LoS by month ####
perf_df$los_fc <- factor(perf_df$los, levels = seq(1, max(perf_df$los)))

los_bars_plot2 <- perf_df %>% 
  select(discharge_month, los_fc) %>% 
  group_by(discharge_month, los_fc) %>%
  summarise(n = n()) %>% 
  ungroup() %>% 
  ggplot() +
  geom_bar(data = . %>% mutate(dummy_fill = "los"), 
           aes(x = los_fc, y = 1, fill = n), stat = "identity", width = 1) +
  geom_bar(data = select(perf_df, discharge_month, los_fc, iss_band) %>%
                  group_by(discharge_month, los_fc) %>% mutate(n_los = n()) %>% 
                  group_by(iss_band, add = TRUE) %>% mutate(prop = -1 * (n() / n_los)) %>% ungroup(), 
           aes(x = los_fc, y = prop, alpha = iss_band), stat ="identity", width = 1, position = "fill", fill = alpha("#cb4b16", .5)) +
  scale_fill_gradient(low = alpha(compl, .5), high = alpha("red", .5),
                      labels = function(x) floor(x)) +
  coord_polar() +
  ylim(c(-2.8, NA)) +
  facet_wrap(vars(discharge_month), ncol = 2) +
  geom_text(aes(x = .5, y = -2.8, label = discharge_month), family = "sans", color = white) +
  labs(title = "Length of stay distribution by month",
       caption = "Source: TARN database | Graphics: Major Trauma Team @ SRFT",
       alpha = "ISS band") +
  theme_session +
  theme(text = element_text(size = 14),
        axis.text.y = element_blank(),
        strip.text = element_blank(),
        strip.background = element_blank(),
        legend.title = element_text(vjust = .8),
        legend.key.width = unit(.8, "cm"),
        plot.margin = unit(c(1, .5, .5, 1),"cm"),
        plot.title = element_text(hjust = 0, margin = margin(t = 0, b = 1, unit = "cm")),
        plot.caption = element_text(margin = margin(50, 0, 0, 0), face = "plain", hjust = .9))
  
# png(filename = paste0("./perf_ind/los_bars_plot2.png"), 
#     res = 350, width = 21, height = 29, units = "cm", type = "cairo-png")
# los_bars_plot2
# dev.off()

#### LoS distribution by iss band ####
select(perf_df, los, iss_band) %>% 
  count(los, iss_band) %>% 
  {
  ggplot(data = ., aes(x = los, y = n, fill = iss_band)) +
  geom_col(data = subset(., iss_band == ">15"), alpha = .5) +
  geom_col(data = subset(., iss_band == "9-15"), alpha = .5) +
  geom_col(data = subset(., iss_band == "1-8"), alpha = .5) +
  scale_y_continuous(labels = function(x) floor(x)) +
  facet_wrap(~ iss_band) +
  theme_session
  }

#### idea to visualize tariff indicators ####

perc_achie <- bpt_df %>% 
  select(month_dis, tariff_actual, tariff_pot) %>% 
  group_by(month_dis) %>% summarise(prop_per = sum(tariff_actual) / sum(tariff_pot), 
                                    achiev = sum(tariff_actual)) %>% 
  mutate(rang_color = case_when(
        between(prop_per, 0, .5) ~ "red",
        between(prop_per, .5, .75) ~ "orange",
        between(prop_per, .75, 1) ~ "green")) %>% 
  ggplot() +
  geom_rect(aes(ymax = prop_per, ymin = 0, xmax = 2, xmin = 1, fill = rang_color), alpha = .5) +
  geom_rect(aes(ymax = 1, ymin = prop_per, xmax = 2, xmin = 1), fill = alpha("#eee8d5", .5)) +
  coord_polar(theta = "y", start = (3 * pi) / 2) +  ## here I pretend I understand what radians are
  ylim(c(0,2)) + xlim(c(0,2)) +
  geom_text(aes(x = 0.5, y = 0.5, label = percent(prop_per), color = rang_color), size = 6, 
            fontface = "bold", alpha = 0.5) +
  geom_text(aes(x = 2, y = 0.5, label = format(month_dis, "%B")), size = 5, color = white, vjust = -0.8) +
  geom_text(aes(x = 0, y = 1.5, label = dollar(achiev, prefix = "£")), color = white) +
  scale_fill_manual(values = c("green" = "#859900", "orange" = "#cb4b16", "red" = "#dc322f")) +
  scale_color_manual(values = c("green" = "#859900", "orange" = "#cb4b16", "red" = "#dc322f")) +
  facet_wrap(~month_dis, ncol = 2) +
  labs(title = "% of achieved perfect income by month",
       caption = "Source: TARN database | Graphic: Major Trauma Team @ SRFT") +
  theme_session +
  theme(axis.text = element_blank(),
        text = element_text(size = 14),
        strip.text = element_blank(),
        strip.background = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(1, .5, .5, 1),"cm"),
        plot.title = element_text(hjust = 0, margin = margin(t = 0, b = 1, unit = "cm")),
        plot.caption = element_text(margin = margin(50, 0, 0, 0), face = "plain", hjust = .9)) 

# png(filename = paste0("./perf_ind/perc_achie.png"), 
#     res = 350, width = 21, height = 29, units = "cm", type = "cairo-png")
# perc_achie
# dev.off()

#### idea of plot for finance in current financial year ####

# gauge and labels with summary of income
gauge_perc <- bpt_df %>% 
  select(tariff_actual, tariff_pot) %>% 
  summarise(prop_per = sum(tariff_actual) / sum(tariff_pot)) %>% 
  mutate(rang_color = case_when(
    between(prop_per, 0, .5) ~ "red",
    between(prop_per, .5, .75) ~ "orange",
    between(prop_per, .75, 1) ~ "green")) %>% 
  ggplot() +
  geom_rect(aes(ymax = prop_per, ymin = 0, xmax = 2, xmin = 1, fill = rang_color), alpha = .5) +
  geom_rect(aes(ymax = 1, ymin = prop_per, xmax = 2, xmin = 1), fill = alpha("#eee8d5", .5)) +
  coord_polar(theta = "y", start = (3 * pi) / 2) +  ## here I pretend I understand what radians are
  ylim(c(0,2)) + xlim(c(0,2)) +
  geom_text(aes(x = 0, y = 0, label = percent(prop_per), color = rang_color), size = 6, 
            fontface = "bold", alpha = 0.5) +
  geom_text(aes(x = 2, y = 0.5, label = "% of perfect income"), size = 5, color = white, vjust = -0.8) +
  scale_color_manual(values = c("green" = "#859900", "orange" = "#cb4b16", "red" = "#dc322f")) +
  theme_session +
  theme(axis.text = element_blank(),
        legend.position = "none",
        plot.margin = margin(r = 1, l = 1, b = 1, t = 1),
        plot.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"))

achieved_label <- ggplot() + 
  geom_text(aes(x = .5, y = .8, 
                label = paste0(dollar(sum(bpt_df$tariff_actual) , prefix = "£"), "\n of achieved income")),
            fontface = "bold", color = white, size = 6) +
  theme_session +
  theme(axis.text = element_blank(),
        plot.margin = margin(1, 1, 1, 1),
        plot.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"))

potential_label <- ggplot() + 
  geom_text(aes(x = .5, y = .8, 
                label = paste0(dollar(sum(bpt_df$tariff_pot) , prefix = "£"), "\n of potential income")),
            fontface = "bold", color = white, size = 6) +
  theme_session +
  theme(axis.text = element_blank(),
        plot.margin = margin(1, 1, 1, 1),
        plot.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"))


# summary_inc_row <- plot_grid(achieved_label, gauge_perc, potential_label, nrow = 1, rel_widths = c(.5, 1, .5))


# ribbon style plot

ribbon_df <- bpt_df %>%
  select(outcome_date, month_dis, tariff_actual, tariff_pot) %>%
  mutate(week_dis = cut.Date(outcome_date, "weeks")) %>%
  select(-outcome_date) %>%
  group_by(month_dis, week_dis) %>%
  summarise(tariff_actual = sum(tariff_actual, na.rm=TRUE),
            tariff_pot = sum(tariff_pot, na.rm=TRUE),
            prop = tariff_actual / tariff_pot) %>%
  ungroup() %>% mutate(month_dis = strftime(month_dis, "%B"))

ribbon1 <- tibble(x = seq(0, max(ribbon_df$tariff_pot)),
                  ymax = x,
                  ymin = seq(0, max(ribbon_df$tariff_pot)) * .75)

ribbon2 <- tibble(x = seq(0, max(ribbon_df$tariff_pot)),
                  ymax = x,
                  ymin = seq(0, max(ribbon_df$tariff_pot)) * .5)

ribbon3 <- tibble(x = seq(0, max(ribbon_df$tariff_pot)),
                  ymax = x,
                  ymin = seq(0, max(ribbon_df$tariff_pot)) * .25)

ribbon4 <- tibble(x = seq(0, max(ribbon_df$tariff_pot)),
                  ymax = x,
                  ymin = 0)

achie_pot_ribbon <- ribbon_df %>%
  ggplot(aes(y = tariff_actual, x = tariff_pot)) +
  geom_ribbon(inherit.aes = FALSE, data = ribbon4, aes(x = x, ymin = ymin, ymax = ymax), alpha = .3, fill = "#efb7b3") +
  geom_ribbon(inherit.aes = FALSE, data = ribbon3, aes(x = x, ymin = ymin, ymax = ymax), alpha = .3, fill = "#efcf97") +
  geom_ribbon(inherit.aes = FALSE, data = ribbon2, aes(x = x, ymin = ymin, ymax = ymax), alpha = .3, fill = "#d8d8ab") +
  geom_ribbon(inherit.aes = FALSE, data = ribbon1, aes(x = x, ymin = ymin, ymax = ymax), alpha = .3, fill = "#c0d8c1") +
  geom_point(aes(color = month_dis), size = 3, alpha = .4) +
  annotate("text", x = 47000, y = 40000, fontface = "bold", hjust = 0, label = "75% to 100%", angle = 28, alpha = .6) +
  annotate("text", x = 48000, y = 30000, fontface = "bold", hjust = 0, label = "50% to 75%", angle = 28, alpha = .6) +
  annotate("text", x = 48000, y = 17000, fontface = "bold", hjust = 0, label = "25% to 50%", angle = 28, alpha = .6) +
  annotate("text", x = 48000, y = 5000, fontface = "bold", hjust = 0, label = "0% to 25%", angle = 28, alpha = .6) +
  scale_y_continuous(position = "right") +
  scale_color_solarized(guide = guide_legend(nrow = 2, ncol = 6, byrow = TRUE, 
                                             title = "Week of the month",
                                             title.position = "left",
                                             title.vjust = .8)) +
  coord_cartesian(ylim=c(0, max(ribbon_df$tariff_pot) * .85)) +
  labs(x       = "Potential income", 
       y       = "Achieved\nincome", color = NULL, 
       caption = "Source: TARN database | Graphic: Major Trauma Team @ SRFT",
       title = "Summary of tariff from TARN audit",
       subtitle = str_wrap(paste("The dataset runs from", min(bpt_df$outcome_date), "to", max(bpt_df$outcome_date),".", 
                                 nrow(bpt_df), "were submited in this period.", "Of these,", sum(as.numeric(as.character(bpt_df$iss)) > 15), "were ISS > 15,",
                                 sum(as.numeric(as.character(bpt_df$iss)) >= 9 & as.numeric(as.character(bpt_df$iss)) <= 15), "were ISS 9-15, and", 
                                 sum(as.numeric(as.character(bpt_df$iss)) < 9), "were ISS 1-8."))
       ) +
  theme_session +
  theme(plot.margin = margin(1, 1, 1, 1, unit = "cm"),
        axis.title = element_text(),
        plot.title = element_text(hjust = 0, margin = margin(t = 0)),
        plot.caption = element_text(face = "plain", hjust = 1, margin = margin(t = .5, unit = "cm")),
        plot.subtitle = element_text(hjust = 0, margin = margin(t = .5, unit = "cm")))

# png(filename = "./perf_ind/ribbon_income.png", 
#     res = 350, width = 25, height = 20, units = "cm", type = "cairo-png")
# achie_pot_ribbon
# dev.off()

# bar plot of iss bands filled with type of loss (whole or partial loss)
iss_type_loss_plot <- select(bpt_df, iss, tariff_loss, bpt_actual, tariff_diff) %>% 
  mutate(iss = as.integer(as.character(iss)),
         iss_band = case_when(
           iss >= 9 & iss <= 15 ~ "9-15",
           iss > 15             ~ ">15"
         ),
         type_loss = case_when(
           bpt_actual == "no bpt" & iss_band == ">15" ~ "Whole",
           bpt_actual == "level 1" & iss_band == ">15" ~ "Partial",
           bpt_actual == "no bpt" & iss_band == "9-15" ~ "Whole",
           TRUE ~ "No"
         )) %>% 
  group_by(iss_band, type_loss) %>% summarise(n = n()) %>% ungroup() %>% 
  ggplot(aes(x = iss_band, y = n, fill = type_loss))  +
  geom_col(width = .1, alpha = .5) +
  geom_text(aes(label = n), hjust = 1, position = "stack", color = white) +
  geom_text(aes(x = iss_band, y = 0, label = iss_band), vjust = -2, hjust = 0, color = white) +
  coord_flip() +
  scale_fill_solarized(guide = guide_legend(title = "Type of loss", 
                                      title.position = "top",
                                      title.vjust = 1.5,
                                      label.position = "bottom")) +
  theme_session +
  theme(axis.text = element_blank(),
        plot.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(.1, unit = "cm"),
        legend.position = c(.6, .5)
        )

# combine ribbon with bar plot of type of loss by iss band
# png(filename = "./perf_ind/ribbon_iss.png", 
#     res = 350, width = 25, height = 20, units = "cm", type = "cairo-png")
# ggdraw(achie_pot_ribbon) +
#   draw_plot(iss_type_loss_plot, 0, .45, .25, .4)
# dev.off()

#### idea to visualize day of week and time of arrival ####
dow_hour_heatmap <- select(perf_df, arrival_date, arrival_time) %>% 
  mutate(dt_arrival = lubridate::ymd_hms(paste(arrival_date, arrival_time)),
         hour_arrival = strftime(dt_arrival, "%H"),
         dow_arrival = strftime(dt_arrival, "%A")) %>% 
  select(-dt_arrival) %>% 
  count(dow_arrival = factor(dow_arrival, levels = c("Monday", 
                             "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), 
        hour_arrival) %>%
  ggplot(aes(x = hour_arrival, y = dow_arrival, alpha = n)) +
  geom_tile(fill = "#d33682") +
  scale_alpha(range = c(.1, .5),
              guide = guide_legend(
                label.position = "bottom",
                title.vjust = 1,
                title.hjust = 0,
                nrow = 1)) +
  scale_x_discrete(position = "top") +
  coord_fixed() +
  labs(title = "Day of week and time of arrival",
       subtitle = glue("From {min(perf_df$arrival_date)} to {max(perf_df$arrival_date)}"),
       alpha = "Number of arrivals",
       caption = "Source: TARN database | Graphic: Major Trauma Team @ SRFT") +
  # theme_session +
  theme(text = element_text(size = 8),
        plot.margin = margin(1, .5, 1, .5, unit = "cm"),
        plot.caption = element_text(hjust = 1, margin = margin(t = 1, b = 0, unit = "cm"), size = 6),
        plot.title = element_text(hjust = 0),
        plot.subtitle = element_text(hjust = 0, margin = margin(t = .2, b = 1, unit = "cm")),
        legend.spacing.x = unit(0, units = "cm"),
        legend.key.height = unit(2, units = "mm")
        )
  
# png(filename = "./perf_ind/dow_hour_heatmap.png", 
#     res = 350, width = 21, height = 15, units = "cm", type = "cairo-png")
# dow_hour_heatmap
# dev.off()

#### visualize whole calendar year of arrivals ####

calendar_heatmap_adm <- select(perf_bpt, arrival_date) %>% 
  count(arrival_date) %>% 
  ggplot_calendar_heatmap("arrival_date", "n", dayBorderColour = compl, monthBorderColour = compl) +
  scale_fill_gradient(low = alpha("#d33682", .1), high = alpha("#d33682", .5), 
                      labels = function(x) floor(x)) +
  labs(title = str_wrap("Calendar heatmap of Major Trauma admissions from TARN dataset"),
       subtitle = glue("From {min(perf_df$arrival_date)} to {max(perf_df$arrival_date)}"),
       fill = "Number of admissions",
       caption = "Source: TARN database | Graphic: Major Trauma Team @ SRFT") +
  theme_session +
  theme(text = element_text(size = 8),
        strip.text = element_blank(),
        plot.margin = margin(1, .5, .5, 1, unit = "cm"),
        plot.title = element_text(hjust = 0),
        plot.subtitle = element_text(hjust = 0, margin = margin(b = 1, t = .5, unit = "cm")))

ggsave("./perf_ind/calendar_heatmap_adm.png", calendar_heatmap_adm, "png", height = 10, width = 14, dpi = "retina", type = "cairo-png")

#### idea to visualize day of week, time of arrival and unmet criteria ####

loss_dow_hour_heatmap <- perf_bpt %>% 
  mutate(dt_arrival = lubridate::ymd_hms(paste(arrival_date, arrival_time)),
         hour_arrival = strftime(dt_arrival, "%H"),
         dow_arrival = strftime(dt_arrival, "%A")) %>% 
  group_by(dow_arrival = factor(dow_arrival, levels = c("Monday", 
                             "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), 
        hour_arrival) %>%
  summarise(prop_los = sum(tariff_loss == "yes") / n()) %>% ungroup() %>% 
  ggplot(aes(x = hour_arrival, y = dow_arrival, alpha = prop_los)) +
  geom_tile(fill = "#d33682", color = alpha(white, .2)) +
  scale_alpha(range = c(.1, .5),
              guide = guide_legend(
                label.position = "bottom",
                title.vjust = 1,
                title.hjust = 0,
                nrow = 1)) +
  scale_x_discrete(position = "top") +
  coord_fixed() +
  labs(title = "Proportion of submissions with unmet criteria by day and time of arrival",
       subtitle = glue("From {min(perf_df$arrival_date)} to {max(perf_df$arrival_date)}"),
       alpha = "Prop of submissions",
       caption = "Source: TARN database | Graphic: Major Trauma Team @ SRFT") +
  theme_session +
  theme(text = element_text(size = 8),
        plot.margin = margin(1, .5, 1, .5, unit = "cm"),
        plot.caption = element_text(hjust = 1, margin = margin(t = 1, b = 0, unit = "cm"), size = 6),
        plot.title = element_text(hjust = 0),
        plot.subtitle = element_text(hjust = 0, margin = margin(t = .2, b = 1, unit = "cm")),
        legend.spacing.x = unit(0, units = "cm"),
        legend.key.height = unit(2, units = "mm")
        )
  
# png(filename = "./perf_ind/loss_dow_hour_heatmap.png", 
#     res = 350, width = 21, height = 15, units = "cm", type = "cairo-png")
# loss_dow_hour_heatmap
# dev.off()

#### idea to visualize composition of patients by iss band by month of discharge ####

iss_waffle_df <- perf_df %>% 
  select(arrival_date, iss_band) %>%
  mutate(arrival_date = floor_date(arrival_date, "month"),
         n_pts = 1) %>%
  group_by(arrival_date) %>%
  mutate(n_month = sum(n_pts)) %>%
  group_by(iss_band, add = TRUE) %>%
  mutate(pts_month = sum(n_pts)) %>% 
  summarise(prop_pts = first(floor(pts_month / n_month * 100))) %>%
  group_by(iss_band, add = TRUE) %>%
  group_modify(.f = ~ slice(.x, rep(1, .x$prop_pts))) %>%
  select(-prop_pts) %>%
  arrange(arrival_date, iss_band) %>%
  ungroup() %>%
  group_by(arrival_date) %>%
  group_modify(.f = ~ {
    bind_cols(iss_band = .x$iss_band, head(expand.grid(x = 1:10, y = 1:10), nrow(.x)))
  }) %>%
  ungroup() %>%
  mutate(arrival_date = factor(strftime(arrival_date, "%b"),
                       levels = strftime(seq.Date(
                         min(.$arrival_date), max(.$arrival_date), "months"
                       ), "%b")))

iss_month_waffle <- 
  ggplot(iss_waffle_df, aes(x = x, y = y, fill = iss_band)) +
  geom_tile(alpha = .5) +
  facet_wrap( ~ arrival_date) +
  coord_fixed(ratio = 1.5 / 1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_manual(
    values = solarized_pal(accent = "red")(3),
    guide = guide_legend(
      title = NULL,
      label.position = "bottom",
      label.hjust = .5
    )
  ) +
  labs(title = "Composition of patients's ISS banding by month of discharge\n",
       caption = "Source: TARN database | Graphic: Major Trauma Team @ SRFT") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "gray40"), 
    text = element_text(color = "white"),
    plot.title = element_text(hjust = .5, margin = margin(b = 1.5, t = 1.5, unit = "cm")),
    plot.caption = element_text(margin = margin(t = 1.5, b = 1, unit = "cm")),
    legend.direction = "horizontal",
    legend.spacing.x = unit(3, "mm"),
    legend.key.height = unit(1, "mm"),
    legend.key.width = unit(10, "mm"),
    legend.position = "top",
    legend.margin = margin(b = 1, unit = "cm"),
    strip.text = element_text(hjust = 0.1, vjust = 1),
    panel.spacing.x = unit(10,"mm"),
    plot.margin = margin(0, 1.5, 0, 1.5, unit = "cm")
  )

# png(filename = "./perf_ind/iss_month_waffle.png", 
#     res = 350, width = 21, height = 29, units = "cm", type = "cairo-png")
# iss_month_waffle
# dev.off()

#### idea to visualize mean and median LoS by ISS #### 

mean_all <- mean(perf_df$los, na.rm = TRUE)
median_all <- median(perf_df$los, na.rm = TRUE)

median_los_iss <- 
perf_df %>% 
  select(iss_band, los) %>% 
  group_by(iss_band) %>% 
  mutate(med_los = median(los, na.rm = TRUE),
         mean_los = mean(los, na.rm = TRUE)) %>% 
  ggplot(aes(y = iss_band, x = los)) +
  geom_jitter(aes(fill = iss_band), color = "black", width = .1, height = 0.2, alpha = 0.3, shape = 21, size = 2.5) +
  # geom_point(aes(x = iss_band, y = mean_los),
  #            size = 5) +
  geom_segment(
    aes(
      y = iss_band,
      yend = iss_band,
      x = median_all,
      xend = med_los
    ),
    color = "grey85",
    size = 0.7
  ) +
  geom_point(aes(x = med_los), color = "grey85", size = 6) +
  geom_point(aes(x = med_los, color = iss_band), size = 5) +
  # geom_vline(aes(yintercept = mean_all)) +
  geom_vline(aes(xintercept = median_all), color = "grey85") +
  scale_x_continuous(limits = c(1, 60), breaks = seq(1, 60, 6)) +
  guides(fill = FALSE, color = FALSE) +
  labs(title = "LoS distribution by ISS band") +
  theme_bw() +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(color = "grey80"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 9),
    axis.ticks = element_line(color = "grey80"),
    panel.border = element_rect(colour = "grey80", fill = "transparent"),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "gray40"),
    panel.background = element_rect(fill = "gray40"),
    text = element_text(color = "white"),
    plot.title = element_text(hjust = .5, color = "grey80"),
    plot.caption = element_text(margin = margin(
      t = 1.5, b = 1, unit = "cm"
    ))
  )

#### visualize distribution of age for male and female ####
density_age_plot <- 
  select(perf_df, age, sex) %>% 
  ggplot(aes(fill = sex)) +
  geom_density(data = . %>% filter(sex == "Female"), aes(x = age), color = NA, alpha = .5) +
  geom_density(data = . %>% filter(sex == "Male"), aes(x = age, y = -..density..), color = NA, alpha = .5) +
  annotate("text", x = 40, y = -0.005, label = "Male", size = 16, fontface = "bold", 
           color = hcl.colors(2, palette = "Dynamic")[2]) +
  annotate("text", x = 65, y = 0.005, label = "Female", size = 16, fontface = "bold",
           color = hcl.colors(2, palette = "Dynamic")[1]) +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(guide = "none", values = hcl.colors(2, palette = "Dynamic")) + 
  theme_minimal() +
  theme(panel.background = element_rect(color = "transparent", fill = "transparent"),
        plot.background = element_rect(color = "transparent", fill = "transparent"))
  

#### Discharged and admitted patients per day ####

# get average number of admissions and discharges per day of the year from 2014 to 2018 financial years
inout_avg <- 
  perf1418_df %>% 
  select(arri = arrival_date, dis = discharge_date) %>% 
  mutate(
    fin_year = case_when(
      between(dis, as.Date("2018-04-01"), as.Date("2019-03-31")) ~ "2018",
      between(dis, as.Date("2017-04-01"), as.Date("2018-03-31")) ~ "2017",
      between(dis, as.Date("2016-04-01"), as.Date("2017-03-31")) ~ "2016",
      between(dis, as.Date("2015-04-01"), as.Date("2016-03-31")) ~ "2015",
      between(dis, as.Date("2014-04-01"), as.Date("2015-03-31")) ~ "2014"
    )
  ) %>% 
  pivot_longer(dis:arri, names_to = "in_out", values_to = "date") %>% 
  mutate(
    date = format(date, "%m-%d"),
    value = if_else(in_out == "dis", -1, 1)
  ) %>% 
  group_by(fin_year, date, in_out) %>% 
  summarise(sum_year = sum(value)) %>% 
  ungroup() %>% 
  group_by(date, in_out) %>% 
  summarise(mean_years = mean(sum_year)) %>% 
  ungroup() %>% 
  mutate(date = as.Date(paste0("2019-", date)))

# date limits
limits <- as.Date(c("2019-04-01", "2019-08-30"))

inout_df <-
select(perf_df, arri = arrival_date, dis = discharge_date) %>% 
  filter(arri >= as.Date("2019-04-01") & dis >= as.Date("2019-04-01")) %>%
  pivot_longer(arri:dis, names_to = "in_out", values_to = "date") %>% 
  group_by(date, in_out) %>% 
  mutate(unit = if_else(in_out == "dis", -1, 1),
         csum = cumsum(unit)) %>% 
  arrange(date)

inout_plot <- 
ggplot(inout_df, aes(x = date, y = csum)) +
  geom_line(data = inout_avg %>% filter(between(date, limits[1], limits[2])),
            aes(x = date, y = mean_years, group = in_out),
            color = alpha("firebrick", .1), size = 1,
            linejoin = "round",
            lineend = "round"
  ) +
  geom_point(aes(fill = in_out), shape = 21, color = "grey85", size = 2) +
  facet_col(~ month(date, label = TRUE), scales = "free", space = "free", strip.position = "right") +
  scale_x_date(date_breaks = "1 day", date_labels = "%d", expand = c(0.01, 0.01)) +
  scale_y_continuous(labels = function(x) floor(abs(x))) +
  geom_richtext(angle = 90, nudge_x = -.3, fill = NA, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt"),
                data = tibble(date = seq.Date(limits[1], limits[2], "day")), aes(x = date, y = 0, label = wday(date, label = TRUE), 
                color = if_else(wday(date) %in% c(6, 7, 1), TRUE, FALSE))) +  ## create unique sequence of days to avoid multiple labels on same day
  scale_fill_discrete(palette = function(x) hcl.colors(x, "Dark 3"), guide = "none") +
  scale_color_discrete(palette = function(x) hcl.colors(x, "Dynamic"), guide = "none") +
  labs(title = glue("TARN's data showing <b style='color: {hcl.colors(2, 'Dark 3')[1]}'>arrivals</b> and 
                    <b style='color: {hcl.colors(2, 'Dark 3')[2]}'>discharges</b> from
                    {limits[1]} to {limits[2]}"),
       subtitle = glue("<b style='color: {hcl.colors(1, 'Dynamic')[1]}'>Weekdays</b> and 
                       <b style='color: {hcl.colors(2, 'Dynamic')[2]}'>weekends</b> are colored differently | 
                       <b style='color: {alpha('firebrick', .4)}'>Line</b> shows avg. adm./disch. for day of year across 2014-18 financial years"),
       caption = "Graphic: Major Trauma Team @ SRFT | Source: TARN") +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(color = "grey40"),
        plot.background = element_rect(fill = "grey85", color = "grey85"),
        panel.border = element_rect(color = "transparent", fill = "transparent"),
        panel.spacing.y = unit(.8, "cm"),
        plot.title = element_markdown(color ="grey40", face = "bold", size = 20, margin = margin(t = 1, b = .5, unit = "cm")),
        plot.subtitle = element_markdown(color ="grey40", size = 12, margin = margin(b = 1, unit = "cm")),
        plot.caption = element_text(color ="grey40", margin = margin(t = 1.5, b = .5, unit = "cm")),
        plot.margin = margin(0, .5, 0, .5, unit = "cm"),
        strip.text = element_text(color ="grey40", face = "bold")
  )
  
  
ggsave(plot = inout_plot, filename = "./perf_ind/inout.png", width = 14, height = 10, dpi = "retina")

#### patient flow for first and second ward ####
theme_secondflow <- 
  theme_minimal() +
  theme(plot.background = element_rect(fill = "grey20", color = "grey20"),
        panel.background = element_rect(fill = "grey20", color = "grey20"),
        panel.grid = element_blank(),
        text = element_text(color = "grey85"),
        axis.text = element_text(color = "grey85"),
        axis.title = element_text(size = 12),
        axis.title.x = element_text(margin = margin(b = .5, unit = "cm")),
        legend.position = "none")

secondflow_0 <- 
  flow_df %>% 
  select(previous_hospital, transfer) %>% 
  count(transfer, previous_hospital) %>% 
  filter(previous_hospital != "No transfer") %>% 
  replace_na(list(transfer = "Unknown", previous_hospital = "Unknown")) %>% 
  mutate(previous_hospital = fct_reorder(previous_hospital, n)) %>% 
  top_n(15, previous_hospital) %>% 
  ggplot(aes(x = transfer, y = previous_hospital, fill = n)) + 
  geom_tile(alpha = .7, color = "grey20", size = 1.5) +
  scale_x_discrete(position = "top") +
  scale_fill_gradientn(colors = hcl.colors(5, "Emrld")) +
  geom_text(aes(label = n), color = alpha("grey85", .7), fontface = "bold") +
  labs(x = "Type of transfer", y = "Previous hospital") +
  coord_equal() +
  theme_secondflow 

secondflow_1 <- 
flow_df %>% 
  select(transfer, went_ed) %>% 
  count(transfer, went_ed) %>% 
  mutate(transfer = fct_reorder(transfer, n)) %>% 
  ggplot(aes(x = went_ed, y = transfer, fill = n)) + 
  geom_tile(alpha = .7, color = "grey20", size = 1.5) +
  scale_x_discrete(position = "top") +
  scale_fill_gradientn(colors = hcl.colors(5, "Emrld")) +
  geom_text(aes(label = n), color = alpha("grey85", .7), fontface = "bold") +
  labs(x = "Went to ED?", y = "Transfer") +
  coord_equal() +
  theme_secondflow

secondflow_2 <- 
  flow_df %>% 
  select(went_ed, ward_1) %>% 
  count(went_ed, ward_1) %>% 
  replace_na(list(ward_1 = "Unknown")) %>% 
  mutate(ward_1 = fct_reorder(ward_1, n)) %>% 
  ggplot(aes(x = went_ed, y = ward_1, fill = n)) + 
  geom_tile(alpha = .7, color = "grey20", size = 1.5) +
  scale_x_discrete(position = "top") +
  scale_fill_gradientn(colors = hcl.colors(5, "Emrld")) +
  geom_text(aes(label = n), color = alpha("grey85", .7), fontface = "bold") +
  labs(x = "Went to ED?", y = "1st ward") +
  coord_equal() +
  theme_secondflow

secondflow_3 <- 
  flow_df %>% 
  select(ward_1, ward_2) %>% 
  count(ward_1, ward_2) %>% 
  replace_na(list(ward_1 = "Unknown", ward_2 = "Unknown")) %>% 
  mutate(ward_1 = fct_reorder(ward_1, n)) %>% 
  ggplot(aes(x = ward_1, y = ward_2, fill = n)) + 
  geom_tile(alpha = .7, color = "grey20", size = 1.5) +
  scale_x_discrete(position = "top") +
  scale_fill_gradientn(colors = hcl.colors(5, "Emrld")) +
  geom_text(aes(label = n), color = alpha("grey85", .7), fontface = "bold") +
  labs(x = "1st ward", y = "2nd ward") +
  coord_equal() +
  theme_secondflow +
  theme(axis.text.x = element_text(angle = 315, hjust = 1))

afa <- cowplot::plot_grid(secondflow_0, secondflow_1, secondflow_2, secondflow_3, nrow = 1, 
                          rel_widths = c(.5, .5, .5, 1.2))

secondflow_assemble <-                          
afa + labs(title = "TARN's submissions flow for 2019 financial year",
           subtitle = glue("Data extracted from *Performance Review Indicators* spreadsheet ¦ Last discharge was on {max(perf_df$discharge_date)}"),
           caption = "Graphic: Major Trauma Team @ SRFT | Data: TARN database") + 
  theme_secondflow +
  theme(plot.title = element_text(face = "bold", size = 18, color = "white", 
                                  margin = margin(t = .5, unit = "cm"), hjust = .5),
        plot.subtitle = element_markdown(margin = margin(t = .25, b = .2, unit = "cm"), hjust = .5),
        plot.caption = element_text(margin(t = 0, unit = "cm")),
        plot.margin = margin(.5, .5, .1, .5, unit = "cm"))

ggsave(filename = "./perf_ind/secondflow.png", plot = secondflow_assemble, width = 10, height = 8, dpi = "retina")

#### survival curves ####
perf1418_df[["status"]] <- 1  ## event (discahrge) happened in all
perf_df[["status"]] <- 1

perf_merged <-
  bind_rows(
    select(perf1418_df, los, status, sex, iss_band, ps, fin_year, gos),
    select(perf_df, los, status, sex, iss_band, ps, fin_year, gos)
  ) %>% 
  filter(gos != "(1) Death")

perf_survfit <- survfit(Surv(los, status) ~ fin_year, data = perf_merged) %>% 
  surv_summary(data = perf_merged) %>% 
  as_tibble() %>% 
  mutate(surv = 1 - surv,
         alpha = if_else(fin_year == "2019", 1, .3))

curves_cols <- hcl.colors(n_distinct(perf_merged$fin_year), "Dark 3")

surv_fin_1418 <- 
  perf_survfit %>% 
  ggplot(aes(x = time, y = surv, color = strata, alpha = alpha)) +
  geom_step() +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 150), breaks = seq(0, 150, 10)) + 
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 1, .25), limits = c(0, 1)) +
  scale_color_manual(values = curves_cols) +
  scale_alpha_identity() +
  labs(x = "Time to discharge in days",
       y = "Cummulative event probability",
       title = "TARN submissions' cumulative incidence of discharge",
       subtitle = glue("<b style='color: {curves_cols[1]}'>2014</b>, 
                       <b style='color: {curves_cols[2]}'>2015</b>, 
                       <b style='color: {curves_cols[3]}'>2016</b>, 
                       <b style='color: {curves_cols[4]}'>2017</b>, 
                       <b style='color: {curves_cols[5]}'>2018</b> and
                       <b style='color: {curves_cols[6]}'>2019</b>
                       financial years"),
       caption = "Graphic: Major Trauma Team @ SRFT | Source: TARN") +
  theme_minimal() +
  theme(text = element_text(color = "grey40"),
        axis.text = element_text(color = "grey40"),
        axis.text.y = element_text(margin = margin(r = .5, unit = "cm")),
        axis.text.x = element_text(margin = margin(t = .5, unit = "cm")),
        plot.background = element_rect(fill = "grey85", color = "grey85"),
        panel.background = element_rect(fill = "grey85", color = "grey85"),
        plot.title = element_text(face = "bold", size = 20,
                                  color = "grey40", margin = margin(t = 1, unit = "cm")),
        plot.subtitle = element_markdown(margin = margin(b = 1.5, t = .5, unit = "cm")),
        plot.caption = element_text(margin = margin(t = 1, b = .5, unit = "cm")),
        legend.position = "none",
        plot.margin = margin(0, 1, 0, 1, unit = "cm")
  )

ggsave(plot = surv_fin_1418, filename = "./perf_ind/surv_fin_1418.png", width = 11, height = 9, dpi = 320)

# Kaplan-Meier by year faceted by ISS band
perf_survfit_iss <- 
  survfit(Surv(los, status) ~ fin_year + iss_band, data = perf_merged) %>% 
  surv_summary(data = perf_merged) %>% 
  as_tibble() %>% 
  mutate(surv = 1 - surv,
         alpha = if_else(fin_year == "2019", 1, .2))

surv_fin_1418_iss <- 
  perf_survfit_iss %>% 
  ggplot(aes(x = time, y = surv, color = fin_year, alpha = alpha)) +
  geom_step() +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 150), breaks = seq(0, 150, 10)) + 
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 1, .25), limits = c(0, 1)) +
  scale_color_manual(values = curves_cols) +
  scale_alpha_identity() +
  facet_wrap(~ fct_relevel(iss_band, "1-8", after = 2)) +
  labs(x = "Days to discharge", y = "Cumulative event probability",
       title = "TARN submissions' cumulative incidence of discharge",
       subtitle = glue("<b style='color: {curves_cols[1]}'>2014</b>, 
                       <b style='color: {curves_cols[2]}'>2015</b>, 
                       <b style='color: {curves_cols[3]}'>2016</b>, 
                       <b style='color: {curves_cols[4]}'>2017</b>, 
                       <b style='color: {curves_cols[5]}'>2018</b> and
                       <b style='color: {curves_cols[6]}'>2019</b>
                       financial years")
  ) +
  theme_minimal() +
  theme(text = element_text(color = "grey40"),
        axis.text = element_text(color = "grey40"),
        axis.text.y = element_text(margin = margin(r = .5, unit = "cm")),
        axis.text.x = element_text(margin = margin(t = .5, unit = "cm")),
        plot.background = element_rect(fill = "grey85", color = "grey85"),
        panel.background = element_rect(fill = "grey85", color = "grey85"),
        plot.title = element_text(face = "bold", size = 20,
                                  color = "grey40", margin = margin(t = 1, unit = "cm")),
        plot.subtitle = element_markdown(margin = margin(b = 1.5, t = .5, unit = "cm")),
        strip.text = element_text(color = "grey40", size = 12, face = "bold"),
        legend.position = "none",
        plot.margin = margin(0, 0, 0, 0)
  )

# iss band composition of submissions across years
iss_year_comp <- 
perf_merged %>% 
  count(fin_year, iss_band) %>% 
  ggplot(aes(x = fin_year, y = n, fill = iss_band)) +
  geom_col(position = position_fill(reverse = TRUE), alpha = .5, width = .8) +
  scale_y_continuous(labels = glue("{seq(0, 100, 25)}%")) +
  scale_fill_manual(values = hcl.colors(3, "BuPu")) +
  geom_text(aes(label = n), position = position_fill(reverse = TRUE), vjust = 0, color = "grey40", fontface = "bold") + 
  labs(x = "ISS band across years", y = NULL,
       fill = "ISS band",
       caption = "Graphic: Major Trauma Team @ SRFT | Source: TARN") +
  theme_minimal() +
  theme(text = element_text(color = "grey40"),
        panel.grid.major.x = element_blank(),
        panel.grid = element_line(color = alpha("firebrick", .1)),
        axis.text = element_text(color = "grey40"),
        axis.title.x = element_text(face = "bold", size = 16),
        plot.background = element_rect(fill = "grey85", color = "grey85"),
        panel.background = element_rect(fill = "grey85", color = "grey85"),
        legend.background = element_rect(fill = "transparent", color = "transparent"),
        legend.key = element_rect(fill = "transparent", color = "transparent"),
        plot.margin = margin(1, 0, 0, 0, "cm"),
        axis.ticks.y = element_blank(),
        plot.caption = element_text(margin = margin(t = 1, b = .5, unit = "cm"))
  )

# assemble
plot_grid(surv_fin_1418_iss,
          iss_year_comp,
          ncol = 1, rel_heights = c(1, .75))

ggsave(filename = "./perf_ind/surv_assbeml.png", width = 10, height = 8, dpi = "retina")

