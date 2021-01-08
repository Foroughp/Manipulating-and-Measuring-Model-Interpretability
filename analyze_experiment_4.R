library(tidyverse)
library(scales)
library(broom)
library(xtable)
library(lsr)
theme_set(theme_bw())

source('lib.R')

if (!dir.exists('figures/exp4'))
   dir.create('figures/exp4', recursive = T)

apt_labels <- data.frame(q_id = c(10, 11, 12),
                         apt_label = c('Apartment 6: 1 bed, 3 bath, 726 sq ft',
                                       'Apartment 7: 1 bed, 1 bath, 788 sq ft',
                                       'Apartment 8: 1 bed, 3 bath, 350 sq ft'))

my_pallete <- c('#92c5de', '#f4a582', '#92c5de', '#f4a582')

#######################################################
# Training phase
#######################################################
# read data from the training phase
training_responses <- read_csv("data/exp4/data_training.csv")

# recode conditions
training_responses <- training_responses %>%
  mutate(condition = recode_factor(as.factor(condition),
                                   "C0" = "BB-FOCUS",
                                   "C1" = "BB-NO-FOCUS",
                                   "C2" = "CLEAR-FOCUS",
                                   "C3" = "CLEAR-NO-FOCUS")) %>%
  mutate(condition = factor(condition, levels = c("CLEAR-NO-FOCUS", "BB-NO-FOCUS", "CLEAR-FOCUS", "BB-FOCUS")))

training_responses$q_id <- training_responses$q_id + 1

# plot histogram
breaks <- seq(0,3,1)
training_dist_plot <- ggplot(training_responses, aes(x = final_pred, fill=condition)) +
  geom_histogram(binwidth = 0.1, alpha = 0.5) +
  geom_vline(alpha=.2, size=1, aes(xintercept = model_pred, linetype="Model's prediction"))+
  geom_vline(alpha=.2, size=1, aes(xintercept = actual_price, linetype="Actual price"))+
  scale_fill_manual(guide = FALSE,values=my_pallete) + scale_colour_manual(guide = FALSE, values=my_pallete)+
  facet_grid(vars(condition), vars(q_id), scale='free')+
  xlab("Participants' price prediction")+
  ylab("Number of participants")+
  scale_x_continuous(breaks = breaks, labels = sprintf('$%dM', breaks)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position="top", legend.title = element_blank(), legend.key = element_rect(size = 10), legend.key.size = unit(1.5, 'lines'), legend.text=element_text(size=12),
        axis.title.x = element_text(size=20), axis.title.y = element_text(size=18), axis.text.x = element_text(angle = 45, size=10), axis.text.y = element_text(size=14),
        strip.text = element_text(size=14))
ggsave(training_dist_plot, filename = "figures/exp4/dist_training_final_prediction.pdf", height=10, width=10)

#######################################################
# Testing phase
#######################################################
# read testing phase data
responses <- read_csv("data/exp4/data.csv")

# recode conditions and compute outcome measures
responses <- responses %>%
  mutate(condition = recode_factor(as.factor(condition),
                                   "C0" = "BB-FOCUS",
                                   "C1" = "BB-NO-FOCUS",
                                   "C2" = "CLEAR-FOCUS",
                                   "C3" = "CLEAR-NO-FOCUS")) %>%
  mutate(condition = factor(condition, levels = c("CLEAR-NO-FOCUS", "BB-NO-FOCUS", "CLEAR-FOCUS", "BB-FOCUS"))) %>%

  mutate(pred_err = abs(final_pred - actual_price),
         trust = abs(final_pred - model_pred),
         signed_dev = (final_pred - model_pred)) %>%
  mutate(transparency=ifelse(condition=="BB-FOCUS" | condition == "BB-NO-FOCUS","BB", 'CLEAR')) %>%
  mutate(attention = ifelse(condition == "BB-FOCUS" | condition == "CLEAR-FOCUS", 'FOCUS', 'NO-FOCUS')) %>%
  mutate(apt_label = ifelse(q_id == 10, "Apartment 6: 1 bed, 3 bath, 726 sq ft",
                                  ifelse(q_id == 11, 'Apartment 7: 1 bed, 1 bath, 788 sq ft',
                                         ifelse(q_id == 12, 'Apartment 8: 1 bed, 3 bath, 350 sq ft', 'NORMAL')))) %>%
  ungroup()

#########################################
# histograms
#########################################
# users' final prediction
breaks <- seq(0,3,1)
responses_w_NA <- responses

# recode apartment IDs
responses_w_NA$q_id <- responses_w_NA$q_id + 1
responses_w_NA <- within(responses_w_NA, q_id[q_id==13] <- 14)
responses_w_NA <- within(responses_w_NA, q_id[q_id==12] <- 13)
responses_w_NA <- within(responses_w_NA, q_id[q_id==11] <- 12)


# replace -1 for actual price of synthetic apartments with NA
responses_w_NA$actual_price <- replace(responses_w_NA$actual_price, which(responses_w_NA$actual_price == -1), NA)

final_prediction_dist_plot <- ggplot(responses_w_NA, aes(x = final_pred, fill=condition)) +
  geom_histogram(binwidth = 0.1, alpha = 0.5) +
  geom_vline(alpha=.2, size=1, aes(xintercept = model_pred, linetype="Model's prediction"))+
  geom_vline(alpha=.2, size=1, aes(xintercept = actual_price, linetype="Actual price"))+
  scale_fill_manual(guide = FALSE,values=my_pallete) + scale_colour_manual(guide = FALSE, values=my_pallete)+
  facet_grid(vars(condition),vars(q_id), scale='free')+
  xlab("Participants' price prediction")+
  ylab("Number of participants")+
  scale_x_continuous(breaks = breaks, labels = sprintf('$%dM', breaks)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position="top", legend.title = element_blank(), legend.key = element_rect(size = 10), legend.key.size = unit(1.5, 'lines'), legend.text=element_text(size=12),
        axis.title.x = element_text(size=20), axis.title.y = element_text(size=18), axis.text.x = element_text(angle = 45, size=10), axis.text.y = element_text(size=14),
        strip.text = element_text(size=14))
ggsave(final_prediction_dist_plot, filename = "figures/exp4/dist_final_prediction.pdf", height=10, width=10)



hist_responses <- mutate(responses, actual_price = ifelse(actual_price == -1, NA, actual_price))
hist_responses <-hist_responses %>% mutate(q_id_factor =  recode_factor(as.factor(q_id),
                                                                        "0" = "1",
                                                                        "5" = "6",
                                                                        "7" = "8",
                                                                        "8" = "9",
                                                                        "9" = "10",
                                                                        "10" = "12 (Apartment 6)",
                                                                        "11" = "13",
                                                                        "12" = "14 (Apartment 8)"))
# users' final prediction
# breaks <- seq(0,3,1)
# prediction_dist_plot <- ggplot(hist_responses, aes(x = final_pred, fill=condition)) +
#   geom_histogram(binwidth = 0.1, alpha = 0.5) +
#   geom_vline(alpha=.2, size=1, aes(xintercept = model_pred, linetype="Model's prediction"))+
#   geom_vline(alpha=.2, size=1, aes(xintercept = actual_price, linetype="Actual price"))+
#   scale_fill_manual(guide = FALSE,values=my_pallete) + scale_colour_manual(guide = FALSE, values=my_pallete)+
#   facet_grid(vars(q_id_factor), vars(condition))+
#   xlab("Participants' price prediction")+
#   ylab("Number of participants")+
#   scale_x_continuous(breaks = breaks, labels = sprintf('$%gM', breaks)) +
#   theme(legend.position="top", legend.title = element_blank(), legend.key = element_rect(size = 10), legend.key.size = unit(1.5, 'lines'), legend.text=element_text(size=12),axis.title.x = element_text(size=20), axis.title.y = element_text(size=18), axis.text.x = element_text(angle = 45, size=10), axis.text.y = element_text(size=14), strip.text = element_text(size=8))
# ggsave(prediction_dist_plot, filename = "figures/exp4/dist_final_prediction.pdf", height=10, width=12)


######################################################################
# H1: effect of attention check (across both clear and bb conditions)
######################################################################

# effect size
q6_data <- filter(responses , q_id == "10" | q_id == "12")
focus_data <- filter(q6_data, condition == "CLEAR-FOCUS" | condition == "BB-FOCUS")
no_focus_data <- filter(q6_data, condition == "CLEAR-NO-FOCUS" | condition == "BB-NO-FOCUS")
effect_size <- cohensD(focus_data$signed_dev, no_focus_data$signed_dev)

q8_data <- filter(responses , q_id == "12" | q_id == "12")
focus_data <- filter(q8_data, condition == "CLEAR-FOCUS" | condition == "BB-FOCUS")
no_focus_data <- filter(q8_data, condition == "CLEAR-NO-FOCUS" | condition == "BB-NO-FOCUS")
effect_size <- cohensD(focus_data$signed_dev, no_focus_data$signed_dev)

responses %>%
  filter(q_id %in% c(10, 12)) %>%
  group_by(q_id) %>%
  do(tidy(t.test(signed_dev ~ attention, data = .)))

responses %>%
  filter(q_id == 11) %>%
  group_by(q_id) %>%
  do(tidy(t.test(signed_dev ~ attention, data = .)))

######################################################################
# H3: effect of transparency in conditions without attention check
######################################################################
q6_data <- filter(responses , q_id == "10")
clear_data <- filter(q6_data, condition == "CLEAR-NO-FOCUS")
bb_data <- filter(q6_data, condition == "BB-NO-FOCUS")
effect_size <- cohensD(clear_data$signed_dev, bb_data$signed_dev)

q8_data <- filter(responses ,  q_id == "12")
clear_data <- filter(q8_data, condition == "CLEAR-NO-FOCUS")
bb_data <- filter(q8_data, condition == "BB-NO-FOCUS")
effect_size <- cohensD(clear_data$signed_dev, bb_data$signed_dev)


responses %>%
  filter(q_id %in% c(10, 12), attention == "NO-FOCUS") %>%
  group_by(q_id) %>%
  do(tidy(t.test(signed_dev ~ transparency, data = .)))


responses %>%
  filter(q_id == 11, attention == "NO-FOCUS") %>%
  group_by(q_id) %>%
  do(tidy(t.test(signed_dev ~ transparency, data = .)))
######################################################################
# H3: effect of transparency in conditions with attention check
######################################################################
q6_data <- filter(responses , q_id == "10")
clear_data <- filter(q6_data, condition == "CLEAR-FOCUS")
bb_data <- filter(q6_data, condition == "BB-FOCUS")
effect_size <- cohensD(clear_data$signed_dev, bb_data$signed_dev)

q8_data <- filter(responses ,  q_id == "12")
clear_data <- filter(q8_data, condition == "CLEAR-FOCUS")
bb_data <- filter(q8_data, condition == "BB-FOCUS")
effect_size <- cohensD(clear_data$signed_dev, bb_data$signed_dev)

responses %>%
  filter(q_id %in% c(10, 12), attention == "FOCUS") %>%
  group_by(q_id) %>%
  do(tidy(t.test(signed_dev ~ transparency, data = .)))

responses %>%
  filter(q_id == 11, attention == "FOCUS") %>%
  group_by(q_id) %>%
  do(tidy(t.test(signed_dev ~ transparency, data = .)))


# plot q8
q8_data <- filter(responses,q_id == 12)
q8_lm_model <- lm(final_pred ~ condition , data = q8_data)
summary(q8_lm_model)


plot_data <- q8_data %>%
  mutate(final_pred = final_pred * 1e6) %>%
  group_by(condition) %>%
  summarize(mean = mean(abs(final_pred)),
            se = sd(abs(final_pred)) / sqrt(n()),
            model_pred = mean(model_pred * 1e6),
            facet = 'Apartment 8: 1 bed, 3 bath, 350 sq ft')
print(plot_data$mean)

breaks <- seq(0, 1.5e6, by = 500e3)
q8_final_pred_plot <- ggplot(plot_data, aes(x = condition, y = mean, color = condition, fill = condition, label=sprintf("$%gM", round(mean/1e6, digits=2)))) +
  geom_bar(stat = "identity", alpha=.5) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.1, color = "black") +
  geom_hline(aes(yintercept = model_pred, linetype = "Model's prediction"), alpha = 0.5, size=2) +
  scale_fill_manual(guide = FALSE,values=my_pallete) + scale_colour_manual(guide = FALSE, values=my_pallete)+
  xlab('') +
  ylab('Mean participant prediction')+
  geom_text(colour='black', vjust=-1.5)+
  scale_y_continuous(breaks = breaks, limits=c(0, 1.5e6), labels = sprintf('$%gM', breaks/1e6)) +
  scale_linetype_manual(values = "dashed") +
  theme(legend.position="bottom",axis.title.x = element_text(size=14), axis.title.y = element_text(size=18),
        axis.text.x = element_text(size=9), axis.text.y = element_text(size=14),
        legend.text=element_text(size=9), legend.title = element_blank(), legend.spacing.x=unit(0.5,"line"), legend.margin=margin(t=-.5, r=0, b=0, l=0, unit="cm"),
        strip.text = element_text(size=9))+
  facet_wrap(~ facet)
ggsave(q8_final_pred_plot, filename = "figures/exp4/q8_pred.pdf", height=4, width=6)

# plot for q6
q6_data <- filter(responses,q_id == 10)
q6_lm_model <- lm(final_pred ~ condition , data = q6_data)
summary(q6_lm_model)


plot_data <- q6_data %>%
  mutate(final_pred = final_pred * 1e6) %>%
  group_by(condition) %>%
  summarize(mean = mean(abs(final_pred)),
            se = sd(abs(final_pred)) / sqrt(n()),
            model_pred = mean(model_pred * 1e6),
            facet = 'Apartment 6: 1 bed, 3 bath, 726 sq ft')
print(plot_data$mean)

breaks <- seq(0, 1.5e6, by = 500e3)
q6_final_pred_plot <- ggplot(plot_data, aes(x = condition, y = mean, color = condition, fill = condition, label=sprintf("$%gM", round(mean/1e6, digits=2)))) +
  geom_bar(stat = "identity", alpha=.5) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.1, color = "black") +
  geom_hline(aes(yintercept = model_pred, linetype = "Model's prediction"), alpha = 0.5, size=2) +
  scale_fill_manual(guide = FALSE,values=my_pallete) + scale_colour_manual(guide = FALSE, values=my_pallete)+
  xlab('') +
  ylab('Mean participant prediction')+
  geom_text(colour='black', vjust=-1.5)+
  scale_y_continuous(breaks = breaks, limits=c(0, 1.5e6), labels = sprintf('$%gM', breaks/1e6)) +
  scale_linetype_manual(values = "dashed") +
  theme(legend.position="bottom",axis.title.x = element_text(size=14), axis.title.y = element_text(size=18),
        axis.text.x = element_text(size=9), axis.text.y = element_text(size=14),
        legend.text=element_text(size=9), legend.title = element_blank(), legend.spacing.x=unit(0.5,"line"), legend.margin=margin(t=-.5, r=0, b=0, l=0, unit="cm"),
        strip.text = element_text(size=9))+
  facet_wrap(~ facet)
ggsave(q6_final_pred_plot, filename = "figures/exp4/q6_pred.pdf", height=4, width=6)

# plot for q7
q7_data <- filter(responses,q_id == 11)
q7_lm_model <- lm(final_pred ~ condition , data = q7_data)
summary(q6_lm_model)


plot_data <- q7_data %>%
  mutate(final_pred = final_pred * 1e6) %>%
  group_by(condition) %>%
  summarize(mean = mean(abs(final_pred)),
            se = sd(abs(final_pred)) / sqrt(n()),
            model_pred = mean(model_pred * 1e6),
            facet = 'Apartment 7: 1 bed, 1 bath, 788 sq ft')
print(plot_data$mean)

breaks <- seq(0, 1.5e6, by = 500e3)
q7_final_pred_plot <- ggplot(plot_data, aes(x = condition, y = mean, color = condition, fill = condition, label=sprintf("$%gM", round(mean/1e6, digits=2)))) +
  geom_bar(stat = "identity", alpha=.5) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.1, color = "black") +
  geom_hline(aes(yintercept = model_pred, linetype = "Model's prediction"), alpha = 0.5, size=2) +
  scale_fill_manual(guide = FALSE,values=my_pallete) + scale_colour_manual(guide = FALSE, values=my_pallete)+
  xlab('') +
  ylab('Mean participant prediction')+
  geom_text(colour='black', vjust=-1.5)+
  scale_y_continuous(breaks = breaks, limits=c(0, 1.5e6), labels = sprintf('$%gM', breaks/1e6)) +
  scale_linetype_manual(values = "dashed") +
  theme(legend.position="bottom",axis.title.x = element_text(size=14), axis.title.y = element_text(size=18),
        axis.text.x = element_text(size=9), axis.text.y = element_text(size=14),
        legend.text=element_text(size=9), legend.title = element_blank(), legend.spacing.x=unit(0.5,"line"), legend.margin=margin(t=-.5, r=0, b=0, l=0, unit="cm"),
        strip.text = element_text(size=9))+
  facet_wrap(~ facet)
ggsave(q7_final_pred_plot, filename = "figures/exp4/q7_pred.pdf", height=4, width=6)

