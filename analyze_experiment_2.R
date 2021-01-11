library(lmerTest)
library(lsmeans)
library(scales)
library(ggrepel)
library(readr)
library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(xtable)
library(lsr)
theme_set(theme_bw())

source('lib.R')

if (!dir.exists('figures/exp2'))
   dir.create('figures/exp2', recursive = T)

emm_options(pbkrtest.limit = 12500)
emm_options(lmerTest.limit = 12500)
my_pallete <- c('#92c5de','#0571b0','#f4a582','#ca0020','#404040')
#######################################################
# Training phase
#######################################################
# read data
training_responses <- read_csv("data/exp2/data_training.csv")

# recode conditions
training_responses <- training_responses %>%
  mutate(condition = recode_factor(as.factor(condition),
                                   "C1" = "CLEAR-2",
                                   "C3" = "CLEAR-8",
                                   "C0" = "BB-2",
                                   "C2" = "BB-8",
                                   "C4" = "NO-MODEL"))


# break data in two parts for histograms
# update apartment indeces to start from 1
training_responses1 <- filter(training_responses, q_id < 5)
training_responses1$q_id <- training_responses1$q_id + 1
training_responses2 <- filter(training_responses, q_id >= 5)
training_responses2$q_id <- training_responses2$q_id + 1

# plot
breaks <- seq(0,300000,100000)
ggplot(training_responses1, aes(x = final_pred, fill=condition)) +
  geom_histogram(binwidth = 10000, alpha = 0.5) +

  #geom_vline(alpha=.5, size=1, data = plot_data, aes(xintercept = median_final_pred, linetype="Particpants' median final prediction"))+
  geom_vline(alpha=.2, size=1, aes(xintercept = model_pred, linetype="Model's prediction"))+
  geom_vline(alpha=.2, size=1, aes(xintercept = actual_price, linetype="Actual price"))+
  scale_fill_manual(guide = FALSE,values=my_pallete) + scale_colour_manual(guide = FALSE, values=my_pallete)+
  facet_grid(vars(condition), vars(q_id), scale='free')+
  xlab("Participants' price prediction")+
  ylab("Number of participants")+
  scale_x_continuous(breaks = breaks, labels = sprintf('$%dk', breaks/1e3)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position="top", legend.title = element_blank(), legend.key = element_rect(size = 10), legend.key.size = unit(1.5, 'lines'), legend.text=element_text(size=12),
        axis.title.x = element_text(size=20), axis.title.y = element_text(size=18), axis.text.x = element_text(vjust=.5, angle = 45, size=10), axis.text.y = element_text(size=14),
        strip.text = element_text(size=14))
ggsave(filename = "figures/exp2/dist_training_final_prediction_1.pdf", height=10, width=10)

ggplot(training_responses2, aes(x = final_pred, fill=condition)) +
  geom_histogram(binwidth = 10000, alpha = 0.5) +
  geom_vline(alpha=.2, size=1, aes(xintercept = model_pred, linetype="Model's prediction"))+
  geom_vline(alpha=.2, size=1, aes(xintercept = actual_price, linetype="Actual price"))+
  scale_fill_manual(guide = FALSE,values=my_pallete) + scale_colour_manual(guide = FALSE, values=my_pallete)+
  facet_grid(vars(condition), vars(q_id), scale='free')+
  xlab("Participants' price prediction")+
  ylab("Number of participants")+
  scale_x_continuous(breaks = breaks, labels = sprintf('$%dk', breaks/1e3)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position="top", legend.title = element_blank(), legend.key = element_rect(size = 10), legend.key.size = unit(1.5, 'lines'), legend.text=element_text(size=12),
        axis.title.x = element_text(size=20), axis.title.y = element_text(size=18), axis.text.x = element_text(vjust=.5, angle = 45, size=10), axis.text.y = element_text(size=14),
        strip.text = element_text(size=14))
ggsave(filename = "figures/exp2/dist_training_final_prediction_2.pdf", height=10, width=10)

#######################################################
# Testing phase
#######################################################
# read data
responses <- read_csv("data/exp2/data.csv")


# recode conditions and compute outcome measures
responses <- responses %>%
  mutate(condition = recode_factor(as.factor(condition),
                                   "C1" = "CLEAR-2",
                                   "C3" = "CLEAR-8",
                                   "C0" = "BB-2",
                                   "C2" = "BB-8",
                                   "C4" = "NO-MODEL")) %>%
  mutate(pred_err = abs(final_pred - actual_price)/1000000.0,
         model_err = abs(model_pred - actual_price)/1000000.0,
         deviation = abs(final_pred - model_pred)/1000000.0,
         sim_err = abs(user_model_pred - model_pred)/1000000.0) %>%
  ungroup()


#########################################
# histograms
#########################################
# users' final prediction
breaks <- seq(0,300000,100000)
responses1 <- filter(responses, q_id < 6)
responses1$q_id <- responses1$q_id + 1
responses2 <- filter(responses, q_id >= 6)
responses2$q_id <- responses2$q_id + 1

# replace -1 for actual price of apartment 12 (synthetic) with NA
responses1$actual_price <- replace(responses1$actual_price, which(responses1$actual_price == -1e5), NA)
responses2$actual_price <- replace(responses2$actual_price, which(responses2$actual_price == -1e5), NA)

ggplot(responses1, aes(x = final_pred, fill=condition)) +
  geom_histogram(binwidth = 10000, alpha = 0.5) +
  geom_vline(alpha=.2, size=1, aes(xintercept = model_pred, linetype="Model's prediction"))+
  geom_vline(alpha=.2, size=1, aes(xintercept = actual_price, linetype="Actual price"))+
  scale_fill_manual(guide = FALSE,values=my_pallete) + scale_colour_manual(guide = FALSE, values=my_pallete)+
  facet_grid(vars(condition), vars(q_id), scale='free')+
  xlab("Participants' price prediction")+
  ylab("Number of participants")+
  scale_x_continuous(breaks = breaks, labels = sprintf('$%dk', breaks/1e3)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position="top", legend.title = element_blank(), legend.key = element_rect(size = 10), legend.key.size = unit(1.5, 'lines'), legend.text=element_text(size=12),
        axis.title.x = element_text(size=20), axis.title.y = element_text(size=18), axis.text.x = element_text(vjust=.5, angle = 45, size=10), axis.text.y = element_text(size=14),
        strip.text = element_text(size=14))
ggsave(filename = "figures/exp2/dist_final_prediction_1.pdf", height=10, width=10)

ggplot(responses2, aes(x = final_pred, fill=condition)) +
  geom_histogram(binwidth = 10000, alpha = 0.5) +
  geom_vline(alpha=.2, size=1, aes(xintercept = model_pred, linetype="Model's prediction"))+
  geom_vline(alpha=.2, size=1, aes(xintercept = actual_price, linetype="Actual price"))+
  scale_fill_manual(guide = FALSE,values=my_pallete) + scale_colour_manual(guide = FALSE, values=my_pallete)+
  facet_grid(vars(condition), vars(q_id), scale='free')+
  xlab("Participants' price prediction")+
  ylab("Number of participants")+
  scale_x_continuous(breaks = breaks, labels = sprintf('$%dk', breaks/1e3)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position="top", legend.title = element_blank(), legend.key = element_rect(size = 10), legend.key.size = unit(1.5, 'lines'), legend.text=element_text(size=12),
        axis.title.x = element_text(size=20), axis.title.y = element_text(size=18), axis.text.x = element_text(vjust=.5, angle = 45, size=10), axis.text.y = element_text(size=14),
        strip.text = element_text(size=14))
ggsave(filename = "figures/exp2/dist_final_prediction_2.pdf", height=10, width=10)


#########################################
# hypotheses analyses and plots
#########################################
# limit everything to first 10 questions for now
responses_normal <- filter(responses, q_id < 10)

# limit to the four primary conditions
# add transparency and number of features factors
model_data <- filter(responses_normal, condition != "NO-MODEL") %>%
  mutate(transparency = ifelse(condition == "CLEAR-2" | condition == "CLEAR-8", "CLEAR", "BB")) %>%
  mutate(num_features = ifelse(condition == "CLEAR-2" | condition == "BB-2", "two", "eight"))

########################################
# DEVIATION FROM THE MODEL
########################################
# fit the two factor model
hlm_model <- lmer(deviation ~ transparency*num_features + (1|worker_id), data=model_data)
summary(hlm_model)

# look at ANOVA
anova_table <- anova(hlm_model)

# export in LaTEX format
xtable(anova_table, digits=c(0,3,3,2,2,2,4))

# get the marginal means for all combinations of transparency and number of features
means <- emmeans(hlm_model, c("transparency", "num_features"))

# setup contrasts
clist <- list(clear2_vs_bb8 = c(-1, 0, 0, 1))
emmeans::contrast(means, clist)

# setup contrasts
clist <- list(clear8_vs_else = c(-1, 3, -1, -1))
emmeans::contrast(means, clist)

# setup contrasts
clist <- list(clear2_vs_else = c(-1, -1, -1, 3))
emmeans::contrast(means, clist)

model_data %>%
  mutate(outcome = deviation * 1e3) %>%
  plot_distributions_with_means(., 'Average deviation from the model', 'Proportion of participants', my_pallete)
ggsave(file = 'figures/exp2/dev_from_model.pdf', height =4, width = 4)

########################################
# Self-reported trust
########################################
# run 2-by-2 anova for self-reported trust
hlm_model <- lmer(model_conf ~ transparency * num_features + (1 | worker_id), data = model_data)
anova(hlm_model)

# clear-2 vs. bb-8
# get the marginal means for all combinations of transparency and number of features
means <- emmeans(hlm_model, c("transparency", "num_features"))

# setup contrasts
clist <- list(clear2_vs_bb8 = c(-1, 0, 0, 1))
emmeans::contrast(means, clist)

# effect sizes
clear2_data <- filter(model_data, condition == "CLEAR-2")
bb8_data <- filter(model_data, condition == "BB-8")
effect_size <- cohensD(clear2_data$model_conf, bb8_data$model_conf)

########################################
# Simulation error
########################################
# fit the two factor model
hlm_model <- lmer(sim_err ~ transparency*num_features + (1|worker_id), data=model_data)
summary(hlm_model)

# look at ANOVA
anova_table <- anova(hlm_model)

# export in LaTEX format
xtable(anova_table, digits=c(0,3,3,2,2,2,4))

# get the marginal means for all combinations of transparency and number of features
means <- emmeans(hlm_model, c("transparency", "num_features"))

# setup contrasts
clist <- list(clear2_vs_else = c(-1, -1, -1, 3))
emmeans::contrast(means, clist)

clist <- list(clear8_vs_else = c(-1, 3, -1, -1))
emmeans::contrast(means, clist)

model_data %>%
  mutate(outcome = sim_err * 1e3) %>%
  plot_distributions_with_means(., 'Average simulation error', 'Proportion of participants', my_pallete, xlim_percentile = .95)
ggsave(file = 'figures/exp2/sim_error.pdf', height = 4, width = 4)


########################################
# PREDICTION ERROR
########################################
# primary conditions
hlm_model <- lmer(pred_err ~ condition + (1|worker_id), data=model_data)
summary(hlm_model)

# look at ANOVA
anova_table <- anova(hlm_model)

# export in LaTEX format
xtable(anova_table, digits=c(0,3,3,2,2,2,4))

# get the marginal means for conditions
means <- emmeans(hlm_model, "condition")

# two-way anova
hlm_model <- lmer(pred_err ~ transparency * num_features + (1|worker_id), data=model_data)
summary(hlm_model)

# setup contrasts
means <- emmeans(hlm_model, c("transparency","num_features"))
clist <- list(clear8_vs_else = c(-1, 3, -1, -1))
emmeans::contrast(means, clist)


# look at ANOVA
anova_table <- anova(hlm_model)

# export in LaTEX format
xtable(anova_table, digits=c(0,3,3,2,2,2,4))

# all conditions including baseline
model_baseline_data <- filter(responses_normal, q_id < 10)

# fit the one factor model
hlm_model <- lmer(pred_err ~ condition + (1|worker_id), data=model_baseline_data)
summary(hlm_model)

# look at ANOVA
anova_table <- anova(hlm_model)

# export in LaTEX format
xtable(anova_table)

# get the marginal means for conditions
means <- emmeans(hlm_model, "condition")

# setup contrasts
clist <- list(model_vs_baseline = c(-1, -1, -1, -1, 4))
emmeans::contrast(means, clist)

model_baseline_data %>%
  mutate(outcome = pred_err * 1e3) %>%
  plot_distributions_with_means(., 'Average prediction error', 'Proportion of participants', my_pallete,
                                xlim_percentile=0.95, vertical_line = mean(model_baseline_data$model_err) * 1e3)
ggsave(file = 'figures/exp2/prediction_error.pdf', height = 4.5, width =4)


####################################
# last two questions (unusual apartments)
####################################
responses_unusual <- filter(responses, q_id >= 10)

########################################
# deviation for q11
########################################
q11_data <- filter(responses_unusual, q_id == 10)

# prediction error in no-model vs rest
lm_model <- lm(pred_err ~ condition, data=q11_data)
means <- emmeans(lm_model, c("condition"))

clist <- list(baseline_vs_rest = c(-1, -1, -1, -1, 4))
emmeans::contrast(means, clist)

q11_data <- filter(q11_data, condition != "NO-MODEL") %>%
  mutate(transparency = ifelse(condition == "CLEAR-2" | condition == "CLEAR-8", "CLEAR", "BB")) %>%
  mutate(num_features = ifelse(condition == "CLEAR-2" | condition == "BB-2", "two", "eight"))


# two-way anova on pred error
q11_anova <- aov(pred_err ~ transparency*num_features, data=q11_data)
summary(q11_anova)

# prediction error transparent vs. black-box
lm_model <- lm(pred_err ~ transparency*num_features, data=q11_data)
means <- emmeans(lm_model,  c("transparency", "num_features"))
clist <- list(clear_vs_bb = c(-1, 1, -1, 1))
emmeans::contrast(means, clist)

# fit the one factor model
lm_model <- lm(deviation ~ condition, data=q11_data)
summary(lm_model)


# look at one-way ANOVA
anova(lm_model)

# run 2-by-2 anova for deviation on q11
# fit the two factor model
lm_model <- lm(deviation ~ transparency*num_features, data=q11_data)
summary(lm_model)

# look at ANOVA
anova_table <- anova(lm_model)

# export in LaTEX format
xtable(anova_table)

# get the marginal means for all combinations of transparency and number of features
means <- emmeans(lm_model, c("transparency", "num_features"))

# setup contrasts
clist <- list(clear_vs_bb = c(-1, 1, -1, 1))
emmeans::contrast(means, clist)

########################################
# deviation for q12
########################################
# comparison of prediction error for baseline with the rest
q12_data <- filter(responses_unusual, q_id == 11)

#fit a model for deviation
q12_data <- filter(q12_data, condition != "NO-MODEL") %>%
  mutate(transparency = ifelse(condition == "CLEAR-2" | condition == "CLEAR-8", "CLEAR", "BB")) %>%
  mutate(num_features = ifelse(condition == "CLEAR-2" | condition == "BB-2", "two", "eight"))

# fit the one factor model
lm_model <- lm(deviation ~ condition, data=q12_data)
summary(lm_model)

# look at one-way ANOVA
anova(lm_model)

# run 2-by-2 anova for deviation on q11
# fit the two factor model
lm_model <- lm(deviation ~ transparency*num_features, data=q12_data)
summary(lm_model)


# look at ANOVA
anova_table <- anova(lm_model)

# export in LaTEX format
xtable(anova_table)

# get the marginal means for all combinations of transparency and number of features
means <- emmeans(lm_model, c("transparency", "num_features"))

# setup contrasts
clist <- list(clear_vs_bb = c(-1, 1, -1, 1))
emmeans::contrast(means, clist)

plot_data <- q12_data %>%
  group_by(condition) %>%
  summarize(mean = mean(final_pred),
            se = sd(final_pred) / sqrt(n()),
            model_pred = mean(model_pred),
            facet = 'Experiment 2, apartment 12: 1 bed, 3 bath')
print(plot_data$mean)
breaks <- seq(0, 150000, 50000)
ggplot(plot_data, aes(x = condition, y = mean, color = condition, fill = condition, label=sprintf("$%gk", round(mean/1e3, digits=0)))) +
  geom_bar(stat = "identity", alpha=.5) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.1, color = "black") +
  geom_hline(aes(yintercept = model_pred, linetype = "Model's prediction"), alpha = 0.5, size=1) +
  scale_fill_manual(guide = FALSE,values=my_pallete) + scale_colour_manual(guide = FALSE, values=my_pallete)+
  xlab('') +
  ylab('Mean participant prediction')+
  geom_text(colour='black', vjust=-1.5)+
  scale_y_continuous(breaks = breaks, labels = sprintf('$%gk', breaks/1e3)) +
  scale_linetype_manual(values = "dashed") +
  theme(legend.position="bottom",axis.title.x = element_text(size=14), axis.title.y = element_text(size=18),
        axis.text.x = element_text(size=9), axis.text.y = element_text(size=14),
        legend.text=element_text(size=9), legend.title = element_blank(), legend.spacing.x=unit(0.5,"line"), legend.margin=margin(t=-.5, r=0, b=0, l=0, unit="cm"),
        strip.text = element_text(size=9))+
  facet_wrap(~ facet)
ggsave(filename = "figures/exp2/q12_pred.pdf", height=6, width=6)
