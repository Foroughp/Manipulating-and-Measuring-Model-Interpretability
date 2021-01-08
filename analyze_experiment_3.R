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
library(emmeans)
library(lsr)
theme_set(theme_bw())
emm_options(pbkrtest.limit = 12500)
emm_options(lmerTest.limit = 12500)
my_pallete <- c('#92c5de','#0571b0','#f4a582','#ca0020','#bababa')

source('lib.R')

if (!dir.exists('figures/exp3'))
   dir.create('figures/exp3', recursive = T)

# read data
woa_responses <- read_csv("data/exp3/data.csv")

# recode conditions and compute outcome measures
woa_responses <- woa_responses %>%
  mutate(condition = recode_factor(as.factor(condition),
                                   "C1" = "CLEAR-2",
                                   "C3" = "CLEAR-8",
                                   "C0" = "BB-2",
                                   "C2" = "BB-8",
                                   "C4" = "EXPERT")) %>%
  mutate(pred_err = abs(final_pred - actual_price),
         deviation = abs(final_pred - model_pred),
         woa = abs(final_pred - user_init_pred) / abs(model_pred - user_init_pred)) %>%
  ungroup()


#########################################
# histograms
#########################################
# users' final prediction
breaks <- seq(0,3,1)
responses1 <- filter(woa_responses, q_id < 6)
responses1$q_id <- responses1$q_id + 1
responses2 <- filter(woa_responses, q_id >= 6)
responses2$q_id <- responses2$q_id + 1

# replace -1 for actual price of apartment 12 (synthetic) with NA
responses1$actual_price <- replace(responses1$actual_price, which(responses1$actual_price == -1), NA)
responses2$actual_price <- replace(responses2$actual_price, which(responses2$actual_price == -1), NA)

# shift plot
plot_shifts <- function(response_data){
  # summary stats
  plot_data <- response_data %>%
    group_by(condition, q_id) %>%
    summarize(mean_user_init_pred = mean(user_init_pred),
              mean_final_pred = mean(final_pred)) %>%
    group_by(q_id) %>%
    arrange(q_id, condition) %>%
    mutate(y = 1:n())

    breaks <- seq(0,3,1)
    response_data %>%
    group_by(condition, q_id) %>%
    filter(user_init_pred >= quantile(user_init_pred, 0.05),
           user_init_pred <= quantile(user_init_pred, 0.95),
           final_pred >= quantile(final_pred, 0.05),
           final_pred <= quantile(final_pred, 0.95)) %>%
    ungroup() %>%
    ggplot(aes(x = final_pred, fill = condition, color = condition)) +
    geom_histogram(aes(x = user_init_pred), alpha = 0.2, linetype = 'dotted', binwidth = 0.1) +
    geom_histogram(alpha = 0.5, binwidth = 0.1) +
    geom_point(data = plot_data, aes(x = mean_final_pred, y = 80), shape = 20) +
    geom_point(data = plot_data, aes(x = mean_user_init_pred, y = 80), shape = 1) +
    geom_segment(data = plot_data,
                 aes(x = mean_user_init_pred, y = 95,
                     xend = mean_final_pred, yend = 95),
                 arrow = arrow(length = unit(0.03, "npc"))) +
    geom_vline( alpha = .2, size = .5, aes(xintercept = model_pred, linetype = "Model's prediction")) +
    geom_vline(alpha=.2, size=.5, aes(xintercept = actual_price, linetype="Actual price"))+

    facet_grid(condition ~ q_id, scale = "free_x") +
    scale_x_continuous(breaks = breaks, labels = sprintf('$%dM', breaks)) +
    scale_fill_manual(guide = FALSE, values=my_pallete) +
    scale_colour_manual(guide = FALSE, values=my_pallete) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
           legend.position="top",legend.title = element_blank(), legend.key = element_rect(size = 10), legend.key.size = unit(1.5, 'lines'), legend.text=element_text(size=12),
          axis.title.x = element_text(size=20), axis.title.y = element_text(size=18), axis.text.x = element_text(angle = 45, size=10), axis.text.y = element_text(size=14),
          strip.text = element_text(size=14))+
    labs(x = "Participants' initial and final predictions",
         y = "Number of participants")
}

plot_shifts(responses1)
ggsave(file = 'figures/exp3/shift_histograms_1.pdf', height=10, width=10)
plot_shifts(responses2)
ggsave(file = 'figures/exp3/shift_histograms_2.pdf', height=10, width=10)

# R2: how many subjects think the model underestimates the sale price of the unusual 3 bathroom apt?

woa_responses %>%
  filter(q_id == 11) %>%
  summarize(mean(user_init_pred < model_pred))

woa_responses %>%
  filter(q_id == 11) %>%
  summarize(mean(user_init_pred > model_pred))

# R2: How many choose the midpoint of the scale? The ends?
breaks <- seq(0,3,1)
woa_responses %>%
  count(user_init_pred) %>%
  mutate(frac = n / sum(n)) %>%
  ggplot(aes(x = user_init_pred, y = frac)) +
  scale_x_continuous(breaks = breaks, labels = sprintf('$%dM', breaks)) +
  geom_bar(stat = "identity")+
  xlab("Participants' initial prediction")+
  ylab("Fraction of responses")+
  theme(legend.position="bottom",axis.title.x = element_text(size=14), axis.title.y = element_text(size=18),
        axis.text.x = element_text(size=9), axis.text.y = element_text(size=14),
        legend.text=element_text(size=9), legend.title = element_blank(), legend.spacing.x=unit(0.5,"line"), legend.margin=margin(t=-.5, r=0, b=0, l=0, unit="cm"),
        strip.text = element_text(size=9))+
ggsave('figures/exp3/init_pred_hist.jpg',  height=4, width=6)

nrow(woa_responses[woa_responses$user_init_pred == 0,])
nrow(woa_responses[woa_responses$user_init_pred == 3,])
nrow(woa_responses[woa_responses$user_init_pred == 1.5,])

##########
# limit everything to first 10 questions for now
woa_responses_normal <- filter(woa_responses, q_id < 10)

model_expert_data <- filter(woa_responses_normal, q_id < 10)%>%
  mutate(transparency = ifelse(condition == "CLEAR-2" | condition == "CLEAR-8", "CLEAR", "BB")) %>%
  mutate(num_features = ifelse(condition == "CLEAR-2" | condition == "BB-2", "two", "eight"))
########################################
# prediction error
########################################
model_data <- filter(model_expert_data, condition != "EXPERT")

hlm_model <- lmer(pred_err ~ transparency*num_features + (1|worker_id), data=model_data)
summary(hlm_model)

# get the marginal means for all combinations of transparency and number of features
means <- emmeans(hlm_model, c("transparency", "num_features"))

# setup contrasts
# clear vs. bb
clist <- list(clear_vs_bb = c(-1, 1, -1, 1))
emmeans::contrast(means, clist)

########################################
# deviation
########################################
model_data <- filter(model_expert_data, condition != "EXPERT")

# clear2 vs bb8
hlm_model <- lmer(deviation ~ transparency*num_features + (1|worker_id), data=model_data)
summary(hlm_model)

# look at ANOVA
anova_table <- anova(hlm_model)

# export in LaTEX format
xtable(anova_table)

# get the marginal means for all combinations of transparency and number of features
means <- emmeans(hlm_model, c("transparency", "num_features"))

# setup contrasts
clist <- list(clear2_vs_bb8 = c(-1, 0, 0, 1))
emmeans::contrast(means, clist)


# POST-HOC: clear-2 else
clist <- list(clear2_vs_else = c(-1, -1, -1, 3))
emmeans::contrast(means, clist)

# bb8 vs EXPERT
hlm_model <- lmer(deviation ~ condition + (1|worker_id), data=model_expert_data)
summary(hlm_model)

# look at ANOVA
anova_table <- anova(hlm_model)

# export in LaTEX format
xtable(anova_table)

# get the marginal means for all combinations of transparency and number of features
means <- emmeans(hlm_model, "condition")

# setup contrasts
clist <- list(bb8_vs_expert = c(0, 0, 0, 1, -1))
emmeans::contrast(means, clist)

# compute the means and standard errors by condition from the model
hlm_model <- lmer(deviation ~ condition + (1 | worker_id), data = model_expert_data)
means <- lsmeansLT(hlm_model, "condition")

model_expert_data %>%
  mutate(outcome = deviation * 1e3) %>%
  plot_distributions_with_means(., 'Average deviation from the model', 'Proportion of participants', my_pallete)
ggsave(file = 'figures/exp3/dev_from_model.pdf', height = 4.5, width = 4)

########################################
# WEIGHT OF ADVICE
########################################
# keeping only finite (non Inf/NA) woa values
model_expert_data_woa <- filter(model_expert_data, is.finite(woa))

model_data_woa <- filter(model_expert_data_woa, condition != "EXPERT")

# clear2 vs bb8
hlm_model <- lmer(woa ~ transparency*num_features + (1|worker_id), data=model_data_woa)
summary(hlm_model)

# look at ANOVA
anova_table <- anova(hlm_model)

# export in LaTEX format
xtable(anova_table)

# get the marginal means for all combinations of transparency and number of features
means <- emmeans(hlm_model, c("transparency", "num_features"))

# setup contrasts

clist <- list(clear2_vs_bb8 = c(-1, 0, 0, 1))
emmeans::contrast(means, clist)

# clear-2 else
clist <- list(clear2_vs_else = c(-1, -1, -1, 3))
emmeans::contrast(means, clist)

# bb8 vs EXPERT
hlm_model <- lmer(woa ~ condition + (1|worker_id), data=model_expert_data_woa)
summary(hlm_model)

# look at ANOVA
anova_table <- anova(hlm_model)

# export in LaTEX format
xtable(anova_table)

# get the marginal means for all combinations of transparency and number of features
means <- emmeans(hlm_model, "condition")

# setup contrasts
clist <- list(bb8_vs_expert = c(0, 0, 0, 1, -1))
emmeans::contrast(means, clist)


hlm_model <- lmer(woa ~ condition + (1 | worker_id), data = model_expert_data_woa)
summary(hlm_model)

model_expert_data_woa %>%
  mutate(outcome = woa) %>%
  plot_distributions_with_means(., 'Average weight of advice (WOA)', 'Proportion of participants', my_pallete, label_format = '%.2f')
ggsave(file = 'figures/exp3/woa.pdf', height = 4.5, width = 4)

# response to R2
# how many WOA responses are outside of [0,1]?

# overall

model_data %>%
  filter(q_id < 10) %>%
  mutate(woa_bin = case_when(
    is.finite(woa) & (user_init_pred <= final_pred & final_pred <= model_pred)~ "u1 < u2 < m (woa <= 1)",
    is.finite(woa) & (final_pred <= user_init_pred & user_init_pred <= model_pred) & woa <= 1 ~ "u2 < u1 < m and woa <= 1",
    is.finite(woa) & (final_pred <= user_init_pred & user_init_pred <= model_pred) & woa > 1 ~ "u2 < u1 < m and woa > 1",
    is.finite(woa) & (user_init_pred <= model_pred & model_pred <= final_pred)~ "u1 < m < u2 (woa > 1)",
    is.finite(woa) & (model_pred <= final_pred & final_pred <= user_init_pred)~ " m < u2 < u1 (woa <= 1)",
    is.finite(woa) & (final_pred <= model_pred & model_pred <= user_init_pred)~ " u2 < m < u1 (woa > 1)",
    is.finite(woa) & (model_pred <= user_init_pred & user_init_pred <= final_pred) & woa <= 1 ~ "m < u1 < u2 and woa <= 1",
    is.finite(woa) & (model_pred <= user_init_pred & user_init_pred <= final_pred) & woa > 1 ~ "m < u1 < u2 and woa > 1",

    !is.finite(woa) ~ "WOA undefined\n(Initial prediction matched model)",
    TRUE ~ "Other"
  )) %>%
  count(woa_bin) %>%
  ungroup() %>%
  mutate(frac_in_bin = n / sum(n),
         se = sqrt(frac_in_bin * (1 - frac_in_bin) / n))

####################################
# last two questions (unusual apartments)
####################################
woa_responses_unusual <- filter(woa_responses, q_id >= 10)

########################################
# deviation for q11
########################################
# anova for q11
q11_model_data <- filter(woa_responses_unusual, condition != "EXPERT", q_id == 10)%>%
  mutate(transparency = ifelse(condition == "CLEAR-2" | condition == "CLEAR-8", "CLEAR", "BB")) %>%
  mutate(num_features = ifelse(condition == "CLEAR-2" | condition == "BB-2", "two", "eight"))

# fit the one factor model
lm_model <- lm(deviation ~ condition, data=q11_model_data)
summary(lm_model)

# look at one-way ANOVA
anova(lm_model)

# run 2-by-2 anova for deviation on q11
# fit the two factor model
lm_model <- lm(deviation ~ transparency*num_features, data=q11_model_data)
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
# DEVIATION for q12
########################################
# anova for q12
q12_model_data <- filter(woa_responses_unusual, condition != "EXPERT", q_id == 11)%>%
  mutate(transparency = ifelse(condition == "CLEAR-2" | condition == "CLEAR-8", "CLEAR", "BB")) %>%
  mutate(num_features = ifelse(condition == "CLEAR-2" | condition == "BB-2", "two", "eight"))

# fit the one factor model
lm_model <- lm(deviation ~ condition, data=q12_model_data)
summary(lm_model)

# look at one-way ANOVA
anova(lm_model)

# run 2-by-2 anova for deviation on q11
# fit the two factor model
lm_model <- lm(deviation ~ transparency*num_features, data=q12_model_data)
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

