library("ggthemes")
library("tidyr")
library("dplyr")
library("ggplot2")
library("DescTools")
library("GGally")
library("stringr")
library("stats")
library("RColorBrewer")
library("wesanderson")
library("viridis")
library("plotly")
library("knitr")
library("readr")
library("kableExtra")
library("webshot")
library("nlme")
library("ez")

data <- read.csv("final_data.csv") %>%
  rename("score" = "ï..score") 

for(i in 1:nrow(data)){
  if(data$valence[i] == "postive"){
    data$valence[i] <- "positive"
  }
}

for(i in 1:nrow(data)){
  if(data$valence[i] == "positive" && data$concreteness[i] == "concrete"){
    data$category[i] <- "PC"
  }else if(data$valence[i] == "negative" && data$concreteness[i] == "concrete") {
   data$category[i] <- "NC" 
  }else if(data$valence[i] == "negative" && data$concreteness[i] == "abstract") {
    data$category[i] <- "NA" 
  }else {
    data$category[i] <- "PA" 
  }
}

# anova <- lm(score ~ valence * concreteness, data = data) %>%
#   anova() %>%
#   rename("p value" = "Pr(>F)") %>%
#   kbl() %>%
#   kable_paper(full_width = F)
# print(anova)

anova <- ezANOVA(data = data, dv = score, wid = subject, within = concreteness * valence) %>%
  kbl() %>%
  kable_paper(full_width = F)
print(anova)


demo <- read.csv("demo_and_raw_scores.csv")%>%
  rename("Subject" = "ï..Subject")

dist_plot <- ggplot(data, aes(x = category, y = score, col = category, fill = category)) +
  geom_boxplot(alpha = 0.25, position = position_dodge(width = 0.75)) +
  geom_point(size = 1.5, position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.75)) +
  theme_hc() +
  labs(x = "Word Category", y = "Total Recall", col = "Word Categories", title = "Total Recall for Each Word Category") +
  theme(axis.title.x = element_blank(),) +
  guides(fill = "none") 

ggsave("dist_plot.png", dist_plot)


data_means <- data %>% group_by(category) %>%
  summarize(mean_score = mean(score), category = unique(category), se = sd(score)/sqrt(nrow(.))) 

avg_bar_plot <- ggplot(data_means, aes(x = category, y = mean_score, fill = category)) +
  geom_bar(alpha = 0.5, stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_score - se, ymax = mean_score + se), 
                width = 0.1, position = position_dodge(0.9)) +
  labs(x = "Word Category", y = "Mean Recall", fill = "Word Categories", title = "Mean Recall for Each Word Category") +
  theme_hc() +
  theme(axis.title.x = element_blank())


ggsave("avg_barplot.png", avg_bar_plot)

data_sep <- read.csv("final_data.csv") %>%
  rename("score" = "ï..score") %>%
  group_by(valence, concreteness) %>% 
  summarize(mean_score = mean(score), valence = unique(valence), concreteness = unique(concreteness))


avg_line_plot <- ggplot(data_sep, aes(x = concreteness, y = mean_score, color = valence)) +
  geom_point() +
  geom_line(aes(group = valence)) +
  labs(x = "Concretness", y = "Mean Recall", color = "Valence", title = "Mean Recall for Each Word Category") +
  theme_hc() +
  ylim(8, 9)

ggsave("avg_line_plot.png", avg_line_plot)

means_table <- data_means %>%
  kbl() %>%
  kable_paper(full_width = T)
print(means_table)

  
  