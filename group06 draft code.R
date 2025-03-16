library(tidyverse)
library(sjPlot)
library(jtools)
library(knitr)
library(janitor)
library(stats)
library(GGally)
library(car)

dataset <- read.csv("dataset06.csv")

dataset <- dataset %>%
  drop_na()
#binary: rating > 7 is set to 1, rating <= 7 is set to 0)
dataset$rating_new <- ifelse(dataset$rating > 7, 1, 0)
#Convert categorical variables to factor variables
dataset$rating_new <- as.factor(dataset$rating_new)
dataset$genre <- as.factor(dataset$genre)

str(dataset)

##############Exploratory Data Analysis##############
ggplot(data = dataset, aes(x = rating_new, y = year, fill = rating_new)) +
  geom_boxplot() +
  labs(x = "rating_new", y = "year")+ 
  theme(legend.position = "none")

# numerical explanatory variable
ggplot(data = dataset, aes(x = rating_new, y = length, fill = rating_new)) +
  geom_boxplot() +
  labs(x = "rating_new", y = "length")+ 
  theme(legend.position = "none")

ggplot(data = dataset, aes(x = rating_new, y = budget, fill = rating_new)) +
  geom_boxplot() +
  labs(x = "rating_new", y = "budget")+ 
  theme(legend.position = "none")

ggplot(data = dataset, aes(x = rating_new, y = votes, fill = rating_new)) +
  geom_boxplot() +
  labs(x = "rating_new", y = "votes")+ 
  theme(legend.position = "none")

#log(votes)
ggplot(data = dataset, aes(x = rating_new, y = log(votes + 1), fill = rating_new)) +
  geom_boxplot() +
  labs(x = "rating_new", 
       y = "log(votes + 1)", 
       title = "Log-Transformed Votes vs Rating Category") + 
  theme(legend.position = "none")

# categorical explanatory variable
dataset %>% 
  tabyl(genre, rating_new) %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting() %>% 
  adorn_ns() # To show original counts

ggplot(data = dataset, aes(x = rating_new, group = genre)) +
  geom_bar(aes(y = ..prop.., fill = genre), stat = "count", position = "dodge") +
  labs(x = "Rating", y = "Proportion")
#boxplot-genre
ggplot(dataset, aes(x = genre, y = rating, fill = genre)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###############formal analysis ###############
###model selection
levels(dataset$rating_new) #baseline is 0(rating lower than 7))

#model1-full model: rating_new ~ year + length + budget + votes + genre
model1 <- glm(rating_new ~ year + length + budget + votes + genre, 
                  data = dataset, 
                  family = binomial(link = "logit"))
model1 %>%
  summary()

vif(model1)

summ(model1)
confint(model1) %>%
  kable()
#model2-stepwise: rating_new ~ year + length + budget + genre
model2 <- step(model1, direction = "both", trace = TRUE)
summary(model2)

vif(model2)

summ(model2)
confint(model2) %>%
  kable()

# Log-odds
plot_model(model1, show.values = TRUE, transform = NULL,
           title = "Log-Odds(higher than 7)", show.p = FALSE)
dataset <- dataset %>%
  mutate(logodds.higherthan7 = predict(model2))

# Odds
model2 %>%
  coef() %>%
  exp()
plot_model(model2, show.values = TRUE, axis.lim = c(0.1,200),
           title = "Odds (higher than 7)", show.p = FALSE)
dataset <- dataset %>%
  mutate(odds.higherthan7 = exp(logodds.higherthan7))

#Probabilities
dataset <- dataset %>%
  mutate(probs.higherthan7 = fitted(model2))

plot_model(model2, type = "pred", terms = "genre", title = "",
           axis.title = c("genre", "Prob. of rating higher than 7"))

ggplot(data = dataset, aes(x = year, y = probs.higherthan7)) +
  geom_smooth(method="glm", 
              method.args = list(family="binomial"), 
              se = FALSE) +
  labs(x = "year", y = "Probability of rating higher than 7")

ggplot(data = dataset, aes(x = length, y = probs.higherthan7)) +
  geom_smooth(method="glm", 
              method.args = list(family="binomial"), 
              se = FALSE) +
  labs(x = "length", y = "Probability of rating higher than 7")

ggplot(data = dataset, aes(x = budget, y = probs.higherthan7)) +
  geom_smooth(method="glm", 
              method.args = list(family="binomial"), 
              se = FALSE) +
  labs(x = "budget", y = "Probability of rating higher than 7")


library(pROC)
roc1 <- roc(dataset$rating_new, fitted(model1))
roc2 <- roc(dataset$rating_new, fitted(model2))
roc3 <- roc(dataset$rating_new, fitted(model3))

auc1 <- auc(roc1)
auc2 <- auc(roc2)
auc3 <- auc(roc3)

cat("AUC for model1:", auc1, "\n")
cat("AUC for model2:", auc2, "\n")
cat("AUC for model3:", auc3, "\n")

plot(roc1, col = "blue", main = "ROC Curves for 3 models")
plot(roc2, col = "red", add = TRUE)
plot(roc3, col = "black", add = TRUE)
legend("bottomright", legend = c("Model1", "Model2", "Model3"), col = c("blue", "red", "black"), lwd = 2)

#model4-interaction without votes：rating_new ~ year + length + budget * genre
#we can try this in further work
model4 <- glm(rating_new ~ year + length + budget * genre, 
              data = dataset, 
              family = binomial(link = "logit"))
summary(model6)
vif(model6)

summ(model6)
confint(model6) %>%
  kable()


