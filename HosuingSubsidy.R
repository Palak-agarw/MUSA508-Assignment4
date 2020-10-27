options(scipen=10000000)

library(tidyverse)
library(kableExtra)
library(caret)
library(knitr) 
library(pscl)
library(plotROC)
library(pROC)
library(lubridate)
# Leah - added this library
library(gridExtra)

palette5 <- c("#981FAC","#CB0F8B","#FF006A","#FE4C35","#FE9900")
palette4 <- c("#981FAC","#FF006A","#FE4C35","#FE9900")
palette2 <- c("#981FAC","#FF006A")

house_subsidy <- read.csv("DATA/housingSubsidy.csv")

##continuous variables
### Leah - added campaign,pdays, previous, cons.price.idx, cons.conf.idx
### Leah - unemployment rate plot is weird
### Leah - Changed fun.y to fun
house_subsidy %>%
  dplyr::select(y,unemploy_rate, spent_on_repairs, age, campaign, pdays, 
                previous,cons.price.idx,cons.conf.idx) %>%
  gather(Variable, value, -y) %>%
  ggplot(aes(y, value, fill=y)) + 
  geom_bar(position = "dodge", stat = "summary", fun = "mean") + 
  facet_wrap(~Variable, scales = "free") +
  scale_fill_manual(values = palette2) +
  labs(x="y", y="Value", 
       title = "Feature associations with the likelihood of taking credit",
       subtitle = "(continous outcomes)") +
  theme(legend.position = "none")

## categorical variables
#grid.arrange(ncol=5,
house_subsidy %>%
  dplyr::select(y, education) %>%
  gather(Variable, value, -y) %>%
  count(Variable, value, y) %>%
  ggplot(aes(value, n, fill = y)) +   
  geom_bar(position = "dodge", stat="identity") +
  facet_wrap(~Variable, scales="free") +
  scale_fill_manual(values = palette2) +
  labs(x="Take Credit", y="Count",
       title = "Education association with likelihood of taking credit")
     

## Jobs
house_subsidy %>%
  dplyr::select(y, job) %>%
  gather(Variable, value, -y) %>%
  count(Variable, value, y) %>%
  ggplot(aes(value, n, fill = y)) +   
  geom_bar(position = "dodge", stat="identity") +
  facet_wrap(~Variable, scales="free") +
  scale_fill_manual(values = palette2) +
  labs(x="Take Credit", y="Count",
       title = "Job type association with the likelihood of taking credit")

## Marital
house_subsidy %>%
  dplyr::select(y, marital) %>%
  gather(Variable, value, -y) %>%
  count(Variable, value, y) %>%
  ggplot(aes(value, n, fill = y)) +   
  geom_bar(position = "dodge", stat="identity") +
  facet_wrap(~Variable, scales="free") +
  scale_fill_manual(values = palette2) +
  labs(x="Take Credit", y="Count",
       title = "Marital status association with the likelihood of taking credit")

## taxLien
house_subsidy %>%
  dplyr::select(y, taxLien) %>%
  gather(Variable, value, -y) %>%
  count(Variable, value, y) %>%
  ggplot(aes(value, n, fill = y)) +   
  geom_bar(position = "dodge", stat="identity") +
  facet_wrap(~Variable, scales="free") +
  scale_fill_manual(values = palette2) +
  labs(x="Take Credit", y="Count",
       title = "Tax Lien association with the likelihood of taking credit")

house_subsidy %>%
  dplyr::select(y, mortgage) %>%
  gather(Variable, value, -y) %>%
  count(Variable, value, y) %>%
  ggplot(aes(value, n, fill = y)) +   
  geom_bar(position = "dodge", stat="identity") +
  facet_wrap(~Variable, scales="free") +
  scale_fill_manual(values = palette2) +
  labs(x="Take Credit", y="Count",
       title = "Mortgag association with the likelihood of taking credit")

house_subsidy %>%
  dplyr::select(y, taxbill_in_phl) %>%
  gather(Variable, value, -y) %>%
  count(Variable, value, y) %>%
  ggplot(aes(value, n, fill = y)) +   
  geom_bar(position = "dodge", stat="identity") +
  facet_wrap(~Variable, scales="free") +
  scale_fill_manual(values = palette2) +
  labs(x="Take Credit", y="Count",
       title = "Full-time residence in Philadelphia association with the likelihood of taking credit")

house_subsidy %>%
  dplyr::select(y, contact) %>%
  gather(Variable, value, -y) %>%
  count(Variable, value, y) %>%
  ggplot(aes(value, n, fill = y)) +   
  geom_bar(position = "dodge", stat="identity") +
  facet_wrap(~Variable, scales="free") +
  scale_fill_manual(values = palette2) +
  labs(x="Take Credit", y="Count",
       title = "Previous contact association with the likelihood of taking credit")

# Change order of months
house_subsidy %>%
  dplyr::select(y, month) %>%
  gather(Variable, value, -y) %>%
  count(Variable, value, y) %>%
  ggplot(aes(value, n, fill = y)) +   
  geom_bar(position = "dodge", stat="identity") +
  facet_wrap(~Variable, scales="free") +
  scale_fill_manual(values = palette2) +
  labs(x="Take Credit", y="Count",
       title = "Month contacted association with the likelihood of taking credit")

# Change order of days
house_subsidy %>%
  dplyr::select(y, day_of_week) %>%
  gather(Variable, value, -y) %>%
  count(Variable, value, y) %>%
  ggplot(aes(value, n, fill = y)) +   
  geom_bar(position = "dodge", stat="identity") +
  facet_wrap(~Variable, scales="free") +
  scale_fill_manual(values = palette2) +
  labs(x="Take Credit", y="Count",
       title = "Day contacted association with the likelihood of taking credit")

house_subsidy %>%
  dplyr::select(y, poutcome) %>%
  gather(Variable, value, -y) %>%
  count(Variable, value, y) %>%
  ggplot(aes(value, n, fill = y)) +   
  geom_bar(position = "dodge", stat="identity") +
  facet_wrap(~Variable, scales="free") +
  scale_fill_manual(values = palette2) +
  labs(x="Take Credit", y="Count",
       title = "Outcome of previous campaign association with the likelihood of taking credit")
#)


##
set.seed(3456)
trainIndex <- createDataPartition(house_subsidy$y, p = .50, y = paste(house_subsidy$education),
                                  list = FALSE,
                                  times = 1)
housingTrain <- house_subsidy[ trainIndex,]
housingTest  <- house_subsidy[-trainIndex,]

## Regression
housingModel <- glm(y_numeric ~ .,
                        data=housingTrain %>% 
                          dplyr::select(-contact, -month, -y, -day_of_week,-campaign,-pdays,-previous,-poutcome),
                        family="binomial" (link="logit"))

summary(housingModel)

## Adding Coefficients
x <- housingModel$coefficients
exp(x)


## Fit metrics
pR2(housingModel)

## Prediction
testProbs <- data.frame(Outcome = as.factor(housingTest$y_numeric),
                        Probs = predict(housingModel, housingTest, type= "response"))

ggplot(testProbs, aes(x = Probs, fill = as.factor(Outcome))) + 
  geom_density() +
  facet_grid(Outcome ~ .) +
  scale_fill_manual(values = palette2) +
  labs(x = "Click", y = "Density of probabilities",
       title = "Distribution of predicted probabilities by observed outcome") +
  theme(strip.text.x = element_text(size = 18),
        legend.position = "none")

## Confusion matrix
testProbs <- 
  testProbs %>%
  mutate(predOutcome  = as.factor(ifelse(testProbs$Probs > 0.5 , 1, 0)))

caret::confusionMatrix(testProbs$predOutcome, testProbs$Outcome, 
                       positive = "1")

## ROC curve
auc(testProbs$Outcome, testProbs$Probs)

ggplot(testProbs, aes(d = as.numeric(testProbs$Outcome), m = Probs)) +
  geom_roc(n.cuts = 50, labels = FALSE, colour = "#FE9900") +
  style_roc(theme = theme_grey) +
  geom_abline(slope = 1, intercept = 0, size = 1.5, color = 'grey') +
  labs(title = "ROC Curve - clickModel")

## Cross validation
ctrl <- trainControl(method = "cv", number = 100, classProbs=TRUE, summaryFunction=twoClassSummary)

cvFit <- train(y ~ .,
               data=house_subsidy %>% 
                 dplyr::select(-contact, -month, -y_numeric, -day_of_week,-campaign,-pdays,-previous,-poutcome), 
               method="glm", family="binomial",
               metric="ROC", trControl = ctrl)

cvFit

## Goodness metrics
dplyr::select(cvFit$resample, -Resample) %>%
  gather(metric, value) %>%
  left_join(gather(cvFit$results[2:4], metric, mean)) %>%
  ggplot(aes(value)) + 
  geom_histogram(bins=35, fill = "#FF006A") +
  facet_wrap(~metric) +
  geom_vline(aes(xintercept = mean), colour = "#981FAC", linetype = 3, size = 1.5) +
  scale_x_continuous(limits = c(0, 1)) +
  labs(x="Goodness of Fit", y="Count", title="CV Goodness of Fit Metrics",
       subtitle = "Across-fold mean reprented as dotted lines")

## Cost benefit
cost_benefit_table <-
  testProbs %>%
  count(predOutcome, Outcome) %>%
  summarize(True_Negative = sum(n[predOutcome==0 & Outcome==0]),
            True_Positive = sum(n[predOutcome==1 & Outcome==1]),
            False_Negative = sum(n[predOutcome==0 & Outcome==1]),
            False_Positive = sum(n[predOutcome==1 & Outcome==0])) %>%
  gather(Variable, Count) %>%
  mutate(Revenue =
           case_when(Variable == "True_Negative" ~ Count*0,
                Variable == "True_Positive" ~ ((Count*.25*10000)-(Count*2850)-(Count*.25*5000)),
                Variable == "False_Negative" ~ Count*0,
                Variable == "False_Positive" ~ -(Count*2850))) %>%
  mutate(Households_Helped=
           case_when(Variable == "True_Negative" ~ Count*0,
                     Variable == "True_Positive" ~ Count*.25,
                     Variable == "False_Negative" ~ Count*0,
                     Variable == "False_Positive" ~ Count*0)) %>%
  bind_cols(data.frame(Description = c(
    "We correctly predicted not taking credit",
    "We correctly predicted taking credit",
    "We predicted would not take credit and customer took credit",
    "We predicted customer would take credit and customer did not take credit")))

kable(cost_benefit_table,
      caption = "Cost/Benefit Table") %>% kable_styling()

## Optimise threshold
iterateThresholds <- function(data) {
  x = .01
  all_prediction <- data.frame()
  while (x <= 1) {
    
    this_prediction <-
      testProbs %>%
      mutate(predOutcome = ifelse(Probs > x, 1, 0)) %>%
      count(predOutcome, Outcome) %>%
      summarize(True_Negative = sum(n[predOutcome==0 & Outcome==0]),
                True_Positive = sum(n[predOutcome==1 & Outcome==1]),
                False_Negative = sum(n[predOutcome==0 & Outcome==1]),
                False_Positive = sum(n[predOutcome==1 & Outcome==0])) %>%
      gather(Variable, Count) %>%
      mutate(Revenue =
               ifelse(Variable == "True_Negative", Count * 0,
                      ifelse(Variable == "True_Positive",((.35 - .1) * Count),
                             ifelse(Variable == "False_Negative", (-0.35) * Count,
                                    ifelse(Variable == "False_Positive", (-0.1) * Count, 0)))),
             Threshold = x)
    
    all_prediction <- rbind(all_prediction, this_prediction)
    x <- x + .01
  }
  return(all_prediction)
}

whichThreshold <- iterateThresholds(testProbs2)

whichThreshold_revenue <- 
  whichThreshold %>% 
  group_by(Threshold) %>% 
  summarize(Revenue = sum(Revenue))

ggplot(whichThreshold_revenue)+
  geom_line(aes(x = Threshold, y = Revenue))+
  geom_vline(xintercept =  pull(arrange(whichThreshold_revenue, -Revenue)[1,1]))+
  labs(title = "Model Revenues By Threshold For Test Sample",
       subtitle = "Vertical Line Denotes Optimal Threshold")
