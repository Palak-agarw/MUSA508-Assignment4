options(scipen=10000000)

library(tidyverse)
library(kableExtra)
library(caret)
library(knitr) 
library(pscl)
library(plotROC)
library(pROC)
library(lubridate)
library(ggcorrplot)
library(gridExtra)

palette5 <- c("#981FAC","#CB0F8B","#FF006A","#FE4C35","#FE9900")
palette4 <- c("#981FAC","#FF006A","#FE4C35","#FE9900")
palette2 <- c("#981FAC","#FF006A")

house_subsidy <- read.csv("DATA/housingSubsidy.csv")

## Data wrangling
### Employment 
house_subsidy <-
  house_subsidy %>%
  mutate(Employment = ifelse(house_subsidy$job == "blue-collar"|house_subsidy$job == "services"|house_subsidy$job =="admin."|house_subsidy$job =="entrepreneur"|
                               house_subsidy$job =="self-employed"|house_subsidy$job =="technician"|house_subsidy$job =="management"|
                               house_subsidy$job =="housemaid",1,0))

### Age
house_subsidy <-
  house_subsidy %>%
  mutate(Age_group = case_when(
    age < 18 ~ "Less than 18",
    age >= 18 & age < 25  ~ "18-25",
    age >= 25 & age < 35  ~ "25-35",
    age >= 35 & age <= 50  ~ "35-50",
    age >= 50 & age < 65   ~ "50-65",
    age >= 65   ~ "Above 65"))

### Education
house_subsidy <-
  house_subsidy %>%
  mutate(Education_group = case_when(
    education == "basic.9y" |education == "basic.6y" | education == "basic.4y" ~ "Basics",
    education == "high.school"  ~ "High School",
    education == "university.degree" |education == "professional.course"  ~ "College",
    education == "unknown" |education == "illiterate"  ~ "Illiterate"))

### Seasons
house_subsidy <-
  house_subsidy %>%
  mutate(Season = case_when(
    month == "dec" |month == "jan" | month == "feb" ~ "Winter",
    month == "mar" |month == "apr" | month == "may" ~ "Spring",
    month == "jun" |month == "jul" | month == "aug" ~ "Summer",
    month == "sep" |month == "oct" | month == "nov" ~ "Fall"))

## Day
house_subsidy <-
  house_subsidy %>%
  mutate(Day = case_when(
    day_of_week == "mon" |day_of_week == "fri" ~ "Busy",
    day_of_week == "tue" |day_of_week == "wed" | day_of_week == "thu" ~ "Non-busy"))

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

### Correlations

numericVars1 <- 
  select_if(house_subsidy, is.numeric) %>% na.omit() %>%
  dplyr::select(age, unemploy_rate, cons.price.idx, cons.conf.idx, inflation_rate, spent_on_repairs,y_numeric)

ggcorrplot(
  round(cor(numericVars1), 1), 
  p.mat = cor_pmat(numericVars1),
  colors = c("#25CB10", "white", "#FA7800"),
  type="lower",
  insig = "blank") +  
  labs(title = "Correlation across Internal Characteristics", caption="Figure 1.1") 

Vars2 <- 
  house_subsidy %>% na.omit() %>%
  dplyr::select(job,marital,education,y_numeric)

ggcorrplot(
  round(cor(Vars2), 1), 
  p.mat = cor_pmat(Vars2),
  colors = c("#25CB10", "white", "#FA7800"),
  type="lower",
  insig = "blank") +  
  labs(title = "Correlation across Internal Characteristics", caption="Figure 1.1") 

##
set.seed(3456)
trainIndex <- createDataPartition(house_subsidy$y, p = .65, 
                                  y = paste(house_subsidy$Education_group,house_subsidy$Age_group,house_subsidy$Season,
                                            house_subsidy$Employment,house_subsidy$taxLien, house_subsidy$Day),
                                  list = FALSE,
                                  times = 1)
housingTrain <- house_subsidy[ trainIndex,]
housingTest  <- house_subsidy[-trainIndex,]

## Regression

housingModel1 <- glm(y_numeric ~ .,
                     data=housingTrain %>% 
                       dplyr::select(-X, -contact,-month,-day_of_week,-pdays,-previous,-poutcome,-y,-Age_group, 
                                     -Education_group, -inflation_rate),
                     family="binomial" (link="logit"))
summary(housingModel1)

housingModel2 <- glm(y_numeric ~ .,
                     data=housingTrain %>% 
                       dplyr::select(-X, -contact,-month,-day_of_week,-pdays,-previous,-poutcome,-y,-age, 
                                     -education),
                     family="binomial" (link="logit"))

summary(housingModel2)

housingModel3 <- glm(y_numeric ~ .,
                     data=housingTrain %>% 
                       dplyr::select(-X, -contact,-month,-day_of_week,-pdays,-previous,-poutcome,-y,-age, 
                                     -education, -job, marital),
                     family="binomial" (link="logit"))

summary(housingModel3)


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

cvFit1 <- train(y ~ .,
                data=house_subsidy %>% 
                  dplyr::select(-y_numeric, -X, -contact,-month,-day_of_week,-pdays,-previous,-poutcome,-Age_group, 
                                -Education_group, -inflation_rate) %>%
                  dplyr::mutate(y = ifelse(y=="yes","c1.yes","c2.no")), 
                method="glm", family="binomial",
                metric="ROC", trControl = ctrl)


cvFit1  ## .115

cvFit2 <- train(y ~ .,
                data=house_subsidy %>% 
                  dplyr::select(-y_numeric,-X, -contact,-month,-day_of_week,-pdays,-previous,-poutcome,-age, 
                                -education) %>%
                  dplyr::mutate(y = ifelse(y=="yes","c1.yes","c2.no")), 
                method="glm", family="binomial",
                metric="ROC", trControl = ctrl)


cvFit2  ##.122

cvFit3 <- train(y ~ .,
                data=house_subsidy %>% 
                  dplyr::select(-y_numeric,-X, -contact,-month,-day_of_week,-pdays,-previous,-poutcome,-age, 
                                -education, -job, marital) %>%
                  dplyr::mutate(y = ifelse(y=="yes","c1.yes","c2.no")), 
                method="glm", family="binomial",
                metric="ROC", trControl = ctrl)


cvFit3  ##.138

cvFit4 <- train(y ~ .,
                data=house_subsidy %>% 
                  dplyr::select(-y_numeric,-X, -contact,-month,-day_of_week,-pdays,-previous,-poutcome,-age, 
                                -education, -job, marital,-taxLien, -mortgage, -Employment) %>%
                  dplyr::mutate(y = ifelse(y=="yes","c1.yes","c2.no")), 
                method="glm", family="binomial",
                metric="ROC", trControl = ctrl)


cvFit4 ##.129

cvFit5 <- train(y ~ .,
                data=house_subsidy %>% 
                  dplyr::select(-y_numeric,-X, -contact,-month,-day_of_week,-pdays,-previous,-poutcome,-age, 
                                -education, -job,-taxLien, -mortgage, -Employment) %>%
                  dplyr::mutate(y = ifelse(y=="yes","c1.yes","c2.no")), 
                method="glm", family="binomial",
                metric="ROC", trControl = ctrl)


cvFit5 ##.127

cvFit6 <- train(y ~ .,
                data=house_subsidy %>% 
                  dplyr::select(-y_numeric,-X, -contact,-month,-day_of_week,-pdays,-previous,-poutcome,-age, 
                                -education, -job,-taxLien, -mortgage, -Employment, -Day) %>%
                  dplyr::mutate(y = ifelse(y=="yes","c1.yes","c2.no")), 
                method="glm", family="binomial",
                metric="ROC", trControl = ctrl)


cvFit6 ##.1325

cvFit7 <- train(y ~ .,
                data=house_subsidy %>% 
                  dplyr::select(-y_numeric,-X, -contact,-month,-day_of_week,-pdays,-previous,-poutcome,-age, 
                                -education, -job,-taxLien, -mortgage, -Employment, -Day, -Education_group) %>%
                  dplyr::mutate(y = ifelse(y=="yes","c1.yes","c2.no")), 
                method="glm", family="binomial",
                metric="ROC", trControl = ctrl)


cvFit7 ##.1365

cvFit8 <- train(y ~ .,
                data=house_subsidy %>% 
                  dplyr::select(-y_numeric,-X, -contact,-month,-day_of_week,-previous,-poutcome,-age, 
                                -education, -job,-taxLien, -mortgage, -Employment, -Day, -Education_group) %>%
                  dplyr::mutate(y = ifelse(y=="yes","c1.yes","c2.no")), 
                method="glm", family="binomial",
                metric="ROC", trControl = ctrl)


cvFit8 ##.2175

cvFit9 <- train(y ~ .,
                data=house_subsidy %>% 
                  dplyr::select(-y_numeric,-X, -contact,-month,-day_of_week,-poutcome,-age, 
                                -education, -job,-taxLien, -mortgage, -Employment, -Day, -Education_group) %>%
                  dplyr::mutate(y = ifelse(y=="yes","c1.yes","c2.no")), 
                method="glm", family="binomial",
                metric="ROC", trControl = ctrl)


cvFit9 ##.227

cvFit10 <- train(y ~ .,
                data=house_subsidy %>% 
                  dplyr::select(-y_numeric,-X, -contact,-month,-day_of_week,-poutcome,-age, 
                                -education, -job,-taxLien, -mortgage, -Employment, -Day, -y_numeric) %>%
                  dplyr::mutate(y = ifelse(y=="yes","c1.yes","c2.no")), 
                method="glm", family="binomial",
                metric="ROC", trControl = ctrl)


cvFit10 ##.2205

cvFit11 <- train(y ~ .,
                 data=house_subsidy %>% 
                   dplyr::select(-y_numeric,-X, -contact,-month,-day_of_week,-poutcome,-Age_group, 
                                 -education, -job,-taxLien, -Employment, -Day, -y_numeric) %>%
                   dplyr::mutate(y = ifelse(y=="yes","c1.yes","c2.no")), 
                 method="glm", family="binomial",
                 metric="ROC", trControl = ctrl)


cvFit11 ##.2225

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
                Variable == "True_Positive" ~ ((Count*.25*10000) + (Count*.25*56000)-(Count*2850)-(Count*.25*5000)),
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
