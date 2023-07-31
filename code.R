library(tidyverse)
library(scales)
library(grid)
library(gridExtra)
library(ROSE)
library(caTools)
library(caret)
library(rpart)
library(rpart.plot)
library(vip)
library(randomForest)


df <- read_csv("D:/Internships/Technohack/Task - 3/data.csv")

View(df)
glimpse(df)

df %>% is.na() %>% sum()

summary(df)
# Some outliers are there in monthly income and total working years,
# years at company... 
# All the employee count is 1, unnecessary column.


# Attrition rate:
df %>% count(Attrition) %>% mutate('Perc' = percent(n/sum(n))) %>% 
  ggplot(aes(x = Attrition, y = n)) +
  geom_bar(stat = 'identity', position = position_dodge2(),
           width = 0.3, fill = 'yellow', colour = 'black') +
  theme_minimal() + theme(axis.title.y = element_blank()) +
  geom_text(aes(label = Perc), vjust = 2)
# Imbalanced data.


plot1 <- function(var){ # categorical plot
  my_col <- c('orange', 'darkgreen')
  df %>% count({{var}}, Attrition) %>% 
    ggplot(aes(x = {{var}}, y = n, fill = Attrition)) +
    geom_bar(stat = 'identity', position = position_dodge2(),
             width = 0.4) + labs(y = 'Count') +
    scale_fill_manual(values = my_col) +
    theme_minimal() + scale_y_continuous(n.breaks = 7) +
    theme(legend.position = 'None', 
          axis.text.x = element_text(angle = 90, vjust = 0.5,
                                     hjust = 1)) -> p1
  
  df %>% count({{var}}, Attrition) %>% 
    ggplot(aes(x = {{var}}, y = n, fill = Attrition)) +
    geom_col(position = 'fill', width = 0.4) +
    scale_y_continuous(labels = percent, n.breaks = 7) + 
    labs(y = 'Percentage') + theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5,
                                     hjust = 1)) +
    scale_fill_manual(values = my_col) -> p2
  
  # Chisq-test for association
  table(var, Attrition) %>% chisq.test() -> t
  round(t$p.value, 5) -> p
  
  grid.arrange(p1,p2, ncol = 2,
    top = textGrob(paste0('p-value : ', p), 
                   gp = gpar(fontsize = 14, font = 2)))
}


# Variables with unique values:
sapply(lapply(df, unique), length) -> v

data.frame('Variables' = names(df),
           'CountOfUniques' = array(v)) %>% 
  ggplot(aes(x = Variables, y = CountOfUniques)) + 
  geom_bar(stat = 'identity', position = position_dodge(),
           width = 0.2, fill = 'blue') + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5,
                                   hjust = 1)) +
  geom_text(aes(label = CountOfUniques), vjust = -0.2, size = 4)

vars_unq <- c('EmployeeCount','Over18','StandardHours')

df <- df %>% select(!(all_of(vars_unq)))


attach(df)
plot1(Gender)
plot1(Department)
plot1(BusinessTravel)
plot1(JobRole)
plot1(MaritalStatus)
plot1(OverTime)



plot2 <- function(var, test){
  my_col2 <- c('green','red')
  
  df %>% count({{var}}, Attrition) %>% 
    group_by({{var}}) %>% mutate(per = percent(n/sum(n))) %>% 
    ggplot(aes(x = {{var}}, y = n, colour = Attrition)) +
    geom_line(lwd = 1) + geom_text(aes(label = per), vjust = -0.5,
                                   colour = 'blue', size = 3) +
    geom_point(colour = 'black', size = 2) +
    scale_color_manual(values = my_col2) + theme_minimal() -> a
  
  # Chisq-test for association
  if(test == 'no'){
    a + labs(y = 'Count') %>% return()
  }else if(test == 'yes'){
    table(var, Attrition) %>% chisq.test() -> t
    round(t$p.value, 5) -> p
    
    a + labs(title = paste0('p-value : ', p),
             y = 'Count') %>% return()
  }
}

plot2(JobSatisfaction, test = 'yes')
plot2(EnvironmentSatisfaction, test = 'yes')
plot2(Education, test = 'yes')
plot2(RelationshipSatisfaction, test = 'yes')
plot2(WorkLifeBalance, test = 'yes')
plot2(YearsInCurrentRole, test = 'no')
plot2(YearsWithCurrManager, test = 'no')
plot2(TotalWorkingYears, test = 'no')
plot2(DistanceFromHome, test = 'no')
# plot2(YearsAtCompany, test = 'no')
# plot2(PercentSalaryHike, test = 'no')





plot3 <- function(var){
  my_col3 <- c('blue','yellow')
  df %>% ggplot(aes({{var}}, fill = Attrition)) +
    geom_density(alpha = 0.6, colour = NA) +
    scale_fill_manual(values = my_col3) +
    theme_minimal() -> p1
  
  df %>% ggplot(aes(x = Attrition, y = {{var}})) + 
    geom_boxplot(outlier.color = 'darkblue', outlier.size = 1,
                 outlier.shape = 4, outlier.stroke = 1.3) +
    geom_violin(alpha = 0.4, color = NA, fill = 'green') +
    coord_flip() + theme_minimal() + labs(x = '')  -> p2
  
  grid.arrange(p1,p2, ncol = 2)
}

plot3(MonthlyIncome)
plot3(Age)



# Overtime ~ Salary
df %>% group_by(OverTime, Attrition) %>% 
  summarise(MonthlyIncome = round(mean(MonthlyIncome))) -> avg_df

df %>% ggplot(aes(x = OverTime, y = MonthlyIncome, 
                  fill = Attrition)) + 
  geom_boxplot(outlier.colour = 'darkblue', outlier.shape = 4,
               outlier.stroke = 1.3, outlier.alpha = 0.5) +
   geom_text(data = avg_df, aes(label = MonthlyIncome),
             position = position_dodge(width = 0.8)) +
  theme_minimal() + scale_y_continuous(labels = 
            label_number(suffix = "K", scale = 1e-3),
            n.breaks = 10)
 
######################################################################
df_o <- ovun.sample(Attrition~., data = df,
                    p = 0.5, method = 'over', seed = 1)$data %>% 
  as_tibble()


f <- function(x)(paste(x[1],'X',x[2]))
glue::glue("Dimension of new data: {d}",
           d = dim(df_o) %>% f)

# New attrition rate:
df_o %>% count(Attrition) %>% 
  mutate(Percentage = percent(n/sum(n)))

df_o <- df_o %>% mutate(Attrition2 = case_when(
  Attrition == 'Yes' ~ 1,
  Attrition == 'No' ~ 0
)) %>% mutate(Attrition2 = as.factor(Attrition2))
#======================
# Decision tree:
# Splitting the data:
set.seed(42)
s <- sample.split(df$Attrition, SplitRatio = 3/4)
train_data <- df[s,]
test_data <- df[!s,]

# K-fold cross validation:
folds <- createMultiFolds(train_data$Attrition, k = 5, times = 5)
control <- trainControl(method = "repeatedcv", index = folds)
classifier_cv <- train(Attrition ~ ., 
                       data = train_data, method = "rpart", 
                       trControl = control)

# final model
classifier_cv$finalModel -> dtree_f 

par(mfrow = c(1,2))
rpart.plot(dtree_f, extra = 4)
prp(dtree_f)


# Function for performance metrics
stats2 <- function(C){
  t <- C$table
  
  acc <- C$overall[[1]]
  pre <- t[2,2]/(t[2,2]+t[2,1])
  rec <- t[2,2]/(t[2,2]+t[1,2])
  f1 <- 2*(rec*pre)/(rec+pre)
  
  matrix(c(acc,pre,rec,f1), byrow = T,
         dimnames = list(c('Accuracy','Precision',
                           'Recall','F1-Score'))) -> M
  return(list('Confusion Matrix' = t,
              'Metrics' = M))
}

# For training data:
attr_pred_train <- predict(classifier_cv, newdata = train_data)
confusionMatrix(as.factor(train_data$Attrition), 
                attr_pred_train) -> C1
stats2(C1)


# For testing data:
attr_pred_test <- predict(classifier_cv, newdata = test_data)
confusionMatrix(as.factor(test_data$Attrition), 
                attr_pred_test) -> C2
stats2(C2)


#========
df_o <- ovun.sample(Attrition~., data = df,
                    p = 0.5, method = 'over', seed = 1)$data %>% 
  as_tibble()


# dimension of new data
f <- function(x)(paste(x[1],'X',x[2]))
glue::glue("Dimension of new data: {d}",
           d = dim(df_o) %>% f)

df_o %>% count(Attrition) %>% 
  mutate(Percentage = percent(n/sum(n)))


# Splitting the data:
set.seed(42)
s <- sample.split(df_o$Attrition, SplitRatio = 3/4)
train_data2 <- df_o[s,]
test_data2 <- df_o[!s,]

# K-fold cross validation:
folds <- createMultiFolds(train_data2$Attrition, k = 5, times = 5)
control <- trainControl(method = "repeatedcv", index = folds)
classifier_cv <- train(Attrition ~ ., 
                       data = train_data2, method = "rpart", 
                       trControl = control)

# final model
classifier_cv$finalModel -> dtree_f2 

par(mfrow = c(1,2))
rpart.plot(dtree_f2, extra = 4)
prp(dtree_f2)


# For training data:
attr_pred_train2 <- predict(classifier_cv, newdata = train_data2)
confusionMatrix(as.factor(train_data2$Attrition), 
                attr_pred_train2) -> C1
stats2(C1)


# For testing data:
attr_pred_test2 <- predict(classifier_cv, newdata = test_data2)
confusionMatrix(as.factor(test_data2$Attrition), 
                attr_pred_test2) -> C2
stats2(C2)



