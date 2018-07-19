#Churn model

## Libraries and options
source("utils.R")

library (RCurl)
library(ggthemes)
library(plotly)
library(ggplot2)
library(tidyverse)
library(caret)
library(randomForest)
library(mlbench)
library(plotROC)
library(ROCR)
library(doParallel)
library(pROC) # for AUC calculations
library(corrplot)
library(Information)
library(partykit)
library(ROSE)
library(DMwR)
library(MLmetrics)
library(e1071)
library(glmnet)
options(scipen = 10000)

##Global parameters
na_threshold <- 10
missings_threshold <- 0.9
majority_threshold <- 0.95
cooks_d_threshold <- 6
outlier_threshold <- 1.5
corr_threshold <- 0.5
seed <- 42
ID_vars <- "customerID"
target <- "Churn"

## Load data
data_url <- "https://community.watsonanalytics.com/wp-content/uploads/2015/03/WA_Fn-UseC_-Telco-Customer-Churn.csv"
download <- getURL(data_url)
data <- read.csv (text = download)

##Treat missings
data <- data %>% mutate_all(funs(empty_as_na)) %>%
  mutate(na_sum = rowSums(is.na(.))) %>% 
  filter(na_sum < na_threshold) %>%
  select(-na_sum)

#check how is the data looking with respect to missing observations
missings <- data.frame(column = names(sapply(data, function(x) sum(is.na(x)))),
                       NAs = sapply(data, function(x) sum(is.na(x))),
                       row.names = NULL) %>%
  arrange(-desc(NAs))


#plot number of missing values per variable
missings_plot <- ggplot(data=missings, aes(x = reorder(column, -missings$NAs), y = NAs)) +
  geom_bar(stat="identity") +
  theme_tufte() +
  geom_hline(yintercept = nrow(data), color="red") +
  geom_text(aes(ncol(data)/2,
                0.9*nrow(data),
                label = "total number of observations"),
            size = 3,
            vjust = 1,
            color = "red") +
  theme(axis.text.x = element_text(size=8, angle=45, hjust = 1),
        axis.ticks.x = element_blank()) +
  xlab("Variables") +
  ylab("# of missing observations") +
  ggtitle("Number of missing observations in the data set")
ggplotly(missings_plot)

#save plot
ExportPlot(missings_plot, "figures/missings")

too_many_missings <- missings %>%
  mutate(perc_missing = NAs/nrow(data)) %>%
  filter(perc_missing > missings_threshold) %>%
  select(column) %>%
  pull() %>%
  as.character()

data <- data %>%
  select(-one_of(c(too_many_missings))) %>%
  tidyr::replace_na(list(
    TotalCharges = round(mean(data$TotalCharges, na.rm = TRUE),1)
    )
    )

# Treat categorical variables

to_recode <- c("OnlineSecurity", 
               "OnlineBackup", 
               "DeviceProtection", 
               "TechSupport", 
               "StreamingTV", 
               "StreamingMovies")

data <- data %>% 
  mutate_at(to_recode, funs(recode(., "No internet service"="No")))%>%
  mutate_if(is.character,as.factor)

categoricalVars <- setdiff(names(sapply(data, is.factor))[sapply(data, is.factor) == TRUE], c(ID_vars, target))

# Remove poor levels from discrete variables
pl <- poor_levels(data[, categoricalVars])

#majority categories plot
#get majority classes and their percentages for the whole table
majorities <- sapply(select(data, -ID_vars),
                     function(x) majority_percent(x), simplify = TRUE)

#restructure the results a bit
majorities_df <- data.frame(variable = names(as.data.frame(majorities)),
                            majority_class = majorities[1,],
                            percentage = as.numeric(majorities[2,]),
                            row.names = NULL)

#plot number of missing values per variable
majorities_plot <- ggplot(data=majorities_df,
                          aes(x = reorder(variable, -majorities_df$percentage),
                              y = percentage,
                              label = majority_class)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept = 1, color="red") +
  theme_tufte() +
  theme(axis.text.x = element_text(size=8, angle=45, hjust = 1),
        axis.ticks.x = element_blank()) +
  xlab("Variable") +
  ylab("% in majority category") +
  ggtitle("Majority categories")
ggplotly(majorities_plot)

ExportPlot(majorities_plot, "figures/majorities")

to_exclude <- majorities_df %>%
  filter(percentage > majority_threshold) %>%
  select(variable) %>%
  distinct() %>%
  pull() %>%
  as.character() %>%
  c(.)

data <- data %>%
  select(-one_of(to_exclude))

continuousVars <- setdiff(names(sapply(data, is.numeric))[sapply(data, is.numeric) == TRUE], c(ID_vars, target))

## treat outliers

# univariate approach
# cap outliers that are above 1.5*IQR with 5% and 95% respective values and transform variables to z-scores
data <- data %>%
  mutate_at(continuousVars, cap_outliers, threshold = outlier_threshold)

# multivariate approach

# Find influential observations based on cook's distance
mod <- glm(as.formula(paste(target, "~", paste(continuousVars, collapse = "+"))), 
           data = data, 
           family=binomial(link="logit"))
cooksd <- cooks.distance(mod)

plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = cooks_d_threshold*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>cooks_d_threshold*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

# Remove those influential observations from the data
influential_obs <- as.numeric(names(cooksd)[(cooksd > cooks_d_threshold*mean(cooksd, na.rm=T))])  # influential row numbers
data <- data %>% .[-influential_obs,]

## check pairwise correlations
corrs <- data %>%
  filter(complete.cases(.)) %>%
  select(continuousVars) %>%
  mutate_all(as.numeric) %>%
  cor() %>%
  round(2)

corrplot(corrs,
         method = "circle",
         order = "hclust",
         sig.level = 0.01)

corrs_df <- corrs %>%
  as.table() %>%
  as.data.frame() %>%
  filter(Freq > corr_threshold  & Freq < 1) %>%
  arrange(desc(Freq)) %>%
  mutate(odd = rep_len(c(0,1), nrow(.))) %>%
  filter(odd == 1) %>%
  select(-odd) %>%
  mutate_if(is.factor, as.character)

levels(data$Churn) <- c(0,1)
data$Churn <- as.numeric(as.character(data$Churn))
infoTables <- create_infotables(data = data,
                                y = target,
                                bins = 10,
                                parallel = T)

#plot IV
IV_df <- infoTables$Summary[order(-infoTables$Summary$IV), ]
IV_df$Variable <- factor(IV_df$Variable,
                         levels = IV_df$Variable[order(-IV_df$IV)])

IV <- ggplot(IV_df, aes(y = reorder(Variable, IV), x = IV)) +
  geom_point(col = "tomato2", size = 4) + # Draw points
  theme_hc() +
  geom_segment(aes(y = Variable,
                   yend = Variable,
                   x = min(IV),
                   xend = max(IV)),
               linetype = "dashed",
               size = 0.1) + # Draw dashed lines
  labs(title = "Information Value",
       x = "IV",
       y = "Variable")
ggplotly(IV)

ExportPlot(IV, "figures/IV")

#exclude variables that have higher correlation than 0.7
#when excluding take the variable with lower IV
# make sure label column is not excluded
to_drop <- unique(apply(corrs_df, 1, drop_vars, IV_df))
data <- data %>%
  select(-one_of(to_drop)) %>%
  mutate_if(is.character,as.factor)

## discretize some variables
min(data$tenure)
max(data$tenure)

data$tenure <- case_when(
  data$tenure >= 0 & data$tenure <= 12 ~ "0-12 Month",
  data$tenure > 12 & data$tenure <= 24 ~ "12-24 Month",
  data$tenure > 24 & data$tenure <= 36 ~ "24-36 Month",
  data$tenure > 36 & data$tenure <= 48 ~ "36-48 Month",
  data$tenure > 48 & data$tenure <= 60 ~ "48-60 Month",
  data$tenure > 60 ~ "> 60 Month"
)

data$Churn <- as.factor(data$Churn)
levels(data$Churn) <- c("No","Yes")
data <- data %>% 
  select(-ID_vars)

## modeling
set.seed(seed)
in_train <- createDataPartition(data[[target]], p = 0.8, list=FALSE)
training <- data[in_train,] %>%
  filter(complete.cases(.))

testing <- data[-in_train,]  %>%
  filter(complete.cases(.))

# Set up model parameters############################################
model_form <- as.formula(paste(target," ~ ."))

#create model weights (they sum to one)
model_weights <- ifelse(training[[target]] == "No",
                         (1/table(training[[target]])[1]) * 0.5,
                         (1/table(training[[target]])[2]) * 0.5)

#control list
trainControl <- trainControl(method="repeatedcv",
                             number=5,
                             repeats=2,
                             search="random",
                             classProbs = TRUE,
                             savePredictions = TRUE,
                             summaryFunction = multiClassSummary,
                             allowParallel = TRUE,
                             verboseIter = TRUE)

# Random forest model#################################################
set.seed(seed)
gc()
#run model in parallel
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)
# Grow a forest
start.time <- Sys.time()
fit_rf <- train(model_form,
                training,
                method ='rf',
                ntree = 50,
                trControl = trainControl,
                na.action = na.omit,
                weights = model_weights,
                metric = "ROC",
                tuneLength = 4,
                verbose = TRUE)
end.time <- Sys.time()
time.taken <- end.time - start.time
cat('\n', paste("training of random forest model for", target,"took:", round(time.taken, 1), units(time.taken)))
print(fit_rf)
stopCluster(cluster)
registerDoSEQ()

# Variable importance#######################################################
varImp <- importance(fit_rf$finalModel, type = 2)
varImp <- data.frame(var = rownames(varImp), imp = varImp[, 1])
varImp$imp <- as.numeric(varImp$imp)
varImp$var <- factor(varImp$var, levels = varImp$var)
varImp <- arrange(varImp, desc(imp)) %>% top_n(40)
#head(varImp, 4)
varImp$imp <- as.numeric(varImp$imp)
# Plot of variable importance
VI <- ggplot(varImp, aes(y = reorder(var, imp), x = imp)) +
  geom_point(col = "tomato2", size = 4) + # Draw points
  theme_hc() +
  geom_segment(aes(y = var,
                   yend = var,
                   x = min(imp),
                   xend = max(imp)),
               linetype = "dashed",
               size = 0.1) + # Draw dashed lines
  labs(title = "Variable importance",
       x = "Mean decrese in Ginni",
       y = "Variable")
ggplotly(VI)

ExportPlot(VI, "figures/VI")


# ROC curve and AUC for random forrest model####################################
#predicting the test data
rf_probs <- predict(fit_rf, testing, type="prob")$Yes
rf_class <- predict(fit_rf, testing)
rf_labels <- testing[[target]]

#roc analysis for test data
rf_prediction <- prediction(rf_probs, rf_labels)
rf_performance <- performance(rf_prediction, "tpr", "fpr")
rf_auc <- performance(rf_prediction, "auc")@y.values[[1]]

rf_roc_data <- data.frame(fpr = unlist(rf_performance@x.values),
                          tpr = unlist(rf_performance@y.values),
                          model = "random forest")

rf_roc <- ggplot(rf_roc_data, aes(x=fpr,
                                  ymin=0,
                                  ymax=tpr,
                                  group = model,
                                  fill = model,
                                  color = model)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr), lwd = 1) +
  ggtitle(paste0("ROC Curve, AUC = ", round(rf_auc, 2))) +
  theme_hc() +
  geom_abline(intercept = 0, slope = 1, size = 1, color = "black", lty = "dashed") +
  labs(x = "1-Specificity", y = "Sensitivity")
ggplotly(rf_roc)

ExportPlot(rf_roc, "figures/rf_ROC")


# Logistic regression##########################################################
set.seed(seed)
gc()

#run model in parallel
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)
start.time <- Sys.time()
fit_lr <- train(model_form,
                training,
                method = "glmnet",
                tuneLength = 10,
                trControl = trainControl,
                na.action = na.omit,
                weights = model_weights,
                metric = "ROC",
                standardize = FALSE,
                maxit = 10000)
end.time <- Sys.time()
time.taken <- end.time - start.time
cat('\n', paste("logistic regression model training took:", round(time.taken, 1), units(time.taken)))
print(fit_lr)
stopCluster(cluster)
registerDoSEQ()

lr_final_model <- fit_lr$finalModel
coef(lr_final_model, s = fit_lr$bestTune$lambda)

# Roc curve and AUC for logistic regression############################################
#predicting the test data
lr_probs <- predict(fit_lr, testing, type="prob")$Yes
lr_class <- predict(fit_lr, testing)
lr_labels <- testing[[target]]

#roc analysis for test data
lr_prediction <- prediction(lr_probs, lr_labels)
lr_performance <- performance(lr_prediction, "tpr", "fpr")
lr_auc <- performance(lr_prediction, "auc")@y.values[[1]]

lr_roc_data <- data.frame(fpr = unlist(lr_performance@x.values),
                          tpr = unlist(lr_performance@y.values),
                          model = "logistic regression")

lr_roc <- ggplot(lr_roc_data, aes(x=fpr,
                                       ymin=0,
                                       ymax=tpr,
                                       group = model,
                                       fill = model,
                                       color = model)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr), lwd = 1) +
  ggtitle(paste0("ROC Curve, AUC = ", round(lr_auc, 2))) +
  theme_hc() +
  geom_abline(intercept = 0, slope = 1, size = 1, color = "black", lty = "dashed") +
  labs(x = "1-Specificity", y = "Sensitivity")
ggplotly(lr_roc)

ExportPlot(lr_roc, "figures/lr_ROC")

conf_mat_lr <- confusionMatrix(lr_class, lr_labels)

lbls <- c("True\nNegative", "False\nPositive", "False\nNegative", "True\nPositive")

confmat <- as.data.frame(round(100*conf_mat_lr$table/sum(conf_mat_lr$table),1))
cm_plot_lr <- ggplot(data =  confmat, mapping = aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  geom_text(aes(label = paste(lbls, "\n", Freq, "%")), vjust = 1) +
  scale_fill_gradient(low = "white", high = "orange") +
  theme_pander(14) +
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        panel.grid.major = element_blank()) +
  scale_y_discrete(limits = rev(levels(as.factor(confmat$Prediction))), labels = c("Yes", "No")) +
  scale_x_discrete(#position = "top",
    labels = c("No", "Yes"))
ggplotly(cm_plot_lr)

ExportPlot(cm_plot_lr, "figures/lr_conf_mat")

# Vanilla logit model######################################################
# this model is here in order to get signs of coefficients for variable importance plot

fit_logit <- glm(model_form, family = binomial(link = "logit"),
               data = training)

#predicting the test data
logit_probs <- predict(fit_logit, testing, type="response")
logit_class <- predict(fit_logit, testing)
logit_labels <- testing[[target]]

#roc analysis for test data
logit_prediction <- prediction(logit_probs, logit_labels)
logit_performance <- performance(logit_prediction, "tpr", "fpr")
logit_auc <- performance(logit_prediction, "auc")@y.values[[1]]

logit_roc_data <- data.frame(fpr = unlist(logit_performance@x.values),
                           tpr = unlist(logit_performance@y.values),
                           model = "logit")

logit_roc <- ggplot(logit_roc_data, aes(x=fpr,
                                         ymin=0,
                                         ymax=tpr,
                                         group = model,
                                         fill = model,
                                         color = model)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr), lwd = 1) +
  ggtitle(paste0("ROC Curve, AUC = ", round(lr_auc, 2))) +
  theme_hc() +
  geom_abline(intercept = 0, slope = 1, size = 1, color = "black", lty = "dashed") +
  labs(x = "1-Specificity", y = "Sensitivity")
ggplotly(logit_roc)

# Variable importance and direction plot##################################################
logit_coef <- data.frame(var = names(fit_logit$coefficients),
                       coef = fit_logit$coefficients) %>%
  mutate(sign = ifelse(coef < 0, -1, 1))

varImp_s <- varImp %>%
  left_join(logit_coef, by = c("var" = "var")) %>%
  filter(complete.cases(.)) %>%
  mutate(imp_sign = imp*sign)

VI_s <- ggplot(varImp_s, aes(y = reorder(var, imp), x = imp_sign)) +
  geom_point(col = "tomato2", size = 4) + # Draw points
  theme_hc() +
  geom_vline(xintercept = 0) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  geom_segment(aes(y = var,
                   yend = var,
                   x = min(imp_sign),
                   xend = max(imp_sign)),
               linetype = "dashed",
               size = 0.1) + # Draw dashed lines
  labs(title = "Variable importance",
       x = "Relative variable importance and direction",
       y = "Variable")
ggplotly(VI_s)

ExportPlot(VI_s, "figures/VI_direction")


