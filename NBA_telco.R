### Viva la multiclass classification

library(tidyverse)
library(readr)
library(doParallel)
library(caret)
library(pROC)
library(lubridate)
library(partykit)
library(ROCR)
library(reshape2)
library(gridExtra)
library(ggplot2)
library(ggthemes)
library(dummies)
library(RODBC)
library(tidyR)
library(reshape2)
library(randomForest)



# Export plot in PDF and PNG (optionally EPS)
ExportPlot <- function(gplot, filename, width = 2, height = 1.5) {
    # Notice that A4: width=11.69, height=8.27
    ggsave(paste(filename, '.pdf', sep = ""), gplot, width = width, height = height)
    #postscript(file = paste(filename, '.eps', sep = ""), width = width, height = height)
    print(gplot)
    dev.off()
    png(file = paste(filename, '_.png', sep = ""), width = width * 100, height = height * 100)
    print(gplot)
    dev.off()
}

# Function recognizes categorical vars with categories that cover only negligible number of observations
poor_levels <- function(dtf, nms = colnames(dtf), trim = 0.01 * nrow(dtf)) {
    stopifnot(is.null(nms) | (is.character(nms) & length(nms) > 0))
    stopifnot(is.data.frame(dtf))

    level_table <-
    lapply(dtf[, which(lapply(dtf[, nms], class) %in% c("character", "factor"))], table)
    lapply(level_table, function(tbl) {
        names(which(tbl < trim))
    })
}

# Observations that contain more tha one reaction to a campaign type are multiplicated in a way that there is one reaction per row.
# e.g. Subscriber that reacted to campaigns C1 an c6 has vector that looks like: 1 0 0 0 0 1 0 0.
# We expand this observation into two rows wit vectors: 1 0 0 0 0 0 0 0 and 0 0 0 0 0 1 0 0.
expand_rows <- function(df, ids, campaign_names) {

    sub_df <- df %>%
    select_(.dots = campaign_names) %>%
    select(which(colSums(., na.rm = TRUE) > 0))
    sub_df_ids <- select_(df, .dots = ids)
    sub_df_m <- cbind(sub_df_ids, sub_df)

    flt_df <- sub_df_m %>% filter(rowSums(sub_df, na.rm = TRUE) > 1)
    rest_df <- sub_df_m %>% filter(rowSums(sub_df) <= 1)
    campaign_names_included = colnames(flt_df[, - c(1:2)])

    tmp_df <- tidyr::unite_(flt_df, "new_col", colnames(sub_df), sep = ",", remove = TRUE)
    tmp_df$cmp <- lapply(tmp_df$new_col,
                     function(x) campaign_names_included[which(as.integer(unlist(strsplit(x, split = ","))) == 1)])

    unnest_df <- tmp_df %>%
    unnest(cmp)

    ohe_df <- dummy.data.frame(unnest_df, names = "cmp",
                           dummy.classes = campaign_names,
                           omit.constants = FALSE,
                           all = TRUE)

    missing_columns <- setdiff(campaign_names, campaign_names_included)

    ohe_df <- select(ohe_df, - new_col)
    colnames(ohe_df) <- c(ids, gsub("cmp", "", colnames(ohe_df[-c(1:2)])))
    null_cols <- setdiff(setdiff(campaign_names, missing_columns), colnames(ohe_df[-c(1:2)]))
    ohe_df <- ohe_df %>% 
      mutate_(campaign_13 = 0, campaign_16 = 0)
    ohe_df <- select_(ohe_df, .dots = c(ids, setdiff(campaign_names, missing_columns)))

    final_df <<- rbind(rest_df, ohe_df)

    rest_of_colnames = intersect(colnames(sub_df), campaign_names)
    df_dropped <- df[, !(names(df) %in% rest_of_colnames)]
    df_expanded <<- merge(x = df_dropped, y = final_df, by = ids, all.y = TRUE)
}

# Define global parameters
target = "Campaign"
idVars = c('SubscriberID', 'SnapshotDate')
notNullThreshold = 0.05
nCores = 3
noLevelsThreshold = 256
CustomMetricShareOfTop = 0.05
poorCampaignThreshold = 0.5
seed <- 42

# Load Data
data <- read_csv("NBA_sample.csv", 
                      col_types = cols(X1 = col_skip()))

########################## Data prep ###################################################

# classes of the target variable are recognized by "campaign" prefix.
campaign_names <- grep("campaign_*", colnames(data), value = TRUE)

# New variable for subscriber's age
data$Age <- as.numeric(round((data$SnapshotDate - data$BirthDate) / 365.25))

# expand rows with more than one response in capmaign vector
expand_rows(df = data, ids = idVars, campaign_names = campaign_names)

# Artifical ID variable, better be safe than sorry. Because after rows expansion, combination of ID and date is not unique anymore.
df_expanded$ID <- seq(1, nrow(df_expanded), 1)

# Replace "-" from column names with underscores. Just to be sure that column names are easy to work with.
colnames(df_expanded) <- gsub("-", "", colnames(df_expanded))

#make sure there is just one campaign per row
campaign_types <- select_(df_expanded, .dots = c("ID", campaign_names))
campaign_types$sum <- rowSums(select(campaign_types, -1))
table(campaign_types$sum)

# Observations with no reaction to any campaign type have "no_response" class in the target variable.
campaign_types$No_Response <- ifelse(campaign_types$sum == 0, 1, 0)
#head(campaign_types, 30)

# Create multiclass target variable from campaign types responses. Pivot columns to rows
Flag <- melt(select(campaign_types, - sum),
                     variable.name = target,
                     id.vars = "ID") %>%
  filter(value == 1) %>%
  select(-value)
head(Flag, 30)
round(prop.table(table(Flag$Campaign)) * 100, 2)


# Get rid of useless variables
MyDF <- select(df_expanded, - c(
    StartDate,
    EndDate,
    ContractLengthM,
    CycleDetails,
    Cycle,
    PackagePlanSubscriptionDate,
    DaysToActiveStopDate,
    DaysToSuspendStopDate,
    DaysToDisableStopDate,
    BirthDate
    ))

# Join the base dataset with multiclass target variable. Filter by Active and Individual Subscribers
MyDF <- inner_join(MyDF, Flag, by = c("ID" = "ID")) %>%
select(-one_of(campaign_names)) %>%
filter(CustomerStatus == "Active" & CustomerTypeGroup == "Individual")
nrow(MyDF)
colnames(MyDF)

# Group camapigns with low number of observations into one group called "Other"
MyDF$Campaign <- as.character(MyDF$Campaign)
tbl <- round(prop.table(table(MyDF$Campaign)) * 100, 2)
poor_campaigns <- names(tbl[tbl < poorCampaignThreshold])
MyDF[, "Campaign"] <- ifelse(MyDF[, "Campaign"] %in% poor_campaigns, 'Other', MyDF[, "Campaign"])
table(MyDF$Campaign)

# Define categorical and continuous variables
categoricalVarsPre <- names(sapply(MyDF, typeof))[sapply(MyDF, typeof) == "character"]
continuousVarsPre <- colnames(MyDF)[!(colnames(MyDF) %in% c(target, idVars, categoricalVarsPre))]
cnt <- dim(MyDF)[1]
cntNotNull <- sapply(MyDF[c(target, continuousVarsPre, categoricalVarsPre)], function(x) length(x[!is.na(x)]))
avgs <- sapply(MyDF[, continuousVarsPre], mean, na.rm = TRUE)
isDistinct <- sapply(MyDF[c(target, continuousVarsPre, categoricalVarsPre)], function(x) length(unique(x[!is.na(x)]))) > 1

goodVars <- colnames(MyDF[c(target, continuousVarsPre, categoricalVarsPre)])[(cntNotNull * 1.0 / cnt > notNullThreshold & isDistinct)]

continuousVars <- continuousVarsPre[continuousVarsPre %in% goodVars]
categoricalVars <- categoricalVarsPre[categoricalVarsPre %in% goodVars]

# Remove poor levels from discrete variables
pl <- poor_levels(MyDF[, categoricalVars])
MyDF[, "CustomerSegment"] <- ifelse(MyDF[, "CustomerSegment"] %in% pl$CustomerSegment, 'Other', MyDF[, "CustomerSegment"])
MyDF[, "Nationality"] <- ifelse(MyDF[, "Nationality"] %in% pl$Nationality, 'Other', MyDF[, "Nationality"])
MyDF[, "Occupation"] <- ifelse(MyDF[, "Occupation"] %in% pl$Occupation, 'Other', MyDF[, "Occupation"])
MyDF[, "SalesChannel"] <- ifelse(MyDF[, "SalesChannel"] %in% pl$SalesChannel, 'Other', MyDF[, "SalesChannel"])

MyDF <- MyDF[, c(idVars, continuousVars, categoricalVars)]

# Fill NA values with averages
catVars <- names(sapply(MyDF, typeof))[sapply(MyDF, typeof) == "character"]
contVars <- colnames(MyDF)[!(colnames(MyDF) %in% c(target, idVars, catVars))]
for (cont in contVars) {
    MyDF[is.na(MyDF[, cont]), cont] <- avgs[cont]
}
for (cat in catVars) {
    MyDF[is.na(MyDF[, cat]), cat] <- "unknown"
}

# Divide the initial data into train and test data sets using stratified sampling
round(prop.table(table(MyDF$Campaign)), 3) * 100

trgtCnt <- MyDF %>%
    group_by_(target) %>%
    summarise(TargetCnt = n())
set.seed(seed)
idx <- createDataPartition(MyDF[, target], p = 0.6)[[1]]
train <- MyDF[idx,]
test <- MyDF[-idx,]

dim(train)
table(train$Campaign)

# Final adjustments do dataset make sure there are factors ,not just characters
modelVarsAll <- colnames(train)[!(colnames(train) %in% c(idVars, target))]
train[catVars] <- lapply(train[catVars], factor)
test[catVars] <- lapply(test[catVars], factor)
MyDF[catVars] <- lapply(MyDF[catVars], factor)

########################## Classification ###################################################

model_form <- as.formula(paste(target, " ~ . -", paste("SubscriberID", "SnapshotDate", "ID", sep = " - ", collapse = "-")))

# Define hyperparameters, grow tree
# Here, our main goal is not to maximize accuracy or AUC, instead we set hyperparameters in a way we
# get some reasonable number of microsegments. Tree is controlled via the parameters in the control vector.
# Basically the higher numbers you set, the more difficult it is for the tree to split.
# Mincriterion parameter sets the minimum statistcial significance of a split.
# Minprob sets the minimum percentage of observations needed to establis ha terminal node.
# Minsplit sets the minimum sum of weights in a node in order to be considered for splitting.
ctrl_multi <- ctree_control(maxdepth = 30,
    mincriterion = 0.95,
    minprob = 0.01,
    minsplit = 1000L,
    multiway = TRUE)
# Grow a tree
start.time <- Sys.time()
fit_multi <- ctree(model_form, data = train, control = ctrl_multi)
end.time <- Sys.time()
time.taken <- end.time - start.time
cat('\n', paste("Postpaid model training took", round(time.taken, 1), units(time.taken)))
print(fit_multi)

#save trained model . Each model is saved with its time stamp. It takes some time :(
file_post = paste0("trained_models_postpaid/", "tree_postpaid_", format(Sys.time(), "%d_%m_%y_%H_%M"), ".rda")
save(fit_multi, file = file_post)

# load saved model. Either the same that was just saved with commaned load(file), or arbitrary one with custom path.
#e.g. "trained_models_prepaid/my_favourite_model".
load(file_post)

# this plots the tree graph, however I do not recommend to use if you have more than 100 leaves, it gets cluttered
plot(fit_multi, main = "Tree for NBO postpaid", gp = gpar(fontsize = 5), # font size changed to 6
  inner_panel = node_inner,
  ip_args = list(
       abbreviate = TRUE,
      id = FALSE))

# Make predictions using the test data set
#probs_post <- predict(fit_multi_post, test_post, type = "prob")
#preds_post <- predict(fit_multi_post, test_post)
#nodes_post <- predict(fit_multi_post, test_post, type = "node")

# Map scored data back to the test set
#results_post <- data.frame(ID = test_post$ID, Campaign = test_post$Campaign, Prediction = preds_post, MicroSegment = nodes_post)

# Make predictions using the all data
probs <- predict(fit_multi, MyDF, type = "prob")
preds <- predict(fit_multi, MyDF)
nodes <- predict(fit_multi, MyDF, type = "node")

# Map scored data back to the original set
results <- data.frame(SubscriberID = MyDF$SubscriberID,
    SnapshotDate = MyDF$SnapshotDate, ID = MyDF$ID,
    MicroSegment = nodes,
    Campaign = MyDF$Campaign)

# pivot table with response rates
results_na <- left_join(campaign_types, results)
results_response <- results_na %>% select(-c(SubscriberID, SnapshotDate, ID, Campaign, sum)) %>%
  group_by(MicroSegment) %>%
  summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  slice(-n())

# Which segmants have highest response rates per campaign?
max_responses <- unlist(apply(select(results_response, - MicroSegment), 2, which.max))
max_responses_df <- data.frame(Campaign = names(max_responses), MicroSegment = max_responses, row.names = NULL)
max_responses_df$Campaign <- as.character(max_responses_df$Campaign)
max_responses_df$ResponseRate = diag(as.matrix(select(results_response, - MicroSegment)[max_responses_df[, 2], max_responses_df[, 1]]))
#max_responses_df
write.csv(max_responses_df, "max_segments_results_NBO_postpaid_multi.csv", row.names = FALSE)

# Which campaign is the best for each segment?
max_campaigns <- as.numeric(unlist(apply(select(results_response, - MicroSegment, - No_Response), 1, which.max)))
max_campaigns_df <- data.frame(MicroSegment = results_response[, 1],
Campaign = gsub("\\..*", "", colnames(results_response)[-1][max_campaigns]))
RR <- NULL
for (x in 1:as.numeric(nrow(results_response))) {
    rr <- as.numeric(select_(results_response[x,], as.character(max_campaigns_df[x, 2])))
    RR <- c(RR, rr)
}
max_campaigns_df$ResponseRate <- RR
max_campaigns_df
write.csv(max_campaigns_df, "max_campaign_results_NBO_postpaid_multi.csv", row.names = FALSE)

# Calculate one-vs-all performance measures per campaign for various cutoff points
# Unfortunatelly there is no nice library in R to do this, hence we have to use nasty for loops
predictions <- cbind(results, probs)
performance_all <- NULL
performance <- data.frame(Campaign = 0, tpr = 0, fpr = 0, cut = 0)
cutoff <- seq(0.01, 1, 0.01)
for (name in as.character(unique(MyDF$Campaign))) {
for (i in 1:length(cutoff)) {
    cut <- cutoff[i]
    dotsP <- paste0(name, " > ", cut)
    dotsN <- paste0(name, " < ", cut)
    tp <- predictions %>% filter(Campaign == name) %>% filter_(.dots = dotsP) %>% nrow()
    fn <- predictions %>% filter(Campaign == name) %>% filter_(.dots = dotsN) %>% nrow()
    fp <- predictions %>% filter(Campaign != name) %>% filter_(.dots = dotsP) %>% nrow()
    tn <- predictions %>% filter(Campaign != name) %>% filter_(.dots = dotsN) %>% nrow()
    tpr <- tp / (tp + fn)
    fpr <- fp / (fp + tn)
    row <- c(name, tpr, fpr, cut)
    performance[i,] <- row
}
    performance_all <- rbind(performance_all, performance)
}
performance_all$tpr <- as.numeric(performance_all$tpr)
performance_all$fpr <- as.numeric(performance_all$fpr)

# Plot one-vs-all ROC curves
ROCs <- ggplot(data = performance_all, aes(x = fpr, y = tpr, group = Campaign, color = Campaign)) +
    geom_line(size = 1) +
    theme_few() +
    geom_abline(intercept = 0, slope = 1, size = 1, color = "black", lty = "dashed") +
    labs(x = "1-Specificity", y = "Sensitivity") +
    ggtitle("One-vs-all ROC curves for multinomial classification model; \nCalculated on all data \nPostpaid")
ROCs

# Get some insight on relative variable importance through random forest model. it takes time.
fit_varImp <- randomForest(model_form, data = train, ntree = 50)
varImp <- importance(fit_varImp, type = 2)
varImp <- data.frame(var = rownames(varImp), imp = varImp[, 1])
varImp$imp <- as.numeric(varImp$imp)
varImp$var <- factor(varImp$var, levels = varImp$var)
varImp <- arrange(varImp, desc(imp)) %>% top_n(20)
#head(varImp, 4)
varImp$imp <- as.numeric(varImp$imp)
# Plot of variable importance
VI <- ggplot(varImp, aes(y = reorder(var, imp), x = imp)) +
  geom_point(col = "tomato2", size = 4) + # Draw points
    geom_segment(aes(y = var,
                   yend = var,
                   x = min(imp),
                   xend = max(imp)),
               linetype = "dashed",
               size = 0.1) + # Draw dashed lines
    labs(title = "Variable importance in NBO model postpaid", x = "Mean decrese in Ginni", y = "Variable")
VI

# Save plots to pdf and png
ExportPlot(ROCs, "NBO_plot_postpaid_ROC", width = 6, height = 4)
ExportPlot(VI, "NBO_plot_postpaid_VI", width = 6, height = 4)

# Number of subscribers in each microsegment and average ARPU
results_joined <- cbind(results, ARPU = MyDF$ARPU)
n_seg <- group_by(results_joined, MicroSegment) %>% summarise(N = length(ID), avg_ARPU = mean(ARPU))
write.csv(n_seg, "n_seg_postpaid.csv", row.names = FALSE)

# Join the number of ppl in microsegment to the resulting pivot table and save it as a csv file.
results_response <- left_join(results_response, n_seg)
head(results_response)
write.csv(results_response, "results_NBO_postpaid_multi.csv", row.names = FALSE)

# Write microsegments joined to the original dataset into the DB
tableNBOPostpaidName = 'nbo.MicroSegmentsPostpaid'

#table columns and data types
idVarsType = c("bigint", "date", "bigint")
MSType = c("bigint")
varTypes <- c(idVarsType,MSType)

results_post_to_db <- select(results_post, - Campaign)
names(varTypes) <- colnames(results_post_to_db)
con <- odbcConnect(dbName)
sqlQuery(con, paste("DROP TABLE IF EXISTS", tableNBOPostpaidName, ";"))
sqlSave(con, results_post_to_db, tableNBOPostpaidName, varTypes = varTypes, rownames = FALSE)
odbcClose(con)
