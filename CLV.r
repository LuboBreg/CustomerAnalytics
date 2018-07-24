#Liubomyr Bregman 
#CLV Lviv Data Science school 


# Special thanks to Sergey Bryl', this part of session is largely inspired by his blog  


set.seed(123)

library(readr)
library(lubridate)
library(rfm)
library(dplyr)
library(reshape2)
library(ggplot2)
library(cluster)
library(randomForest)


transactions <- read_delim("transactions.csv", 
                           "|", escape_double = FALSE, 
                           trim_ws = TRUE)

users <- read_delim("users.csv", 
                           "|", escape_double = FALSE, 
                           trim_ws = TRUE)

products <- read_delim("products.csv", 
                    "|", escape_double = FALSE, 
                    trim_ws = TRUE)



#Data edits
df <- merge(merge(transactions, users), products)
df$ID_product <- as.factor(df$ID_product)
df$ID_category <- as.factor(df$ID_category)
df$gender <- as.factor(df$gender)
df$month <- floor_date(df$txn_time, "month")
df$ID_category_prev <- lag(df$ID_category, n = 1L,  order_by = df$txn_time)
df$date <- as.Date(df$txn_time, origin="2012-01-01")



# creating data frames with CAC 
cac <- data.frame(ID_user=unique(df$ID_user), cac=sample(c(100:4000), length(unique(df$ID_user)), replace=TRUE))
                         



today <- as.Date(max(df$date), format='%Y-%m-%d')
discount_rate <- (1.05)^(1/365) -1


# creating data frames with CLV
df$grossmarg <- 0.2*df$price

clv <- df %>%
  group_by(ID_user) %>%
  summarise(clv=sum(grossmarg/((1 + discount_rate)^as.numeric(today - df$date)))) %>%
  ungroup()

# reporting date



# processing data
#df <- dcast(df, ID_txn + ID_user + gender + date ~ ID_product, value.var='ID_product', fun.aggregate=length)

df <- df %>%
  group_by(ID_user) %>%
  mutate(frequency=n(),
         recency=as.numeric(today-date)) %>%
  filter(date==max(date)) %>%
  filter(ID_txn==max(ID_txn)) %>%
  dplyr::ungroup()



df.segm <- df %>%
  mutate(segm.freq=ifelse(between(frequency, 1, 1), '1',
                          ifelse(between(frequency, 2, 2), '2',
                                 ifelse(between(frequency, 3, 3), '3',
                                        ifelse(between(frequency, 4, 4), '4',
                                               ifelse(between(frequency, 5, 5), '5', '>5')))))) %>%
  mutate(segm.rec=ifelse(between(recency, 0, 6), '0-20 days',
                         ifelse(between(recency, 21, 45), '21-45 days',
                                ifelse(between(recency, 46, 70), '14-70 days',
                                       ifelse(between(recency, 71, 100), '71-100 days',
                                              ifelse(between(recency, 101, 150), '101-150 days', '>151 days')))))) %>%
  
  arrange(ID_user)

head(df.segm)
#Filter the churn

df.segm<-df.segm[which(df.segm$recency <360),]


# defining order of boundaries
df.segm$segm.freq <- factor(df.segm$segm.freq, levels=c('>5', '5', '4', '3', '2', '1'))
df.segm$segm.rec <- factor(df.segm$segm.rec, levels=c('0-20 days', '21-45 days', '14-70 days', '71-100 days', '101-150 days', '>151 days'))


df.segm <- merge(df.segm, cac, by='ID_user')
df.segm <- merge(df.segm, clv, by='ID_user')



lcg.clv <- df.segm %>%
  group_by(segm.rec, segm.freq) %>%
  summarise(quantity=n(),
            # calculating cumulative CAC and CLV
            cac=sum(cac),
            clv=sum(clv)) %>%
  ungroup() %>%
  # calculating CAC and CLV per client
  mutate(cac1=round(cac/quantity, 2),
         clv1=round(clv/quantity, 2))

lcg.clv <- melt(lcg.clv, id.vars=c('segm.rec', 'segm.freq', 'quantity'))

ggplot(lcg.clv[lcg.clv$variable %in% c('clv', 'cac'), ], aes(x=variable, y=value, fill=variable)) +
  theme_bw() +
  theme(panel.grid = element_blank())+
  geom_bar(stat='identity', alpha=0.6, aes(width=quantity/max(quantity))) +
  geom_text(aes(y=value, label=value), size=4) +
  facet_grid(segm.freq ~ segm.rec) +
  ggtitle("LifeCycle Grids - CLV vs CAC (total)")

ggplot(lcg.clv[lcg.clv$variable %in% c('clv1', 'cac1'), ], aes(x=variable, y=value, fill=variable)) +
  theme_bw() +
  theme(panel.grid = element_blank())+
  geom_bar(stat='identity', alpha=0.6, aes(width=quantity/max(quantity))) +
  geom_text(aes(y=value, label=value), size=4) +
  facet_grid(segm.freq ~ segm.rec) +
  ggtitle("LifeCycle Grids - CLV vs CAC (average)")




#SEGMENTATION 

df.segm$Churn <- ifelse (df.segm$recency > 180, 1 ,0)


clust_df <- na.omit(data.matrix(df.segm[c(5,7,11,12,13,16,17, 19)]))





# Number of clusters average within-cluster sum of squares
num <- 20
wss <- (nrow(clust_df)-1)*sum(apply(clust_df,2,var))


for (i in 1:num) {wss[i] <- sum(kmeans(clust_df,
                                       centers=i)$withinss)


}

p<- plot(1:num, wss, type = 'b', xlab=paste("Number of Clusters"))


#train a kmeans 

kmeans <- kmeans((clust_df), 10)
df.segm <-na.omit(df.segm)
df.segm$cluster <- kmeans$cluster
qplot(df.segm$cluster)


#Churn model

churn_model <- randomForest(formula =as.formula('Churn ~ frequency + recency + clv + price + cac + age + gender'), 
             data = df.segm)

df.segm$Churb_prob <- predict(churn_model, df.segm)
qplot(df.segm$Churb_prob)

#Clusters review 
library(plyr)

segments <- ddply(df.segm,~cluster,
      
      summarise,
      mean_clv=mean(clv), 
      mean_recency = mean(recency),
      mean_frequency = mean(frequency),
      mean_churn = mean(Churb_prob)
)


segments$ExpectedValue <- segments$mean_clv * (1 - segments$mean_churn)

View(segments)



