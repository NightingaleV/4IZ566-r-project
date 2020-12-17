if (!require(caret)) install.packages('caret')
if (!require(package)) install.packages('gdata')
if (!require(package)) install.packages('ggplot2')
if (!require(package)) install.packages('corrplot')
if (!require(package)) install.packages('gridExtra')
if (!require(package)) install.packages('dplyr')
if (!require(package)) install.packages('dlookr')
if (!require(package)) install.packages('randomForest')

library(caret)
library(gdata)
library(ggplot2)
library(corrplot)
library(gridExtra)
library(dplyr)
library(randomForest)
library(dlookr)

set.seed(150)
df <- read.csv("data/online_shoppers_intention.csv", sep=",")
df <- tbl_df(df)

numeric_columns <- c('Administrative','Administrative_Duration','Informational','Informational_Duration',
                     'ProductRelated','ProductRelated_Duration','BounceRates','ExitRates','PageValues','SpecialDay')
nominal_columns <- c('Weekend', 'VisitorType','TrafficType','Region','Browser','OperatingSystems','Month')
target_class <- c('Revenue')

# ------------------------------
# --- Data Preparation
# ------------------------------
# Checking the class types for each coulmns
sapply(df, class)

# Checking for Nulls in the data
colSums(is.na(df))

# Remove Missing Rows
df <- df[complete.cases(df), ]

# Check that the proportion of data 
table(df$Revenue)
prop.table(table(df$Revenue))

# Dplyr 
dplyr::filter(df, Month==1)
dplyr::select(df, Administrative:ProductRelated_Duration, contains(''))

# Remove Invalid data - duration cant be negative
for (numeric_feature in numeric_columns) {
  df <- df %>% filter(numeric_feature >= 0)
}

# Convert Months to numeric cat
months_numeric_codes <- list(jan=1,feb=2,mar=3,apr=4,may=5,jun=6,jul=7,aug=8,sep=9,oct=10,nov=11,dec=12)
convert_month_to_numeric_cat <- function(x) match(tolower(x), tolower(month.abb))
df <- df %>% mutate(Month=convert_month_to_numeric_cat(Month))

# Create boolean features
df <- df %>% mutate(Administrative_Visited = (Administrative > 0))
df <- df %>% mutate(Informational_Visited = (Informational > 0)) 
df <- df %>% mutate(ProductRelated_Visited = (ProductRelated > 0))
df <- df %>% mutate(IsSpecialDate = (SpecialDay > 0))

# Create bins
cut(df$Administrative, 5, labels = c(1, 2, 3, 4, 5))
cut(df$Informational, 5, labels = c(1, 2, 3, 4, 5))
cut(df$ProductRelated, 5, labels = c(1, 2, 3, 4, 5))

# Create 

Browser <- factor(df$Browser)

dummies = 
  dummies = model.matrix(~Browser)
dummies[,2:ncol(dummies)]

bind_rows(df, dummies)
;bind_rows

dim(df)
str(df)
summary(df)
introduce(df)

# ------------------------------
# --- Exploratory analysis
# ------------------------------
# Checking the class types for each coulmns
sapply(rawdata, class)

plot_correlation(df)

# Explore Nominal Columns
par(mfcol=c(3,3))
for (nominal_feature in c(nominal_columns)) {
  factor_column <- factor(df[,nominal_feature])
  plot(factor_column, main=paste("Histogram of ",nominal_feature))
}

table(df$Region)

plot_Administrative <- ggplot(df, aes(x=Administrative)) + geom_histogram(aes(y=..density..))
plot_Informational <- ggplot(df, aes(x=Informational)) + geom_histogram(aes(y=..density..))
plot_ProductRelated <- ggplot(df, aes(x=ProductRelated)) + geom_histogram(aes(y=..density..))
plot_Administrative_Duration <- ggplot(df, aes(x=Administrative_Duration)) + geom_histogram(aes(y=..density..))
plot_Informational_Duration <- ggplot(df, aes(x=Informational_Duration)) + geom_histogram(aes(y=..density..))
plot_ProductRelated_Duration <- ggplot(df, aes(x=ProductRelated_Duration)) + geom_histogram(aes(y=..density..))
grid.arrange(plot_Administrative, plot_Informational, plot_ProductRelated,plot_Administrative_Duration, plot_Informational_Duration, plot_ProductRelated_Duration, ncol=3, nrow=2)

par(mfcol=c(3,3))
for (numeric_feature in c(numeric_columns)) {
  plot1 <- ggplot(df, aes(x=numeric_feature))  + geom_histogram(aes(y=..density..), colour="black", fill="white")+
    geom_density(alpha=.2, fill="#FF6666") 
  plot1
}

plot_ProductRelated <- ggplot(df, aes(x=ProductRelated))  + geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.1, fill="#FF6666") 
plot_ProductRelated
grid.arrange(plot1, plot_ProductRelated, ncol=2)

p <- ggplot(df, aes(Administrative)) + geom_histogram(binwidth=1, color="gray", aes(fill=Administrative))
p

# Corelation Matrix
correlations <- cor(select(df, numeric_columns),method="pearson")
corrplot(correlations, number.cex = .9, method = "circle", type = "full", tl.cex=0.8,tl.col = "black")

# ------------------------------
# --- Classification Task
# ------------------------------
# Sampling data for training and testing
require(caTools)
sample = sample.split(rawdata$Class, SplitRatio = .75)
train = subset(rawdata, sample == TRUE)
test  = subset(rawdata, sample == FALSE)

library(caret)
cm<-confusionMatrix(table(pred,test$Class))