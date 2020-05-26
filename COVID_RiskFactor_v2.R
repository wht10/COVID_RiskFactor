#loading required packages
if(!require(caret)) install.packages("caret")
if(!require(corrplot)) install.packages("corrplot")
if(!require(tidyverse)) install.packages("tidyverse") 
if(!require(ggplot)) install.packages("ggplot") 
if(!require(dplyr)) install.packages("dplyr") 
if(!require(kableExtra)) install.packages("kableExtra")
if(!require(lubridate)) install.packages("lubridate")
if(!require(googledrive)) install.packages("googledrive")
if(!require(matrixStats)) install.packages("matrixStats")
if(!require(Hmisc)) install.packages("Hmisc")
if(!require(rattle)) install.packages("rattle")
if(!require(Rtsne)) install.packages("Rtsne")
if(!require(usmap)) install.packages("usmap")
if(!require(maps)) install.packages("maps")
if(!require(mapproj)) install.packages("mapproj")
if(!require(ggthemes)) install.packages("ggthemes")


library(dplyr)
library(tidyverse)
library(kableExtra)
library(ggplot2)
library(lubridate)
library(googledrive)
library(caret)
library(matrixStats)
library(usmap)
library(maps)
library(mapproj)
library(ggthemes)
library(Hmisc)
library(corrplot)
library(rattle)
library(Rtsne)


#Downloading dataset
if(!exists("covid")){
  folder_url <- "https://drive.google.com/drive/folders/1Va6A4pJw6J6QajtwJcn2EETNq5RuVcZ7"
  folder <- drive_get(as_id(folder_url))
  rdrfiles <- drive_ls(folder)
  walk(rdrfiles$id, ~ drive_download(as_id(.x),overwrite = T))}

#read in data
covid <- read.csv("US_counties_COVID19_health_weather_data.csv")

#make a smaller dataset for visualization transforming date to Date class
red_covid <- covid[,1:12] %>% mutate(date = ymd(date))
glimpse(red_covid)

#summary info for counties
red_covid  %>% 
  summarise(Counties = n_distinct(fips),Days_of_Data = n_distinct(date))

#plotting total death per state
totdeathpstate <- red_covid %>% group_by(state) %>% filter(date == max(date)) %>% summarise(totdeath = sum(deaths))
plot_usmap(region = "state", data = totdeathpstate, values = "totdeath", color = "black") +
  scale_fill_viridis_c(name = "Total Death per State", label = scales::comma) +
  theme(legend.position = "right") + ggtitle("Total COVID-19 Deaths per State\nas of 5-14-20")

#log transforming death data and plotting
logtotdeathpstate <- red_covid %>% group_by(state) %>% filter(date == max(date)) %>%
  summarise(totdeath = log(sum(deaths)))
plot_usmap(data = logtotdeathpstate, values = "totdeath", color = "black") +
  scale_fill_viridis_c(name = "Log(Total Death per State)", label = scales::comma) +
  theme(legend.position = "right") + ggtitle("Log Transformed\nTotal COVID-19 Deaths per State\nas of 5-14-20")

#county logtransofrm data plot
logdeathcount <- red_covid %>% group_by(fips) %>%  filter(date == max(date)) %>% mutate(deaths = replace(deaths,deaths == 0, 1)) %>% 
  summarise(totdeath = log(deaths),county = first(county)) 
plot_usmap(region = "counties",data = logdeathcount, values = "totdeath", color = "black") +
  scale_fill_viridis_c(name = "Log(Total Death per County)", label = scales::comma) +
  theme(legend.position = "right")

#counties with no deaths
red_covid %>% filter(date == max(date)) %>% 
  summarise(Deathless_Counties = sum(deaths == 0), Percent_Deathless = mean(deaths == 0))

#missing value check
sapply(red_covid,function(x)sum(is.na(x)))

#plotting by county over time 
#setting parameter for minimum number of deaths for county to be analyzed
min_death <- 10
#plotting deaths in counties over time
red_covid %>%
  group_by(fips) %>%
  filter(deaths >= min_death) %>%
  mutate(days_since_min_death = as.numeric(date-min(date[deaths >= min_death]))) %>%
  ungroup() %>%
  ggplot(aes(x=days_since_min_death, y=deaths,
             color = fips)) + 
  geom_line(alpha = 0.4) +
  xlab(sprintf("Days since %i deaths", min_death)) + 
  theme(legend.position = "none") + 
  ggtitle("COVID19 deaths by US county")

#plotting death by county over time on log axis
red_covid %>%
  group_by(fips) %>%
  filter(deaths >= min_death) %>%
  mutate(days_since_min_death = as.numeric(date-min(date[deaths >= min_death]))) %>%
  ungroup() %>%
  ggplot(aes(x=days_since_min_death, y=deaths,
             color = fips)) + 
  geom_line(alpha = 0.4) +
  xlab(sprintf("Days since %i deaths", min_death)) + 
  scale_y_continuous(trans = "log") + 
  theme(legend.position = "none") + 
  ggtitle("COVID19 log(deaths) by US county")

#plotting first 14 days with log death by county
red_covid %>%
  group_by(fips) %>%
  filter(deaths >= min_death) %>%
  mutate(days_since_min_death = as.numeric(date-min(date[deaths >= min_death])),log_deaths = log(deaths)) %>%
  ungroup() %>% filter(days_since_min_death < 14) %>% 
  ggplot(aes(x=days_since_min_death, y=log_deaths,
             color = fips)) + 
  geom_line(alpha = 0.4) +
  xlab(sprintf("Days since %i deaths", min_death)) + 
  theme(legend.position = "none") + 
  ggtitle("COVID19 deaths by US county")

#log transform and curve fit
#writing function for applying linear model for curve fitting the log transformed data for each county
get_slope <- function(data){
  fit <- lm(logdeaths ~ days, data = data)
  data.frame(slope = fit$coefficients[2])
}

#aplying get_slope function to county log_death data, and removing slopes <= 0
slopes <- red_covid %>%
  group_by(fips) %>%
  filter(deaths >= min_death) %>%
  mutate(days = as.numeric(date-min(date[deaths >= min_death])),logdeaths = log(deaths)) %>% 
  filter(days <= 14) %>% select(days,logdeaths) %>% 
  do(get_slope(.)) %>% arrange(desc(slope)) %>% filter(slope > 0)

get_rsqrd <- function(data){
  fit <- lm(logdeaths ~ days, data = data)
  sum_fit <- summary(fit)
  data.frame(rsqrd = sum_fit$r.squared)
}
#calculating r squared
rsqrds <- red_covid %>%
  group_by(fips) %>%
  filter(deaths >= min_death) %>%
  mutate(days = as.numeric(date-min(date[deaths >= min_death])),logdeaths = log(deaths)) %>% 
  filter(days <= 14) %>% select(days,logdeaths) %>% 
  do(get_rsqrd(.)) %>% arrange(desc(rsqrd)) %>% filter(fips %in% slopes$fips) %>% replace_na(list(rsqrd = 1))
#average of r squared
mean(rsqrds$rsqrd)


#calculating average slope, or growth, of log data 
mu <-mean(slopes$slope)
#showing distribution of slope data with a red line at the average
slopes %>% ggplot(aes(slope)) + geom_histogram() + geom_vline(xintercept =  mu, col = "red") +
  geom_text(aes(x=mu*1.025, label="Average", y=50), colour="red", angle=90) +
  labs(title ="Distribution of log(death) slopes") + ylab("Number of Counties") +
       xlab("Slope of Log(death) vs. Time") 

#assigning factors labels to categories of slopes
red_covid_slope <- left_join(red_covid,slopes) %>% replace_na(list(slope = 0)) %>% 
  mutate(fct_slp = cut(slope, breaks = c(-Inf,0,mu,Inf), labels=c("None","Below_Avg","Above_Avg"))) %>% 
  mutate(fct_slp = as.factor(fct_slp))
#plotting categories on map
fac_plot_dat <- red_covid_slope %>% group_by(fips) %>% summarise(slope = first(fct_slp))
plot_usmap(region = "counties",data = fac_plot_dat, values = "slope", color = "black", label_color = ) +
  scale_color_brewer(palette = "Set2")+
  theme(legend.position = "right") + 
  labs(fill = "COVID Growth Rate") +
  scale_fill_viridis_d()
#summarizing high slope data
red_covid_slope %>% select(state,county,slope,fct_slp) %>% 
  distinct() %>% arrange(desc(slope)) %>% slice(1:10)

#preparing the covid data for joining with the slopes data
#puliing out features that won't be considered for machine learning, leaving only the 
#slope(outcome) and health-socioeconomic features 
reduced_slope <- red_covid_slope %>% 
  select(-c(stay_at_home_announced,stay_at_home_effective,cases,deaths,date)) %>% 
  distinct()
#extracting health and socioeconmic features from original dataset
dat <- covid[,2:166] %>% 
  select(-c(stay_at_home_announced,stay_at_home_effective,cases,deaths)) %>% 
  distinct() %>% mutate(fct_slope = reduced_slope$fct_slp)
#plotting frequency of different slope categories
dat %>% ggplot(aes(fct_slope)) + geom_bar() + ggtitle("Distribution of Slope Categories") +
  ylab("Count") + xlab("Log_death Slope Categories")
#showing their abundance in a summary
dat %>% group_by(fct_slope) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))

#plotting missing values for predictors and imputation cutoff
na_list <- sapply(dat,function(x)sum(is.na(x)))
tibble(missing_val = na_list, colname = names(na_list)) %>% 
  arrange(desc(missing_val)) %>% mutate(Percent_Data_Missing = missing_val/2896) %>% 
  ggplot(aes(Percent_Data_Missing)) + geom_histogram() +
  geom_vline(xintercept =  .5, col = "red") +
  geom_text(aes(x=.5*1.025, label="Cutoff for Imputation", y=50), 
            colour="red", angle=90, text=element_text(size=11)) +
  ggtitle("Distribution of Missing Values for Predictors") +
  ylab("Count") + xlab("Proportion of Missing Values for a Predictor")

#Finding columns above the imputation cutoff
sparse_features <- tibble(missing_val = na_list, colname = names(na_list)) %>% 
  filter(missing_val/2896 > .5) %>% pull(colname)
#removing features with more than 20% of data missing
clean_dat <- dat %>% select(-all_of(sparse_features))

#splitting data for imputation on predictors
x <- clean_dat[,-length(clean_dat)]
y <- clean_dat[,length(clean_dat)]
#peroforming knn-impuation on predictors
impute_proc <- preProcess(x,method = "knnImpute")
impute_dat <- predict(impute_proc,x)
#verifying missing values have been filled
sum(sapply(impute_dat,function(x)sum(is.na(x))))
#removing feature where missing values were not filled
#this also normalizes the data
impute_dat_clean <- impute_dat %>% 
  select(-presence_of_water_violation)

#verifying missing values have been filled
sum(sapply(impute_dat_clean,function(x)sum(is.na(x))))

#checking for features with near zero variance
nzv <- nearZeroVar(impute_dat_clean, saveMetrics = T)
#Finding how many features hav near zero variance
sum(nzv$nzv)

#Looking for variables that have correlate highly with eath other
#removing county names, state names, and fips, and outcome
x1 <- impute_dat_clean[,-(1:3)]
#calculating correlation matarix
cormat <- cor(x1)
#plotting distribution of Feature Correlation
tibble(corrs = cormat[upper.tri(cormat)]) %>% 
  ggplot(aes(corrs)) + geom_histogram() + geom_vline(xintercept = .75,color = "red") +
  geom_text(aes(x=.75*1.025, label="Cutoff for High Correlation", y=600), 
            colour="red", angle=90, text=element_text(size=11)) +
  ggtitle("Distribution of Correlations of Features") +
  ylab("Count") + xlab("Correlation Values")

#Showing summary of correlation
corrs <-tibble(corrs = cormat[upper.tri(cormat)])
summary(corrs)

  #calculating correlation and p value with rcorr function
cor_ext <- rcorr(as.matrix(x1))
#plotting heatmap of correlated features
{plot.new(); dev.off()}
corrplot(cor_ext$r,type = "upper",order = "hclust", method = "shade",
         p.mat = cor_ext$P,sig.level = .01, insig = "blank",tl.pos = "n")

#establishing function to reformat matrix that rcorr returns
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
#using function to flatten matrix
flatcor <- tibble(flattenCorrMatrix(cor_ext$r,cor_ext$P))
#showing most significantly correlated variables above cutoff threshold
flatcor %>% filter(cor >.75) %>% arrange(desc(p))

#Identifying highly correlated features above cutoff
highlycor <- findCorrelation(cormat,cutoff = .75,exact = T)
#removing highly correlated features
x1_cor_out <- x1[,-highlycor]
#making new correlation matrix for data without highly correlated features
cormat2 <- cor(x1_cor_out)
corrs2 <- tibble(corrs = cormat2[upper.tri(cormat2)])
#plotting correlation ditribution
corrs2 %>% ggplot(aes(corrs)) + geom_histogram() +
  ggtitle("Distribution of Correlations of Features") +
  ylab("Count") + xlab("Correlation Values")
#summarizing correlations after highly correlated features have been removed
summary(corrs2)
#plotting heatmap for features after correlated features have been removed
cor_ext_cor_out <- rcorr(as.matrix(x1_cor_out))
corrplot(cor_ext_cor_out$r,type = "upper",order = "hclust", method = "shade",
         p.mat = cor_ext$P,sig.level = .01, insig = "blank",tl.pos = "n")

#plotting pca with correlated features included
pca_w_cors <- prcomp(x1)
#plotting outcome clusters with PC1 and PC2
data.frame(pca_w_cors$x[,1:2], slope_category = y) %>% 
  ggplot(aes(PC1,PC2,fill = slope_category)) +
  geom_point(cex = 3, pch=21) +
  coord_fixed(ratio = 1) + 
  scale_fill_viridis_d()

#plotting pca without correlated features
pca_wo_cors <- prcomp(x1_cor_out)
data.frame(pca_wo_cors$x[,1:2], slope_category = y) %>% 
  ggplot(aes(PC1,PC2,fill = slope_category)) +
  geom_point(cex = 3, pch=21) +
  coord_fixed(ratio = 1) + 
  scale_fill_viridis_d()

#plotting tsne without correlated features
set.seed(1993)
tsne_out <- Rtsne(as.matrix(x1, pca = F, perplexity = 53, theta =0))
#plotting tsne
data.frame(Dim1 = tsne_out$Y[,1],Dim2 = tsne_out$Y[,2]) %>% 
  ggplot(aes(Dim1,Dim2)) + geom_point(aes(color = y)) + 
  ggtitle("Tsne for Visualizing clusters of Slopes") +
  labs(color = "Slope \nCategories")



#splitting data into validation and training for applyin machine learning algos
set.seed(1993)
edxIndex <- createDataPartition(y,p = .8,list = F,times = 1)
cor <- x1 %>% add_column(fct_slope = y)
nocor <- x1_cor_out %>% add_column(fct_slope = y)
#getting training and validation sets for the data with correlated features
edx_cor <- cor[edxIndex,]
validation_cor <-cor[-edxIndex,]
#subsettign data that has highly correlated features removed
edx_no_cor <- nocor[edxIndex,]
validation_no_cor <- nocor[-edxIndex,]


#####Modeling
#establishing 10-fold cv object
fitControl <- trainControl(## 10-fold CV
  method = "cv",
  number = 10)

#untuned knn for cor data
set.seed(1993)
knn_cor_fit <- train(fct_slope ~ ., data = edx_cor,
                     method = "knn",
                     trControl = fitControl)

#storing results
results <- data.frame(Method = "Untuned knn",Data = "Complete Training",Accuracy = max(knn_cor_fit$results$Accuracy))

#untuned knn for no cor data
knn_no_cor_fit <- train(fct_slope ~ ., data = edx_no_cor,
                        method = "knn",
                        trControl = fitControl)
#storing results
results <- results %>% add_row(Method = "Untuned knn",
                               Data = "Training w/o Corr Features", 
                               Accuracy = max(knn_no_cor_fit$results$Accuracy))


#applying knn with regularized parameters
knn_cor_fit_scale <- train(fct_slope ~ ., data = edx_cor,
                         method = "knn",
                         trControl = fitControl,
                         preProc = c("range"))

#storing results
results <- results %>% add_row(Method = "Scaled KNN",
                               Data = "Complete Training", 
                               Accuracy = max(knn_cor_fit_scale$results$Accuracy))
#yj transform
yj_pre_proc <- preProcess(edx_cor[,-ncol(edx_cor)],method = c("YeoJohnson"))
yj_edx_cor <- predict(yj_pre_proc,edx_cor)
#fitting knn witj yj transformed data
knn_cor_fit_yeo <- train(fct_slope ~ ., data = yj_edx_cor,
                         method = "knn",
                         trControl = fitControl)

#storing results
results <- results %>% add_row(Method = "Yeo-Johnson KNN",
                               Data = "Complete Training", 
                               Accuracy = max(knn_cor_fit_yeo$results$Accuracy))


#aplying parameter tuning to knn
knn_cor_param <- train(fct_slope ~ ., data = yj_edx_cor,
                         method = "knn",
                         trControl = fitControl,
                         tuneGrid = data.frame(k = seq(3,15,2)))

#plotting tuning 
ggplot(knn_cor_param) + ggtitle("Accuracy Optimization")

#storing results
results <- results %>% add_row(Method = "Tuned KNN",
                               Data = "Complete Training", 
                               Accuracy = max(knn_cor_param$results$Accuracy))

#yj transform
yj_pre_proc_out <- preProcess(edx_cor[,-ncol(edx_no_cor)],method = c("YeoJohnson"))
yj_edx_cor_out <- predict(yj_pre_proc,edx_no_cor)

#crating model for non correlated data
knn_no_cor_param_norm <- train(fct_slope ~ ., data = yj_edx_cor_out,
                               method = "knn",
                               trControl = fitControl,
                               tuneGrid = data.frame(k = seq(3,55,2)),
                               )
#storing results
results <- results %>% add_row(Method = "Tuned KNN",
                               Data = "Training w/o Corr Features", 
                               Accuracy = max(knn_no_cor_param_norm$results$Accuracy))

#plotting k optimization
ggplot(knn_no_cor_param_norm) + 
  ggtitle("Accuracy Optimization \nfor Data without Correlated Features")


#for variable importance and w/cor for prediction
rpart_no_cor_fit <- train(fct_slope ~ ., data = edx_no_cor,
                          method = "rpart",
                          trControl = fitControl,
                          tuneGrid = data.frame(cp = seq(.001,.02,.002)))
#plotting CART tree
fancyRpartPlot(rpart_no_cor_fit$finalModel)
#storing results 
results <- results %>% add_row(Method = "Tuned CART",
                               Data = "Training w/o Corr Features", 
                               Accuracy = max(rpart_no_cor_fit$results$Accuracy))

#looking at var imp for highest accuracy model for non correlated data
varimp_knn <- varImp(knn_no_cor_param_norm)
impvars_knn <- varimp_knn$importance %>% add_rownames() %>% arrange(desc(Above_Avg)) %>% slice(1:10) %>% pull(rowname)
varimp_cart <- varImp(rpart_no_cor_fit)
impvars_cart <-  varimp_cart$importance %>% add_rownames() %>% arrange(desc(Overall)) %>% slice(1:10) %>% pull(rowname)
impvars <- data_frame(Method = "KNN", Imp_Features = impvars_knn)
impvars <- impvars %>% add_row(Method = "CART",
                               Imp_Features = impvars_cart)

#calculating predicted outcomes for validation group
y_hat_valid <- predict(knn_cor_param,validation_cor)
cf <- confusionMatrix(y_hat_valid,validation_cor$fct_slope)
cf$table
cf$byClass[,c("F1","Balanced Accuracy")]

#making validation dataset with slope added back in and whether guess was correct or not
validation_w_slope <- validation_cor %>% 
  add_column(y_hat_valid,reduced_slope[-edxIndex,]) %>% 
  mutate(correct = if_else(y_hat_valid == fct_slope,"Y","N")) %>% 
  mutate(correct = as.factor(correct))

#plotting distirbutions of slopes and correct guesses  
validation_w_slope %>% ggplot(aes(slope)) + 
  geom_histogram(aes(fill = correct)) +
  scale_y_continuous(trans = "log10") +
  geom_vline(xintercept =  mu, col = "red") +
  geom_text(aes(x=mu*1.025, label="Average for Above 0 slopes", y=100), 
            colour="red", angle=90)+
  ggtitle("Distribution of Slopes and Accuracy") +
  ylab("log_10(Counts)") + xlab("Slope of Log_10(Deaths) vs. Time")


#adding results
results <- results %>% add_row(Method = "YJ Tuned kNN",
                               Data = "Complete Validation", 
                               Accuracy =cf$overall["Accuracy"])



