# **********************************************************************************************
# # Installing required Package 
# **********************************************************************************************

#### Download the required Package

if(!require(tidyverse))    install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret))        install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggthemes))     install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(ggcorrplot))   install.packages("ggcorrplot", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(magrittr))     install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rpart))        install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot))   install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if(!require(neighbr))      install.packages("neighbr", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(formattable)) install.packages("formattable", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(cowplot)) install.packages("cowplot", repos = "http://cran.us.r-project.org")
 

#### Use the required library
library(tidyverse)
library(dplyr)
library(caret)
library(ggthemes)
library(ggcorrplot)
library(randomForest)
library(magrittr)
library(rpart)
library(rpart.plot)
library(neighbr)
library(kableExtra)
library(formattable)
library(gridExtra)
library(cowplot)
library(ROCR)
library(pROC)

# **********************************************************************************************
#### Set Local Variales 
# **********************************************************************************************

#### set_them has general properties of images and graphics
set_theme <- theme(plot.background = element_rect(fill="#F5FFFA",color = "darkblue"),
                   text = element_text(size=16), 
                   axis.title.x = element_text(size=16, color = "black"),
                   axis.title.y = element_text(size=16, color = "black"),
                   panel.border = element_rect(colour="black", linetype = "solid", fill=NA), 
                   plot.title = element_text(hjust = 0.5, size = 18), 
                   plot.caption = element_text(hjust = 0.5))

set_theme_cat <- theme(plot.background = element_rect(fill="#F5FFFA",color = "darkblue"),
             plot.title = element_text(size=20, hjust=.5),
             axis.title.x = element_text(size=18, color = "black"),
             axis.title.y = element_text(size=18, color = "black"),
             axis.text.x = element_text(size=16),
             axis.text.y = element_text(size=16),
             
             legend.text = element_text(size=16),
             legend.title = element_text(size=18))

# **********************************************************************************************
#### import and read the database
# **********************************************************************************************

# the github repo with the data set "telecom_users" is available here: https://github.com/aalastal/Telecom_Churn_project
# the file "telecom_users.csv" provided in the github repo must be included in the working (project) directory for the code below to run
# you can download the data base file "telecom_users.csv" from kaggle website from the following link: https://www.kaggle.com/radmirzosimov/telecom-users-dataset

# read in the Telecom dataset and save the data in telecom_data variable
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
telecom_data <- read_csv("telecom_users.csv", 
                     col_types = cols(Churn = col_factor(levels = c("No","Yes")), 
                                      Contract = col_factor(levels = c("One year","Two year", "Month-to-month")),
                                      Dependents = col_factor(levels = c("Yes","No")),
                                      DeviceProtection = col_factor(levels = c("Yes","No", "No internet service")),
                                      InternetService = col_factor(levels = c("No","DSL", "Fiber optic")),
                                      MultipleLines = col_factor(levels = c("Yes","No", "No phone service")),
                                      OnlineBackup = col_factor(levels = c("Yes","No", "No internet service")),
                                      OnlineSecurity = col_factor(levels = c("Yes","No", "No internet service")),
                                      PaperlessBilling = col_factor(levels = c("Yes","No")),
                                      Partner = col_factor(levels = c("Yes", "No")),
                                      PaymentMethod = col_factor(levels = c("Electronic check", "Mailed check", "Bank transfer (automatic)", "Credit card (automatic)")), 
                                      PhoneService = col_factor(levels = c("Yes", "No")),
                                      SeniorCitizen = col_factor(levels = c("0", "1")),
                                      StreamingMovies = col_factor(levels = c("Yes", "No", "No internet service")),
                                      StreamingTV = col_factor(levels = c("Yes","No", "No internet service")),
                                      TechSupport = col_factor(levels = c("Yes", "No", "No internet service")), 
                                      gender = col_factor(levels = c("Female", "Male"))))


#### Define Original Data
original_telecom_data <- telecom_data

####**** 1.2 Data Overview:
dim(telecom_data) ## return dataset dimention 5986 * 22

### summarize the Database
head_db <- telecom_data  %>% glimpse()


# **********************************************************************************************
#### 2. Data Analysis :(data cleaning, Data Overview,  data exploration and visualization)
# **********************************************************************************************

#####################################################################
####****************************** 2.1 Data Wrangling 

### Calculate the missing Data for each column
missing_table <- telecom_data %>% summarise_all(
    funs(sum(is.na(.)))) %>%
  gather(ColumnTitle, NAs)

### View the missing data table 
formattable(missing_table, align = c("c",rep("c", NCOL(missing_table) - 1)), list(
  `Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
  area(col = 2:2) ~ color_tile("#91CA97", "#00783e")))

##********************************************************
####### Figure 1: Visualizing Percentage of NAs in the columns

missing_data <- telecom_data %>% summarise_all(funs(sum(is.na(.))/n()))
missing_data <- gather(missing_data, key = "variables", value = "percent_missing")
ggplot(missing_data, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "orange", aes(color = I('white')), size = 0.3)+
  coord_flip()+ 
  ggtitle("Percentage of NAs in the columns ")+
  #labs(x="Percent_missing", y="Variables", caption = "Figure 1: Percentage of NAs in the columns in dataset")+
  labs(x="Percent_missing", y="Variables")+
  set_theme
ggsave("save_files/images/figure1.jpeg", width = 10, height = 8)


# ********************************************************************************************************
# Cleaning_data() 
#   
# Initial data pre-processing for the Churn dataframe, steps:
#     1 - Delete field customerID and ID as it is unique
#     2 - Removing rows with tenure less than 1 month to deal with missing values
#     3 - SeniorCitizen values {0,1} converted to {"Yes","No"} to match the other categorical fields format
#     4 - Change the Value to "No" for the following
#         * Value "No internet service" for the following column: OnlineSecurity, OnlineBackup, 
#                 DeviceProtection, TechSupport, streamingTV, streamingMovies
#         * Value "No phone service"  for column MultipleLines

# ********************************************************************************************************
 Cleaning_data <- function(df_Telecom){
  # remove the first and second coloum (X, CistumerId)
  if (dim(df_Telecom)[2] > 20)
  {
    df_Telecom <- subset(df_Telecom, select = -c(1, 2))
  }
  
  df_Telecom <- subset(df_Telecom,tenure > 0) # Removing rows with tenure less than 1 month to deal with missing values
  
  # Ordinal discretisation for field SeniorCitizen
  df_Telecom$SeniorCitizen <- stringr::str_replace_all(df_Telecom$SeniorCitizen, setNames(c("No", "Yes"), c("0", "1")))
  
  # Change the Value "No internet service" to "No" for: OnlineSecurity, OnlineBackup, 
  #          DeviceProtection, TechSupport, streamingTV, streamingMovies
  # NOTE: All these columns are dependent on InternetService=Yes

  df_Telecom$OnlineSecurity <- stringr::str_replace_all(df_Telecom$OnlineSecurity, setNames(c("No"), c("No internet service")))
  df_Telecom$OnlineBackup <- stringr::str_replace_all(df_Telecom$OnlineBackup, setNames(c("No"), c("No internet service")))
  df_Telecom$DeviceProtection <- stringr::str_replace_all(df_Telecom$DeviceProtection, setNames(c("No"), c("No internet service")))
  df_Telecom$TechSupport <- stringr::str_replace_all(df_Telecom$TechSupport, setNames(c("No"), c("No internet service")))
  df_Telecom$StreamingTV <- stringr::str_replace_all(df_Telecom$StreamingTV, setNames(c("No"), c("No internet service")))
  df_Telecom$StreamingMovies <- stringr::str_replace_all(df_Telecom$StreamingMovies, setNames(c("No"), c("No internet service")))
  
  # Change the value "No phone service" to "No" for column MultipleLines
  df_Telecom$MultipleLines <- stringr::str_replace_all(df_Telecom$MultipleLines, setNames(c("No"), c("No phone service")))
  
  if (any (is.na(df_Telecom))) df_Telecom <- drop_na(df_Telecom) # delete NA if any
  return(df_Telecom)
} # END generalPreprocessingFunction


### Apply Cleaning function
telecom_data <- Cleaning_data(telecom_data)

# ********************************************************************************************************
# calStats() 
#
# Calculate several statistical parameters (standard deviation, percentages..) for the categorical fields of the Churn dataframe 
# ********************************************************************************************************

calStats<-function(df){
  
# Deleting the numeric features  
numfields <- select_if(df, is.numeric) %>%  names()
cols <- which(names(df) %in% numfields)
df[, cols]<- NULL

variables <- colnames(df)
# create data frame for results
#df_Statistics <- as.data.frame(c(1:7)) 
df_Statistics <- as.data.frame(c(1:6)) 
df_Statistics <- t(df_Statistics)
#colnames(df_Statistics) <- c("Level", "Variable" ,"Number", "Churn percentage", "Standard Deviation", "Number (No Churn)", "Number (Churn)") 
colnames(df_Statistics) <- c("Variable" ,"Number", "Churn percentage", "Standard Deviation", "Number (No Churn)", "Number (Churn)") 
df_Statistics <- df_Statistics[-1,]
df$Churn_num <- ifelse(df$Churn=='No',0,1)

# Calculating the actual stats
m0 <- df[df[, "Churn"] == "No",]
m1 <- df[df[, "Churn"] == "Yes",]

for (n in 1:ncol(df)) {
  DFresults <- data.frame(
    variable=variables[n],
    #level=levels(factor(df[,n])), 
    number=tapply(df$Churn_num, df[,n], length),
    Churn_percentage=round(tapply(df$Churn_num, df[,n], mean)*100,2), 
    Standard_Deviation=tapply(df$Churn_num, df[,n], sd),
    Num_NoChurn=tapply(m0$Churn_num, m0[,n], length),
    Num_Churn=tapply(m1$Churn_num, m1[,n], length)
  )
  df_Statistics<-  rbind(df_Statistics,DFresults)
  
}
return(df_Statistics)
} # END  calStats


### Aplly statistics function to get the statistic for each categorical feacher in database
df_statistics<-calStats(telecom_data)
df_statistics<-df_statistics[1:((dim(df_statistics)[1])-4),]

##********************************************************
####### Table1: Calculated statistics for the churn dataset

formattable(df_statistics, align = c("c",rep("c", NCOL(df_statistics) - 1)), list(
  `Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
  area(col = 2:6) ~ color_tile("#91CA97", "#71CA97")))

#####################################################################
####****************************** 2.2	Target variable

##********************************************************
####### Figure 2: Customer churn Distribution

telecom_data %>%
  group_by(Churn) %>%
  summarize(count = n()) %>%
  mutate(count2 = count, percentage = round(count / sum(count), 3)) %>%
  mutate(label_text = str_glue("{scales::percent(percentage)}"))%>%
  ggplot(aes(x = Churn, y = count2)) +
  geom_col(fill = c( "#71CA97", "#00783e" ), width = 0.5) +
  geom_label(aes(label=label_text)) +
  coord_flip()+ 
  ggtitle("Customer churn characteristics ")+
  #labs(x="Churn", y="count", caption = "Figure 2: Customer churn Distribution in dataset")+
  labs(x="Churn", y="count")+
  set_theme +
  theme(legend.position = "none")
ggsave("save_files/images/figure2.jpeg", width = 10, height = 8)

#####################################################################
####****************************** 2.3	Continuous features

# There are three numerical columns: tenure, monthly charges and total charges

##********************************************************
####### Figure 3: distribution of Monthly Charges in telecom dataset

  telecom_data %>%
  ggplot(aes(x= MonthlyCharges, color = Churn)) +
  geom_freqpoly( binwidth = 5, size = 1)+ 
  theme_stata()+
  scale_color_brewer(palette = "Set2") +
  ggtitle("distribution of Monthly Charges ")+
  #labs(x="Monthly Charges", y="Count", caption = "Figure 2: distribution of Monthly Charges in telecom dataset") +
    labs(x="Monthly Charges", y="Count") +
  set_theme
ggsave("save_files/images/figure3.jpeg", width = 10, height = 8)

##********************************************************
####### Figure 4: distribution of Total Charges in telecom dataset

telecom_data %>%
  ggplot(aes(x= TotalCharges, color = Churn)) +
  geom_freqpoly( binwidth = 100, size = 1) +
  theme_stata()+ 
  scale_color_brewer("Species", palette = "Set2") +
  ggtitle("distribution of Total Charges ")+
  #labs(x="Total Charges", y="Count", caption = "Figure 4: distribution of Total Charges in telecom dataset") +
  labs(x="Total Charges", y="Count") +
  set_theme
ggsave("save_files/images/figure4.jpeg", width = 10, height = 8)

##********************************************************
####### Figure 5: distribution of tenure in telecom dataset


telecom_data %>%
  ggplot(aes(x= tenure, color = Churn)) +
  geom_freqpoly( binwidth = 5, size = 1) +
  theme_stata()+
  scale_color_brewer("Species", palette = "Set2") +
  ggtitle("distribution of tenure ")+
  #labs(x="Monthly Charges", y="Count", caption = "Figure 5: distribution of tenure in telecom dataset") +
  labs(x="Monthly Charges", y="Count") +
  set_theme
ggsave("save_files/images/figure5.jpeg", width = 10, height = 8)


#####################################################################
####****************************** 2.4	Categorical features

#This dataset has 16 categorical features

####### General code for all Categorical Feacher
plot_categorical <- function(data, target, feachers){
  target <- sym(target) #Converting the string to a column reference
  i <-1 
  plt_matrix <- list()
  for(column in feachers){
    col <- sym(column) 
    temp <- data %>% group_by(!!sym(col),!!sym(target)) %>% 
      summarize(count = n()) %>% 
      mutate(prop = round(count/sum(count),2)) %>%
      ungroup()%>%
      mutate(label_text = str_glue("{scales::percent(prop)}"))
    
    
    options(repr.plot.width=20, repr.plot.height=15) 
    
    plt_matrix[[i]]<-ggplot(data= temp, aes(x=!!sym(col), y=count,fill =!!sym(target))) + 
      geom_bar(stat="identity",alpha=1,color = "grey25") +
      scale_fill_manual("Churn", values = c("Yes" = "#00783e", "No" = "#abd5ab"), )+
      geom_label(aes(label=label_text)) +
      #scale_y_continuous(labels=scales::percent_format()) +
      xlab(column) +
      ylab("Count") +
      ggtitle(paste("Churn percentage across \n ",column)) +
      theme_stata()+
      theme(axis.text.x = element_text(angle=40), plot.title = element_text(hjust = 0.5, size = 14))+
      set_theme_cat
    i<-i+1
  }
  
  plot_grid(plotlist = plt_matrix,ncol=2)
}

##********************************************************
####### 2.4.1 Figure 6: Distribution of churn percentage across gender and senior citizen

  plot_categorical(telecom_data,'Churn',c('gender','SeniorCitizen'))+ set_theme
  ggsave("save_files/images/figure6.jpeg", width = 10, height = 8)

##********************************************************
####### 2.4.2 Figure 7: Distribution of churn percentage across Partner and Dependents

  plot_categorical(telecom_data,'Churn',c('Partner','Dependents'))+ set_theme
  ggsave("save_files/images/figure7.jpeg", width = 10, height = 8)

##********************************************************
####### 2.4.3 Figure 8: Distribution of churn percentage across PhoneService and InternetService

  plot_categorical(telecom_data,'Churn',c('MultipleLines','InternetService'))+ set_theme
  ggsave("save_files/images/figure8.jpeg", width = 10, height = 8)

#####################################################################
####****************************** 2.4.4: Distribution of churn percentage across other services(OnlineSecurity, OnlineBackup, DeviceProtection, TechSupport, StreamingTV, StreamingMovies)
## OnlineSecurity, OnlineBackup


##********************************************************
## Figure 9: Distribution of churn percentage across OnlineSecurity and OnlineBackup

  plot_categorical(telecom_data,'Churn',c('OnlineSecurity','OnlineBackup'))+ set_theme
  ggsave("save_files/images/figure9.jpeg", width = 10, height = 8)

##********************************************************
## Figure 10: Distribution of churn percentage across DeviceProtection and TechSupport

  plot_categorical(telecom_data,'Churn',c('DeviceProtection', 'TechSupport'))+ set_theme
  ggsave("save_files/images/figure10.jpeg", width = 10, height = 8)

##********************************************************
## Figure 11: Distribution of churn percentage across StreamingTV and StreamingMovies

  plot_categorical(telecom_data,'Churn',c('StreamingTV', 'StreamingMovies'))+ set_theme
  ggsave("save_files/images/figure11.jpeg", width = 10, height = 8)


#####################################################################
####****************************** 2.4.5 Contract and Contract
  
##********************************************************
####### Figure 12: Distribution of churn percentage across Contract and PaymentMethod

  plot_categorical(telecom_data,'Churn',c('Contract','PaymentMethod'))+ set_theme
  ggsave("save_files/images/figure12.jpeg", width = 10, height = 8)
  
  

################################################################################################
# **********************************************************************************************
### 3. Modeling and Evaluation approach :(Logistic Regression,KNN, Decision Tree, Random Forest)
# **********************************************************************************************

# **********************************************************************************************
# Create train and test sets (80%) from telecom_data train is used to train various models and test set (20%) is used to assess their performances

set.seed(4, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(4)`

test_index <- createDataPartition(telecom_data$Churn, times = 1, p = 0.2, list = FALSE)
train_data <- telecom_data %>% slice(-test_index)
test_data <- telecom_data %>% slice(test_index)


# ***************************************** Some required function 
# **********************************************************************************************
# tab_int2double()
# 
# Function to convert an integer table to double 
#
# INPUT:   table - table of integer numbers
# OUTPUT:  table - table of double numbers
#
# **********************************************************************************************

tab_int2double <- function(intTab){
  doubleTab <- intTab
  for(i in nrow(intTab)){
    for (j in ncol(intTab)) {
      doubleTab[i,j] <- as.double(intTab[i,j])
    }
  }
  return(doubleTab)
} # END tab_int2double()


# **********************************************************************************************
# NcalcMeasures() 
#
#  Evaluation measures for a confusion matrix
#
# INPUT: numeric TP, FN, FP, TN
# OUTPUT: A list with the following entries:
#        TP - int - True Positive records
#        FP - int - False Positive records
#        TN - int - True Negative records
#        FN - int - False Negative records
#        accuracy - float - accuracy measure
#        pgood - float - precision for "good" (values are 1) measure
#        pbad - float - precision for "bad" (values are 1) measure
#        FPR - float - FPR measure
#        TPR - float - FPR measure
#        MCC - float - Matthew's Correlation Coeficient
#
# **********************************************************************************************
Calc_Measures<-function(TP,FP,TN,FN){
  
  NcalcAccuracy<-function(TP,FP,TN,FN){return(100.0*((TP+TN)/(TP+FP+FN+TN)))}
  NcalcPgood<-function(TP,FP,TN,FN){return(100.0*(TP/(TP+FP)))}
  NcalcPbad<-function(TP,FP,TN,FN){return(100.0*(TN/(FN+TN)))}
  NcalcFPR<-function(TP,FP,TN,FN){return(100.0*(FP/(FP+TN)))}
  NcalcTPR<-function(TP,FP,TN,FN){return(100.0*(TP/(TP+FN)))}
  NcalcTNR<-function(TP,FP,TN,FN){return(100.0*(TN/(TN+FP)))}
  NcalcMCC<-function(TP,FP,TN,FN){return( ((TP*TN)-(FP*FN))/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN)))}
  
  retList<-list(  "TP"=TP,
                  "FP"=FP,
                  "TN"=TN,
                  "FN"=FN,
                  "Accuracy"=NcalcAccuracy(TP,FP,TN,FN),
                  "TPR"=NcalcTPR(TP,FP,TN,FN),
                  "TNR"=NcalcTNR(TP,FP,TN,FN),
                  "NPV"=NcalcPgood(TP,FP,TN,FN),
                  "PPV"=NcalcPbad(TP,FP,TN,FN),
                  "FPR"=NcalcFPR(TP,FP,TN,FN),
                  "MCC"=NcalcMCC(TP,FP,TN,FN)
  )
  return(retList)
} # END Calc_Measures()

# **********************************************************************************************
# ROC_graph() 
#
#  This is a ROC graph
#
# INPUT:        Frame - dataset to create model
#               Fame - dataset to test model
# OUTPUT :      Float - calculated thresholkd from ROC
#
# **********************************************************************************************

ROC_graph<-function(expected,predicted, title_txt){
  
  roc_title = title_txt 
  rr<-roc(expected,predicted,
          plot=TRUE, auc.polygon=TRUE,
          percent=TRUE, grid=TRUE,print.auc=TRUE,
          main=roc_title)
  
  #Selects the "best" threshold for lowest FPR and highest TPR
  analysis<-coords(rr, x="best",best.method="closest.topleft",
                   ret=c("threshold", "specificity",
                         "sensitivity","accuracy",
                         "tn", "tp", "fn", "fp",
                         "npv","ppv"))
  
  fpr<-round(100.0-analysis["specificity"],digits=2L)
  threshold<-analysis["threshold"]
  
  #Add crosshairs to the graph
  abline(h=analysis["sensitivity"],col="red",lty=3,lwd=1)
  abline(v=analysis["specificity"],col="red",lty=3,lwd=1)
  
  #Annote with text
  text(x=analysis["specificity"],y=analysis["sensitivity"], adj = c(-0.2,2),cex=1,
       col="#00783e",
       paste("Threshold: ",round(threshold,digits=4L),
             "% FPR: ",fpr,"%",sep=""))
  
  return(threshold)
}


# **********************************************************************************************
# Best_cutoff() 
#
#  Best cutoff function used to return best cut of point to get perfect metric
#
# INPUT:        prediction model and cutoff points
# 
# OUTPUT :      cutoff which will give maximum accuracy, sensitivity and specificity.
#
# **********************************************************************************************
Best_cutoff <- function(predection,cutoff) 
{
  predicted_churn <- factor(ifelse(predection >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_churn, test_data$Churn)
  accuray <- conf$overall[1]
  sensitivity <- conf$byClass[1]
  specificity <- conf$byClass[2]
  Difference_Abs = abs(sensitivity - specificity)
  mean_val <- mean(accuray , sensitivity, specificity)
  out <- t(as.matrix(c(cutoff, sensitivity, specificity, accuray, Difference_Abs, mean_val))) 
  colnames(out) <- c("cutoff","sensitivity", "specificity", "accuracy", "Difference_Abs", "mean_val")
  return(out)
}

# **********************************************************************************************
  ##### 3.3 The Logistic Regression Model
# **********************************************************************************************

set.seed(8, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(8)`

### Use gml Model use the entire train data set and evaluate the model against the test data set
  model_glm <- glm(as.numeric(Churn=="Yes")~., family = "binomial", data = train_data)

### Print the result of Logistic regression model
  model_glm
  

### Define predictions for the glm model
  preds_glm <- predict(model_glm, test_data, type = "response")


### Define cutoff matrix of metrix for 100 cutoff point to select the cutoff point that return optimal metrics of accuracy, sensitivity, specificity  
  cutoffs = seq(0.01,0.80,length=100) # trying 100 different cutoff within the range of 0.01 and 0.8
  cutoff_glm = matrix(0,100,6) # initializing a matrix with 100 row and 6 columns one row for each cutoff results

  for(i in 1:100)
  {
    cutoff_glm[i,] = Best_cutoff(preds_glm,cutoffs[i])
  }

  cutoff_glm<- data.frame(cutoff_glm)
  colnames(cutoff_glm) <- c("cutoff","sensitivity", "specificity", "accuracy", "Difference_Abs" , "mean_val")

### Define optimal cutoff point
  opt_co_glm <-
    cutoff_glm  %>% filter(Difference_Abs == min(Difference_Abs)) %>%
    select(cutoff) %>% as.numeric()
  
  paste0("Optimal cutoff point for LG model = ", toString(round(opt_co_glm, 2)))
  

##********************************************************
####### Figure 13: Accuracy, Sensitivity and Specificity for various cutoffs by Logistic Regression Model
  ggplot(data = cutoff_glm) +
    geom_line(aes(x = cutoff, y = accuracy, color ="#00783e"), size = 1)+
    geom_line(aes(x = cutoff, y = sensitivity, color = "red"), size = 1) +
    geom_line(aes(x = cutoff, y = specificity, color ="blue"), size = 1)+
    labs(x = "cutoff", y ="value") +
    scale_y_continuous(labels = scales::percent) +
    scale_color_manual(name = "Model evaluation", values = c("#00783e","red", "blue"), labels = c("Accuracy", "Sensitivity", "Specificity"))+
    geom_vline(aes(xintercept = opt_co_glm))+
    geom_text(aes(x= 0.55, y= 0.75),label=paste0("opt_cutoff = ", toString(round(opt_co_glm, 2))),hjust=1, size=4)+
    ggtitle("Accuracy, Sensitivity and Specificity for various cutoffs")+ set_theme

  ggsave("save_files/images/figure13.jpeg", width = 10, height = 8)
  
### convert probabilities into factors (Yes or No) using optimal cutoff point
  preds_glm <- ifelse(preds_glm > opt_co_glm, "Yes","No") %>% factor(levels = c("Yes","No"))

### make confusion matrix for logistic regression model
  cm_glm <- confusionMatrix(preds_glm, test_data$Churn)

### Print the confusion matrix for logistic regression model
  cm_glm
  
### Convert table of matrix table to double
  matrix_glm <- tab_int2double(cm_glm$table)
  
  measures_glm <- Calc_Measures(matrix_glm[1,1],matrix_glm[1,2],
                          matrix_glm[2,2],matrix_glm[2,1])
  
##********************************************************
####### Table 2: Performance Metrics for Logistic Regression Model
  
  formattable(data.frame(measures_glm), align = c("c",rep("c", NCOL(data.frame(measures_glm)) - 1)), list(
    `Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
    area(col = 1:11) ~ color_tile("#9ccc9c", "#9ccc9c")))
  
### Taking out the predictions and actuals to make the ROC curve
  expected_glm  <- ifelse(pull(test_data, Churn) == "Yes", 1, 0)
  predicted_glm <- ifelse( preds_glm == "Yes", 1, 0)

##********************************************************
####### Figure 14: ROC Chart for Logistic Regression model
  
  threshold_glm<- ROC_graph(expected_glm, predicted_glm, "ROC Chart for Logistic Regression model")
  #ggsave("save_files/images/figure14.jpeg", width = 10, height = 8)


######  End Logistic Regression


# **********************************************************************************************
  ##### 3.4 The KNN Model
# **********************************************************************************************

set.seed(16, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(32)`

### Use train function in the caret package to define an optimal K parameter using 10-fold cross validation
  control <- trainControl(method = "repeatedcv", number = 10, p = .9)
  train_knn_cv <- train(Churn ~ ., method = "knn", 
                      data = train_data,
                      tuneGrid = data.frame(k = seq(9, 71, 2)),
                      trControl = control)
##********************************************************
####### Figure 15: Accuracy for each number of neighbors selected predictors
  
  ggplot(train_knn_cv, highlight = TRUE)+ ggtitle("Accuracy for each number of neighbors")+ set_theme
  ggsave("save_files/images/figure15.jpeg", width = 10, height = 8)

### define the optimal K
  opt_k_knn <- train_knn_cv$bestTune  %>% as.numeric()
  paste0("Optimal K parameter = ", toString(round(opt_k_knn, 2)))
  
### redefine the model using the train_data and optimal k
  model_knn <- knn3(Churn ~ ., data = train_data, k = opt_k_knn)

### Define predictions for the knn model
  preds_knn <- predict(model_knn, newdata = test_data, type = "prob")

### Define cutoff matrix of metric for 100 cutoff point to select the cutoff point that return optimal metrics of accuracy, sensitivity, specificity
  
  cutoffs = seq(0.01,0.80,length=100) # trying 100 different cutoff within the range of 0.01 and 0.8
  cutoff_knn = matrix(0,100,6) # initializing a matrix with 100 row and 6 columns one row for each cutoff results 

  for(i in 1:100)
  {
    cutoff_knn[i,] = Best_cutoff(preds_knn[,2], cutoffs[i]) # putting the results of each cutoff in a single row in the matrix
  } 

  cutoff_knn<- data.frame(cutoff_knn)
  colnames(cutoff_knn) <- c("cutoff","sensitivity", "specificity", "accuracy", "Difference_Abs" , "mean_val")
  
### Define obtimal cut off point
  opt_co_knn <-
    cutoff_knn  %>% filter(Difference_Abs == min(Difference_Abs)) %>%
    select(cutoff) %>% top_n(-1) %>% as.numeric()
  
  paste0("Optimal cutoff for KNN model = ", toString(round(opt_co_knn, 2)))
  

##********************************************************
####### Figure 16: Accuracy, Sensitivity and Specificity for various cutoffs in knn model
  
  ggplot(data = cutoff_knn) +
    geom_line(aes(x = cutoff, y = accuracy, color ="#00783e"), size = 1)+
    geom_line(aes(x = cutoff, y = sensitivity, color = "red"), size = 1) +
    geom_line(aes(x = cutoff, y = specificity, color ="blue"), size = 1)+
    labs(x = "cutoffs", y ="value") +
    scale_y_continuous(labels = scales::percent) +
    scale_color_manual(name = "Model evaluation", values = c("#00783e","red", "blue"), labels = c("Accuracy", "Sensitivity", "Specificity"))+
    geom_vline(aes(xintercept = opt_co_knn))+
    geom_text(aes(x= 0.35, y= 0.85),label=paste0("opt_cutoff = ", toString(round(opt_co_knn, 2))),hjust=1, size=4)+
    ggtitle("Accuracy, Sensitivity and Specificity for various cutoffs")+ set_theme

  ggsave("save_files/images/figure16.jpeg", width = 10, height = 8)

### convert probabilities into factors (Yes or No) using optimal cutoff point
  preds_knn <- preds_knn %>%
    as_tibble %$%
    ifelse(Yes > opt_co_knn, "Yes", "No") %>% 
    factor(levels = c("Yes", "No"))
  
### make confusion matrix for KNN model
  cm_knn <- confusionMatrix(preds_knn, test_data$Churn)
  
### Print confusion matrix for KNN model
  cm_knn

### Convert table of matrix table to double
  matrix_knn <- tab_int2double(cm_knn$table)
  measures_knn <- Calc_Measures(matrix_knn[1,1],matrix_knn[1,2],
                              matrix_knn[2,2],matrix_knn[2,1])
  
##********************************************************
####### Table 3: Performance Metrics for KNN Model
  
  formattable(data.frame(measures_knn), align = c("c",rep("c", NCOL(data.frame(measures_knn)) - 1)), list(
    `Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
    area(col = 1:11) ~ color_tile("#9ccc9c", "#9ccc9c")))

### Taking out the predictions and actuals to make the ROC curve
  expected_knn  <- ifelse(pull(test_data, Churn) == "Yes", 1, 0)
  predicted_knn <- ifelse( preds_knn == "Yes", 1, 0)

##********************************************************
####### Figure 17: ROC Chart for KNN model
  
  threshold_knn<- ROC_graph(expected_knn, predicted_knn, "ROC Chart for KNN model")
  #ggsave("save_files/images/figure17.jpeg", width = 10, height = 8)

###### End KNN Model
  

# **********************************************************************************************
  ##### 3.5 The Decision Tree Model
# **********************************************************************************************

  set.seed(32, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(32)`

### Use train function in the caret package to select an optimal complexity parameter (cp) using 20 bootstrap samples with replacement and 10-fold cross validation
  train_dt_cv <- train(Churn ~ .,
                         method = "rpart",
                         tuneGrid = data.frame(cp = seq(0, 0.05, len = 20)),
                         trControl = trainControl(method="repeatedcv", number=10, repeats=5, p = .9),
                         data = telecom_data)


##********************************************************
####### Figure 18: Accuracy for each number of complexity selected predictors
  
  ggplot(train_dt_cv, highlight = TRUE)+ ggtitle("Accuracy for each number of complexity selected predictors")+ set_theme
  ggsave("save_files/images/figure18.jpeg", width = 10, height = 8)

### define the optimal complexity parameter "cp"
  opt_cp_dt <- train_dt_cv$bestTune %>% as.numeric()
  paste0("Optimal cp parameter = ", toString(round(opt_cp_dt, 2)))


### redefine the model using the train_data and optimal cp
  model_dt <- rpart(Churn~., cp = opt_cp_dt, data = train_data ,method = "class")
  
##********************************************************
####### Figure 19: Decision Tree Plot
  
   rpart.plot(model_dt, type = 4, clip.right.labs = FALSE, branch = .4) + title("Decision Tree Plot")
   #ggsave("save_files/images/figure19.jpeg", width = 10, height = 8)

### Define predictions for the Dtree model
  preds_dt <- predict(model_dt, newdata = test_data, type = "prob")


### Define cutoff matrix of metrix for 100 cutoff point to select the cutoff point that return optimal metrics of accuracy, sensitivity, specificity
  cutoffs = seq(0.01,0.80,length=100) # trying 100 different cutoff within the range of 0.01 and 0.8
  cutoff_dt = matrix(0,100,6) # initializing a matrix with 100 row and 6 columns one row for each cutoff results 
  
  for(i in 1:100)
  {
    cutoff_dt[i,] = Best_cutoff(preds_dt[,2], cutoffs[i]) # putting the results of each cutoff in a single row in the matrix
  } 
  
  cutoff_dt<- data.frame(cutoff_dt)
  colnames(cutoff_dt) <- c("cutoff","sensitivity", "specificity", "accuracy", "Difference_Abs" , "mean_val")

### Define optimal cut off point
  opt_co_dt <-
    cutoff_dt  %>% filter(Difference_Abs == min(Difference_Abs)) %>%
    select(cutoff) %>% top_n(-1) %>% as.numeric()
  
  paste0("Optimal cutoff for DT model = ", toString(round(opt_co_dt, 2)))

##********************************************************
####### Figure 20: Accuracy, Sensitivity and Specificity for various cutoffs
  
  ggplot(data = cutoff_dt) +
    geom_line(aes(x = cutoff, y = accuracy, color ="#00783e"), size = 1)+
    geom_line(aes(x = cutoff, y = sensitivity, color = "red"), size = 1) +
    geom_line(aes(x = cutoff, y = specificity, color ="blue"), size = 1)+
    labs(x = "cutoffs", y ="value") +
    scale_y_continuous(labels = scales::percent) +
    scale_color_manual(name = "Model evaluation", values = c("#00783e","red", "blue"), labels = c("Accuracy", "Sensitivity", "Specificity"))+
    geom_vline(aes(xintercept = opt_co_dt))+
    geom_text(aes(x= 0.35, y= 0.72),label=paste0("opt_cutoff = ", toString(round(opt_co_dt, 2))),hjust=1, size=4)+
    ggtitle("Accuracy, Sensitivity and Specificity for various cutoffs")+ set_theme

  ggsave("save_files/images/figure20.jpeg", width = 10, height = 8)

### convert probabilities into factors (Yes or No) using optimal cutoff point
  preds_dt <- preds_dt %>%
    as_tibble %$%
    ifelse(Yes > opt_co_dt, "Yes", "No") %>% 
    factor(levels = c("Yes", "No"))

### make confusion matrix for Decision tree model
  cm_dt <- confusionMatrix(preds_dt, test_data$Churn, positive = "No")
  
### Print confusion matrix for Decision tree model
  cm_dt

### Convert table of matrix table to double
  matrix_dt <- tab_int2double(cm_dt$table)
  measures_dt <- Calc_Measures(matrix_dt[1,1],matrix_dt[1,2],
                             matrix_dt[2,2],matrix_dt[2,1])

##********************************************************
####### Table 4: Performance Metrics for Decision Tree Model
  
  formattable(data.frame(measures_dt), align = c("c",rep("c", NCOL(data.frame(measures_dt)) - 1)), list(
    `Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
    area(col = 1:11) ~ color_tile("#9ccc9c", "#9ccc9c")))


### Taking out the predictions and actuals to make the ROC curve
  expected_dt  <- ifelse(pull(test_data, Churn) == "Yes", 1, 0)
  predicted_dt <- ifelse( preds_dt == "Yes", 1, 0)

##********************************************************
####### Figure 21: ROC Chart for Decision Tree model
  
  threshold_dt<- ROC_graph(expected_dt, predicted_dt, "ROC Chart for Decision tree model")
  ggsave("save_files/images/figure21.jpeg", width = 10, height = 8)

###### End Decision Tree Model

  
# **********************************************************************************************
  ##### 3.6 The Random Forest Model
# ********************************************************************************************** 
set.seed(64, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(64)`

### Use train function in the caret package to select an optimal mtry parameter using 10-fold cross validation
## train function for rf take 8 minuets 
  control <- trainControl(method = "repeatedcv", number = 10, p = .9)
  train_rf_cv <- train(Churn~.,
                  method = "rf",
                  tuneGrid = data.frame(mtry = 3:11),
                  cutoff = c(opt_co_dt,1-opt_co_dt),
                  data = train_data,
                  trControl = control)

### define the optimal mtry
  opt_mtry_rf <- train_rf_cv$bestTune %>% as.numeric()
  paste0("Optimal mtry parameter = ", toString(round(best_mtry, 2)))


##********************************************************
####### Figure 22: Accuracy for each number of randomly selected predictors
  
  ggplot(train_rf_cv, highlight = TRUE) +
    scale_x_discrete(limits = 2:12) +
    ggtitle("Accuracy for each number of randomly selected predictors")+ set_theme
  
  ggsave("save_files/images/figure22.jpeg", width = 10, height = 8)

### redefine the model using the train_data and optimal mtry
  model_rf <- randomForest(Churn ~., data = train_data, mtry = best_mtry, importance = TRUE, ntree=300, do.trace=FALSE)
  
##********************************************************
####### Figure 23: Random Forest  Model with decreasing OOB error as Number of Trees increases
  
  plot(model_rf , main = "Error by number of trees")
  ggsave("save_files/images/figure23.jpeg", width = 10, height = 8)
  
##********************************************************
####### Figure 24:The importance Variables Plot
  
  imp_RF <- importance(model_rf)
  imp_DF <- data.frame(Variables = row.names(imp_RF), MeanDecreaseAccuracy = imp_RF[,3])
  imp_DF <- imp_DF[order(imp_DF$MeanDecreaseAccuracy, decreasing = TRUE),]
  
  ggplot(imp_DF, aes(x=reorder(Variables, MeanDecreaseAccuracy), y=MeanDecreaseAccuracy, fill=MeanDecreaseAccuracy)) +
    geom_bar(stat = 'identity', color = "#2b5329") + 
    labs(x = 'Variables', y= 'Mean Decrease Accuracy') +
    scale_fill_distiller(palette = "green")+
    set_theme + coord_flip() + 
    theme(legend.position="none")
  
  ggsave("save_files/images/figure24.jpeg", width = 10, height = 8)

### Define predictions for the rf model
  preds_rf <- predict(model_rf, test_data, type = "prob") # Calclating the prediction metrics on test data
  
  ### Define cutoff matrix of metrix for 100 cutoff point to select the cutoff point that return optimal metrics of accuracy, sensitivity, specificity
  cutoffs = seq(0.01,0.80,length=100) # trying 100 different cutoff within the range of 0.01 and 0.8
  cutoff_rf = matrix(0,100,6) # initializing a matrix with 100 row and 6 columns one row for each cutoff results 
  
  for(i in 1:100)
  {
    cutoff_rf[i,] = Best_cutoff(preds_rf[,2], cutoffs[i]) # putting the results of each cutoff in a single row in the matrix
  } 
  
  cutoff_rf<- data.frame(cutoff_rf)
  colnames(cutoff_rf) <- c("cutoff","sensitivity", "specificity", "accuracy", "Difference_Abs" , "mean_val")
  cutoff_rf
  
  ### Define optimal cut off point
  opt_co_rf <-
    cutoff_rf  %>% filter(Difference_Abs == min(Difference_Abs)) %>%
    select(cutoff) %>% top_n(-1) %>% as.numeric()
  
  paste0("Optimal cutoff for DT model = ", toString(round(opt_co_rf, 2)))
  
##********************************************************
####### Figure 25: Accuracy, Sensitivity and Specificity for various cutoffs
  
  ggplot(data = cutoff_rf) +
    geom_line(aes(x = cutoff, y = accuracy, color ="#00783e"), size = 1)+
    geom_line(aes(x = cutoff, y = sensitivity, color = "red"), size = 1) +
    geom_line(aes(x = cutoff, y = specificity, color ="blue"), size = 1)+
    labs(x = "cutoff", y ="value") +
    scale_y_continuous(labels = scales::percent) +
    scale_color_manual(name = "Model evaluation", values = c("#00783e","red", "blue"), labels = c("Accuracy", "Sensitivity", "Specificity"))+
    geom_vline(aes(xintercept = opt_co_rf))+
    geom_text(aes(x= 0.33, y= 0.75),label=paste0("opt_cutoff = ", toString(round(opt_co_rf, 2))),hjust=1, size=4)+
    ggtitle("Accuracy, Sensitivity and Specificity for various cut off")+ set_theme
  
  ggsave("save_files/images/figure25.jpeg", width = 10, height = 8)

### convert probabilities into factors (Yes or No) using optimal cutoff point 
   preds_rf <- preds_rf %>%
    as_tibble %$%
    ifelse(Yes > opt_co_rf, "Yes", "No") %>% 
    factor(levels = c("Yes", "No"))
  
### make confusion matrix for Random Forest model
  cm_rf <- confusionMatrix(preds_rf, test_data$Churn)
  
### print confusion matrix for Random Forest model
  cm_rf

### Convert table of matrix table to double
  matrix_rf <- tab_int2double(cm_rf$table)
  measures_rf <- Calc_Measures(matrix_rf[2,2],matrix_rf[1,2],
                               matrix_rf[2,1],matrix_rf[1,1])

##********************************************************
#######  Table 5: Performance Metrics for Random Forest Model
  
formattable(data.frame(measures_rf), align = c("c",rep("c", NCOL(data.frame(measures_rf)) - 1)), list(
    `Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
    area(col = 1:11) ~ color_tile("#9ccc9c", "#9ccc9c")))

### Taking out the predictions and actuals to make the ROC curve
  expected_rf  <- ifelse(pull(test_data, Churn) == "Yes", 1, 0)
  predicted_rf <- ifelse( preds_rf == "Yes", 1, 0)

##********************************************************
####### Figure 26: ROC Chart for Random Fores model
  
  threshold_rf<- ROC_graph(expected_rf, predicted_rf, "ROC Chart for Random Fores model")
  #ggsave("save_files/images/figure26.jpeg", width = 10, height = 8)

###### End random forest model
  
################################################################################################
# **********************************************************************************************
### 4. Result
# ********************************************************************************************** 
  
#### Define Performance Metrics of the Four Models
  
  pm_glm <- data.frame(measures_glm)
  pm_knn <- data.frame(measures_knn)
  pm_dt <- data.frame(measures_dt)
  pm_rf <- data.frame(measures_rf)
  ML_names <- data.frame(Models = c("GLM", "KNN", "DT", "RF"))
  pm_all_methods <- bind_rows(pm_glm, pm_knn, pm_dt, pm_rf)
  
  summary_pm_all <- bind_cols(ML_names, pm_all_methods)
  
##********************************************************
####### Table 6: Performance Metrics of the Four Models
  
  formattable(summary_pm_all, align = c("l",rep("r", NCOL(summary_pm_all) - 1)), 
              list(`Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
                   area(col = 6:11) ~ function(x) percent(x / 100, digits = 0),
                   area(col = 2:12) ~ color_tile("#DeF7E9", "#71CA97")))
  
##********************************************************
####### Figure 27: ROC analysis for models: Logistic, KNN, Random Forest, Decision Tree
  
  glm.roc <- roc(response = test_data$Churn, predictor = as.numeric(preds_glm))
  knn.roc <- roc(response = test_data$Churn, predictor = as.numeric(preds_knn))
  dt.roc <- roc(response = test_data$Churn, predictor = as.numeric(preds_dt))
  rf.roc <- roc(response = test_data$Churn, predictor = as.numeric(preds_rf))

  plot(glm.roc, print.auc = TRUE, print.auc.y = 1.0, percent=TRUE, grid=TRUE, main="ROC analysis for four models")
  plot(knn.roc, col = "#00783e", add = TRUE, print.auc.y = 0.90, print.auc = TRUE)
  plot(dt.roc, col = "blue", add = TRUE, print.auc.y = 0.80, print.auc = TRUE)
  plot(rf.roc, col = "red" , add = TRUE, print.auc.y = 0.70, print.auc = TRUE)
  legend("bottom", c("Logistic", "KNN" , "Decision Tree", "Random Forest"),
         lty = c(1,1), lwd = c(2, 2), col = c("black" ,"#00783e",  "blue", "red"), cex = 0.75)
 
  #### Save shown image 
  #ggsave("save_files/images/figure27.jpeg", width = 10, height = 8)
  

###### End Result
  
  
################################################################################################
# **********************************************************************************************
### Save R Objects
  
  # **********************************************************************************************
  #This R script keeps track of what R objects are saved under what file names to use them in report.RMD 
  
  #### Save variables for	Data set 
  save(original_telecom_data, telecom_data, df_statistics, summary_pm_all, file = "save_files/rda/data_set.rda")
  
  #### Save variables for	Logistic Regression method
  save(model_glm, opt_co_glm, cm_glm, measures_glm, file = "save_files/rda/glm_variables.rda")
  
  #### Save variables for	3.4	KNN method
  save(opt_k_knn, model_knn, opt_co_knn, cm_knn, measures_knn, file = "save_files/rda/knn_variables.rda")
  
  #### Save variables for	Decision Tree method
  save(opt_cp_dt, model_dt, opt_co_dt, cm_dt, measures_dt, file = "save_files/rda/dt_variables.rda")
  
  #### Save variables for	3.5	Random Forest method
  save(opt_mtry_rf, model_rf, opt_co_rf, cm_rf, measures_rf, file = "save_files/rda/rf_variables.rda")
  
  
# **********************************************************************************************
# ***********************************   End the Code *******************************************
# **********************************************************************************************
  

  
  
 
  
  
  

