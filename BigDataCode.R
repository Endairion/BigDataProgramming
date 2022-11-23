#Parallel and Sequential Processing----------------------------------------------------------------------------------------
#set working directory by code
install.packages('dplyr')
install.packages('data.table')
install.packages('tidyverse')
install.packages('parallel')
install.packages('lme4')
setwd("/dataset/")
library(data.table)
library(dplyr)
library(tidyverse)
library(parallel)
library(lme4)

#create function read multiple file using readr package
df <- function(i){
  library(dplyr)
  library(data.table)
  library(tidyverse)
  list_cs_files <- list.files(path = "~/BigDataProgramming/dataset") %>%
    map_df(~fread(.))
}




# detect the number of cores
n.cores <- detectCores()
n.cores
list_system.time <- list()
clust <- makeCluster(n.cores)


# single core (Sequential Processing)
list_system.time[["lapply"]] <- system.time(lapply(1:20, df))

# 12 Cores (Parallel Processing)
list_system.time[["parLapply"]] <- system.time(parLapply(clust, 1:20, df))

stopCluster(clust)

list_system.time

m_system.time <- data.frame(bind_rows(list_system.time))
m_system.time$apply_function <- factor(
  x = names(list_system.time),
  levels = names(list_system.time)[order(m_system.time$elapsed)]
)

v_color <- c("red", "blue")
names(v_color) <- names(list_system.time)
library(ggplot2)

ggplot(m_system.time) + aes(x = apply_function, y = user.self, label = round(user.self,2), fill = apply_function) +
  geom_col() +
  geom_text(nudge_y = 1) +
  theme_bw() +
  labs(
    title = paste0("Bar plot of for reading the file in both parallel and sequential."),
    subtitle = "The CPU time charged for the execution of the process."
  ) + 
  scale_fill_manual(values = v_color)
#Parallel and Sequential Processing----------------------------------------------------------------------------------------

#Descriptive Analysis============================================================
#reading from csv
df <- read.csv("DiabetesDataset.csv")

#check if any NA value
is.na(df)
#structure of df
str(df)
#summary of df
summary(df, digits = 5)

#create new dataframe that store 3 attributes without the area_id
grouped_df <- select(df,"sugar","carb","estimated_diabetes_prevalence")

#calculate standard deviation, mean and median with the new dataframe
sd <- lapply(grouped_df, sd)
mean <- lapply(grouped_df, mean)
median <- lapply(grouped_df, median)

#bind the 3 values into a new dataframe
new_df <- cbind(sd,mean,median)
#Adding a column using the name of the row
temp <- new_df
category <- rownames(new_df)
#setting the row name into default (number:1,2,3...)
rownames(temp)<-NULL
new_df<-cbind(category,temp)
new_df<-as.data.frame(new_df)
#unlist the data
new_df$category<-unlist(new_df$category)
new_df[2:4]<-as.numeric(unlist(new_df[2:4]))
new_df

#Visualization
#plotting histogram for each category with mean and median line 
hist(df$estimated_diabetes_prevalence)
abline(v=median$estimated_diabetes_prevalence,col="blue",lwd=3,lty=2)
abline(v=mean$estimated_diabetes_prevalence,col="red",lwd=3,lty=2)

hist(df$sugar)
abline(v=median$sugar,col="blue",lwd=3,lty=2)
abline(v=mean$sugar,col="red",lwd=3,lty=2)

hist(df$carb)
abline(v=median$carb,col="blue",lwd=3,lty=2)
abline(v=mean$carb,col="red",lwd=3,lty=2)



#plotting mean and standard deviation for the data
library(ggplot2)
ggplot(new_df, aes(x=category, y=mean)) +
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd), width= .1)+
  geom_point()

#Descriptive Analysis============================================================

#Correlation Test==========================================================
mosthighlycorrelated <- function(mydataframe,numtoreport)
{
  # find the correlations
  cormatrix <- cor(mydataframe)
  # set the correlations on the diagonal or lower triangle to zero,
  # so they will not be reported as the highest ones:
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  # flatten the matrix into a dataframe for easy sorting
  fm <- as.data.frame(as.table(cormatrix))
  # assign human-friendly names
  names(fm) <- c("First.Variable", "Second.Variable","Correlation")
  # sort and print the top n correlations
  head(fm[order(abs(fm$Correlation),decreasing=T),],n=numtoreport)
}

model <- select(df,"sugar","carb","estimated_diabetes_prevalence")
mosthighlycorrelated(model,9)

library(ggpubr)

#Scatterplot to visualize correlation
ggscatter(df,x="carb",y="estimated_diabetes_prevalence",
          add="reg.line", conf.int = TRUE,
          cor.coef=TRUE, cor.method = "pearson",
          xlab = "Carb", ylab="Prevalence of Diabetes")

ggscatter(df,x="sugar",y="estimated_diabetes_prevalence",
          add="reg.line", conf.int = TRUE,
          cor.coef=TRUE, cor.method = "pearson",
          xlab = "Sugar", ylab="Prevalence of Diabetes")
#Correlation Test==========================================================

#Hypothesis Testing========================================================

#hypothesis testing for prevalence of diabetes and carb
model1 <-lm(estimated_diabetes_prevalence ~ carb, data = df)
summary(model1)

#hypothesis testing for prevalence of diabetes and sugar
model2 <-lm(estimated_diabetes_prevalence ~ sugar, data = df)
summary(model2)
#Hypothesis Testing==========================================================
