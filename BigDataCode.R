#Parallel and Sequential Processing----------------------------------------------------------------------------------------
#set working directory by code
setwd("D:/Big Data")
getwd()
library(data.table)
library(dplyr)
library(tidyverse)
library(parallel)
library(lme4)

#create function read multiple file using readr package
df <- function(i){
  list_cs_files <- list.files(path = "D:/Big Data") %>%
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
