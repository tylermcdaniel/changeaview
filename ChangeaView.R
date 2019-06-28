#                     Tyler McDaniel                             #
#              SICSS 2019 Change a View Analysis                 #
#                       June, 2019                               #
##################################################################

library(readxl)

# download data
setwd("C:/Users/tyler/Documents/R/SICSS") # change to file location

cav <- read_xlsx("CAV_data.xlsx")


##################################################################
# Reddit Data
##################################################################

################## Posts
reddit_train <- read.csv("reddit_train.csv")
reddit_train$test = 0

reddit_test <- read.csv("reddit_test.csv")
reddit_test$test = 1

reddit_post_data <- rbind(reddit_train,reddit_test)


write.csv(reddit_post_data, "reddit_post_data.csv")

################## Comments
reddit_comments_train <- read.csv("reddit_comments_train.csv")
reddit_comments_train$test = 0

reddit_comments_test <- read.csv("reddit_comments_test.csv")
reddit_comments_test$test = 1

reddit_comment_data <- rbind(reddit_comments_train,reddit_comments_test)


write.csv(reddit_comment_data, "reddit_comment_data.csv")


###################################################################
# Network Analyses
###################################################################

library(tidyverse) 
library(gdata) 
library(igraph) 

################## 
posts <- read.csv("CAV_data.csv", stringsAsFactors = FALSE) 

#data for different topics 
users <- read.csv("CAV_User.csv", stringsAsFactors = FALSE) 

#data for different users 
users$Delta_Given_User <- sub("@", "", users$Delta_Given_User) nodes <- as.data.frame(unique(c(users$User, users$Delta_Given_User)),stringsAsFactors = FALSE) 

#creating node list of users 
colnames(nodes) <- "user"

delta_links <- users %>% filter(Delta_Given > 0) %>%  
#creating edge list of deltas given and received   
select(from = User, to = Delta_Given_User, weight = Delta_Given) %>%   group_by(from, to) %>% summarise(weight = sum(weight)) 

delta_network <- graph_from_data_frame(d = delta_links, vertices = nodes, directed=T) plot(delta_network, edge.arrow.size=.4, edge.curved=.1, vertex.size=2, vertex.label = NA) 
plot(delete.vertices(simplify(delta_network), degree(delta_network)==0),      edge.arrow.size=.2, edge.curved=.1, vertex.size=2, vertex.label = NA)


#################################################################
# comments, shine, and user delta data
comments <- read.csv("CAV_Comment.csv", stringsAsFactors = FALSE)

delta <- read.csv("CAV_User_Delta.csv", stringsAsFactors = FALSE)

shine <- read.csv("CAV_User_Shine.csv", stringsAsFactors = FALSE)

# clean dates
library(lubridate)

delta$Delta_Post_Date[is.na(delta$Delta_Post_Date)]  = ""

delta$New_Date <- ymd(delta$Delta_Post_Date)

for(i in 2018:2019){
  delta$New_Date[endsWith(delta$Delta_Post_Date,paste(i))] <-  
    ymd(format(as.Date(delta$Delta_Post_Date[endsWith(delta$Delta_Post_Date,paste(i))],
                 "%m/%d/%Y"), "%Y/%m/%d"))
}

# make names uniform
delta$User <- paste("@",delta$User, sep = "")


### descriptive plots
library(ggplot2)

# try plotting (color doesnt mean much here, give = receive for another)
ggplot(na.omit(delta[delta$Delta_Direction  != "",]), aes(x = New_Date)) + 
  geom_histogram(stat = "count")+
  ggtitle("Deltas Given and Received Over Time")


# collapse deltas to see topic, confidence
titles <- comments[duplicated(comments$Post_Title) == FALSE,
                   c("Post_Title","Post_Tag","Post_Confidence")]

# join datasets
library(dplyr)
colnames(delta)[colnames(delta) == "Delta_Post_Title"] = "Post_Title"

# this join only uses rows with full information (there are 7 that are excluded from delta[delta$Delta_Direction  != "",])
delta_2 <- inner_join(delta,titles,by = "Post_Title")

# try plotting again
ggplot(delta_2, aes(x = New_Date, fill = Post_Confidence)) + 
  geom_histogram(stat = "count")+
  ggtitle("Deltas Given and Received Over Time") 


#######################################################################
# Load in text groups data
#######################################################################

clusters <- read.csv("user_group_lin.csv", stringsAsFactors = FALSE)

clusters <- clusters[,colnames(clusters) != "X"]

# first user
colnames(clusters) <- c("User","User_Group")

delta_2 <- left_join(delta_2,clusters, by = "User")

# second user
colnames(clusters) <- c("Delta_Interaction_User","Delta_Interaction_User_Group")

delta_2 <- left_join(delta_2,clusters, by = "Delta_Interaction_User")

# create variable for in-group / out-group transactions
delta_2$Interaction = "Out-Group"
delta_2$Interaction[delta_2$User_Group == delta_2$Delta_Interaction_User_Group] = "In-Group"


# try plotting again
ggplot(delta_2[delta_2$Delta_Direction == "Give",], aes(x = New_Date, fill = Interaction)) + 
  geom_histogram(stat = "count")+
  ggtitle("Deltas Given and Received Over Time") 


#################################################################
# Cumulative Deltas
#################################################################

delta_2$Cumulative_Deltas_Received <- 0
delta_2$Cumulative_Deltas_Given <- 0

for(i in unique(delta_2$User)){
for(j in unique(delta_2$New_Date)){
  delta_2$Cumulative_Deltas_Received[delta_2$User == i & delta_2$New_Date == j] <- 
    sum(delta_2$Delta_Direction[delta_2$User == i & delta_2$New_Date <= j] == "Receive")
  delta_2$Cumulative_Deltas_Given[delta_2$User == i & delta_2$New_Date == j] <- 
    sum(delta_2$Delta_Direction[delta_2$User == i & delta_2$New_Date <= j] == "Give")
  
  }
}

# plot change in deltas over time

ggplot(delta_2[delta_2$Delta_Direction == "Receive",], aes(x = New_Date, y = Cumulative_Deltas_Received)) + 
  geom_line(aes(color = User), size = 1) + theme(legend.position = "none") + 
  ggtitle("How Users Accumulate Deltas Over Time")
  
ggplot(delta_2[delta_2$Delta_Direction == "Give",], aes(x = New_Date, y = Cumulative_Deltas_Given)) + 
  geom_line(aes(color = User), size = 1) + theme(legend.position = "none") + 
  ggtitle("How Users Give Deltas Over Time")

# bottom 10% and top 10% of users and deltas accumulated

quantile(delta_2$Cumulative_Deltas_Received[delta_2$New_Date == delta_2$New_Date[56]], .9)

quantile(delta_2$Delta_Received[duplicated(delta_2$User)  == FALSE] , .5)

# what percentage of deltas does top user have?

for(i in unique(delta_2$New_Date)){
  delta_2$Total_Deltas[delta_2$New_Date == i] = 
    sum(delta_2$Delta_Direction[delta_2$New_Date <= i] == "Receive")
  
  top <- delta_2$User[delta_2$New_Date <= i][order(delta_2$Cumulative_Deltas_Received[delta_2$New_Date <= i],
                                                     decreasing = TRUE)]
  top10 <- unique(top)[1:round(length(unique(delta_2$User[delta_2$New_Date <= i]))*.1)]
  
  for(j in top10){
  delta_2$Top_User_Deltas[delta_2$New_Date <= i & 
                            delta_2$User == j] = 
    max(delta_2$Cumulative_Deltas_Received[delta_2$New_Date <= i &  
                                         delta_2$User == j])
  }
  
  delta_2$Top_Total_Deltas[delta_2$New_Date == i] = sum(delta_2$Top_User_Deltas[delta_2$New_Date <= i & delta_2$User %in% top10][duplicated(delta_2$User[delta_2$New_Date <= i & delta_2$User %in% top10]) == FALSE],
                                 na.rm = TRUE)
}

delta_2$Top_Delta_Share = delta_2$Top_Total_Deltas/delta_2$Total_Deltas

# plot

ggplot(delta_2[delta_2$Delta_Direction == "Receive",], aes(x = New_Date, y = Top_Delta_Share)) + 
  geom_line(size = 1) + theme(legend.position = "none") + 
  ggtitle("Share of Deltas Recieved by Top 10% of Users Over Time")


#################################################################
# Three network graphs
#################################################################
delta.1 <- delta_2[delta_2$New_Date <= ymd("2019/2/26"),]

delta.2 <- delta_2[delta_2$New_Date <= ymd("2019/4/26") & 
                     delta_2$New_Date > ymd("2019/2/26"),]
  
delta.3 <- delta_2[delta_2$New_Date <= ymd("2019/6/26") & 
                     delta_2$New_Date > ymd("2019/4/26"),]

write.csv(delta.1, "delta_1.csv")
write.csv(delta.1, "delta_2.csv")
write.csv(delta.1, "delta_3.csv")


#################################################################
# Text Networks
#################################################################

install.packages("devtools")
library(devtools)
install_github("cbail/textnets")
library(textnets)

library(textnets)
data("sotu")
