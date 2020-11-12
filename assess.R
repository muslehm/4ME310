library(dplyr)
library(tidyr)
library(stringr)
library(data.table)

options(scipen = 999) 
wd <- getwd()
setwd(wd)

##Data Cleaning and Preparaton####
#Read data set
query <- read.csv('query.csv', header = TRUE)

#Redefine only the top 10 relevant results out of 20 as  relevant
query$Relevant[query$MyRanking>10] <- FALSE 
query$MyRanking[query$Relevant==FALSE] <- -1 

#examine the dataset for consistency: Rankings and Relevance
table(query$MyRanking)
table(query$Relevant)

###General Results#####
results <- data.frame (Query = character(),Engine = character(),
                       False_Positive  = numeric(), False_Negative = numeric(),
                       Precision = numeric(), Recall=numeric(), stringsAsFactors = FALSE)
for (i in 1:30){
  if(i<6){  query_t <- paste("SW",i,sep="")}
  else if(i>5 & i<11){
    i_e <- i-5
    query_t <- paste("MW",i_e,sep="")}
  else if(i>10 & i<16){
    i_e <- i-10
    query_t <- paste("MWO",i_e,sep="")}
  else if(i>15 & i<21){
    i_e <- i-15
    query_t <- paste("MWN",i_e,sep="")}
  else if(i>20 & i<26){
    i_e <- i-20
    query_t <- paste("MWA",i_e,sep="")}
  else if(i>25 & i<31){
    i_e <- i-25
    query_t <- paste("MWC",i_e,sep="")}

#False Positive: irrelevant result of each engine####
google_fp <- nrow(query[(query$Google!=-1 & query$Relevant == FALSE & query$Query==query_t),])
ddg_fp <- nrow(query[(query$DuckDuckGo!=-1 & query$Relevant == FALSE & query$Query==query_t),])

#False Negative: not recalled from the relevant set####
google_fn <- nrow(query[(query$Google==-1 & query$Relevant == TRUE & query$Query==query_t),])
ddg_fn <- nrow(query[(query$DuckDuckGo==-1 & query$Relevant == TRUE & query$Query==query_t),])

#Precision: percentage of relevant retrieved documents from all retrieved documents ####
google_precision <- nrow(query[(query$Google!=-1 & query$Relevant == TRUE & 
                                  query$Query==query_t),])/nrow(query[(query$Google!=-1 & query$Query==query_t),])
ddg_precision <- nrow(query[(query$DuckDuckGo!=-1 & query$Relevant == TRUE & 
                             query$Query==query_t),])/nrow(query[(query$DuckDuckGo!=-1 & query$Query==query_t),])

#Recall: percentage of relevant retrieved documents from all relevant documents#####
google_recall <- nrow(query[(query$Google!=-1 & query$Relevant == TRUE & 
                             query$Query==query_t),])/nrow(query[(query$Relevant == TRUE& query$Query==query_t),])
ddg_recall <- nrow(query[(query$DuckDuckGo!=-1 & query$Relevant == TRUE& 
                          query$Query==query_t),])/nrow(query[(query$Relevant == TRUE & query$Query==query_t),])

result <- data.frame (Query = c(query_t, query_t), Engine = c('Google', 'DuckDuckGo'), 
                      False_Positive  = c(google_fp, ddg_fp), False_Negative = c(google_fn, ddg_fn),
                       Precision = c(round(google_precision,2), round(ddg_precision,2)), 
                      Recall=c(round(google_recall,2), round(ddg_recall,2)))
results <- rbind(results, result)
}

#Average Results
Stats_by_Engine <- results %>% group_by(Engine) %>% 
  summarize(FP = sum(False_Positive, na.rm = TRUE) , FN = sum(False_Negative, na.rm = TRUE),
            Precision = mean(Precision), Recall = mean(Recall))

results_t <- results
results_t$Query <- substr(results_t$Query,1,nchar(results_t$Query)-1)
Stats_by_Engine_Query <- results_t %>% group_by(Engine, Query) %>% 
  summarize(FP = sum(False_Positive, na.rm = TRUE) , FN = sum(False_Negative, na.rm = TRUE),
            Precision = mean(Precision), Recall = mean(Recall))

Stats_by_Engine_Query$query_r <- ifelse(Stats_by_Engine_Query$Query == 'SW', 1, 
                                        ifelse(Stats_by_Engine_Query$Query == 'MW', 2,
                                               ifelse(Stats_by_Engine_Query$Query == 'MWA', 3,
                                                      ifelse(Stats_by_Engine_Query$Query == 'MWO', 4,
                                                             ifelse(Stats_by_Engine_Query$Query == 'MWN', 5,
                                                                    ifelse(Stats_by_Engine_Query$Query == 'MWC', 6,0))))))
Stats_by_Engine_Query <- arrange(Stats_by_Engine_Query, query_r)


##Plot the Results###
g_eq <- Stats_by_Engine_Query[(Stats_by_Engine_Query$Engine=='Google'),]
d_eq <- Stats_by_Engine_Query[(Stats_by_Engine_Query$Engine=='DuckDuckGo'),]

par(mfrow=c(2,2))
plot(g_eq$query_r, g_eq$Precision, col = "green", xaxt = "n",
     type = "b", pch = 16, ylim = c(0,1), ylab = "Precision", xlab = "",  lwd = 3)
lines(d_eq$query_r, d_eq$Precision, col = "blue", xaxt = "n",
      type = "b", pch = 16, ylim = c(0,1), lwd = 3)
axis(1, at = 1:6, labels = c('SW', 'MW','MWA','MWO','MWN','MWC'))


plot(g_eq$query_r, g_eq$Recall, col = "green", xaxt = "n",
     type = "b", pch = 16, ylim = c(0,1), ylab = "Recall", xlab = "",  lwd = 3)
lines(d_eq$query_r, d_eq$Recall, col = "blue", xaxt = "n",
      type = "b", pch = 16, ylim = c(0,1), lwd = 3)
axis(1, at = 1:6, labels = c('SW', 'MW','MWA','MWO','MWN','MWC'))

plot(g_eq$query_r, g_eq$FP, col = "green", xaxt = "n",
     type = "b", pch = 16, ylim = c(0,40), ylab = "False Positives", xlab = "",  lwd = 3)
lines(d_eq$query_r, d_eq$FP, col = "blue", xaxt = "n",
      type = "b", pch = 16, ylim = c(0,40), lwd = 3)
axis(1, at = 1:6, labels = c('SW', 'MW','MWA','MWO','MWN','MWC'))


plot(g_eq$query_r, g_eq$FN, col = "green", xaxt = "n",
     type = "b", pch = 16, ylim = c(0,40), ylab = "False Negatives", xlab = "",  lwd = 3)
lines(d_eq$query_r, d_eq$FN, col = "blue", xaxt = "n",
      type = "b", pch = 16, ylim = c(0,40), lwd = 3)
axis(1, at = 1:6, labels = c('SW', 'MW','MWA','MWO','MWN','MWC'))

legend(-3.5,75, cex=0.8,legend = c("Google", "DuckDuckGo"), col = c("green", "blue"), 
       lty = 1,title = "IR Algorithm",xpd="NA")

Stats_by_Engine_Query  <- Stats_by_Engine_Query [-7]
par(mfrow=c(1,1))
#Remove unneeded variables
rm(ddg_fn, ddg_fp, ddg_precision, ddg_recall, google_fn, google_fp, 
   google_precision, google_recall, i, i_e, query_t, result, results_t, d_eq, g_eq)


###Prepare for R-P curve####
google <- query[(query$Google!=-1),]
keeps <- c('Google', 'Query','Relevant')
google <- google[keeps]

ddg <- query[(query$DuckDuckGo!=-1),]
keeps <- c('DuckDuckGo', 'Query','Relevant')
ddg <- ddg[keeps]

ddg<- ddg[order(ddg$Query, ddg$DuckDuckGo),]
google<- google[order(google$Query, google$Google),]

row.names(google) <- 1 : 300
row.names(ddg) <- 1 : 300


rank_rpg<- data.frame (Query = character(), Engine = character(),
                       Recall_pc  = numeric(), Precision_pc = numeric(),  
                      stringsAsFactors = FALSE)

rank_rpd<- data.frame (Query = character(), Engine = character(),
                      Recall_pc  = numeric(), Precision_pc = numeric(),  
                      stringsAsFactors = FALSE)

#Google Ranking ####

for (i in 1:30){
  if(i<6){  query_t <- paste("SW",i,sep="")}
  else if(i>5 & i<11){
    i_e <- i-5
    query_t <- paste("MW",i_e,sep="")}
  else if(i>10 & i<16){
    i_e <- i-10
    query_t <- paste("MWO",i_e,sep="")}
  else if(i>15 & i<21){
    i_e <- i-15
    query_t <- paste("MWN",i_e,sep="")}
  else if(i>20 & i<26){
    i_e <- i-20
    query_t <- paste("MWA",i_e,sep="")}
  else if(i>25 & i<31){
    i_e <- i-25
    query_t <- paste("MWC",i_e,sep="")}
  
  que_ran <- google[(google$Query==query_t),]
  documents <- nrow(que_ran[(que_ran$Relevant == TRUE),])
  precision <- 100
  recall <- 10
  result_n <- 1
  true_g <- 0
  
  #Set the Precision at Recall 0
  for(w in 1:10){
    if(que_ran[w,3]== TRUE)
    {
      precision <- round((1/w)*100,2)
      break
    }
  }
  
  if(documents==0)
  {precision <- 0}
  rank_temp <- data.frame (Query = query_t, Engine = "Google",
                           Recall_pc  = 0, Precision_pc = precision)
  rank_rpg <- rbind(rank_rpg, rank_temp) 
  
  for (x in 1:10){
    if(que_ran[x,3] == TRUE)
    {
      true_g <- true_g + 1
      precision <- round((true_g/result_n)*100,2)
      rank_temp <- data.frame (Query = query_t, Engine = "Google",
                               Recall_pc  = recall, Precision_pc = precision)
      rank_rpg <- rbind(rank_rpg, rank_temp) 
      result_n <- result_n + 1
      recall <- recall + 10
    }
    else
    {
      result_n <- result_n + 1
    }
    
  if(x==10 & documents<x)
    {
      for (y in 1:(x-documents))
      {
        precision = 0
        rank_temp <- data.frame (Query = query_t, Engine = "Google",
                                Recall_pc  = recall, Precision_pc = precision)
       rank_rpg <- rbind(rank_rpg, rank_temp) 
       recall <- recall + 10
     }
    }
  }
}

# DDG Ranking ####
for (i in 1:30){
  if(i<6){  query_t <- paste("SW",i,sep="")}
  else if(i>5 & i<11){
    i_e <- i-5
    query_t <- paste("MW",i_e,sep="")}
  else if(i>10 & i<16){
    i_e <- i-10
    query_t <- paste("MWO",i_e,sep="")}
  else if(i>15 & i<21){
    i_e <- i-15
    query_t <- paste("MWN",i_e,sep="")}
  else if(i>20 & i<26){
    i_e <- i-20
    query_t <- paste("MWA",i_e,sep="")}
  else if(i>25 & i<31){
    i_e <- i-25
    query_t <- paste("MWC",i_e,sep="")}
  que_ran <- ddg[(ddg$Query==query_t),]
  documents <- nrow(que_ran[(que_ran$Relevant == TRUE),])
  precision <- 100
  recall <- 10
  result_n <- 1
  true_g <- 0
  for(w in 1:10){
    if(que_ran[w,3]== TRUE)
    {
      precision <- round((1/w)*100,2)
      break
    }
  }
  if(documents==0)
  {precision <- 0}
  
  rank_temp <- data.frame (Query = query_t, Engine = "DuckDuckGo",
                           Recall_pc  = 0, Precision_pc = precision)
  rank_rpd <- rbind(rank_rpd, rank_temp) 
  for (x in 1:10){
    if(que_ran[x,3] == TRUE)
    {
      true_g <- true_g + 1
      precision <- round((true_g/result_n)*100,2)
      rank_temp <- data.frame (Query = query_t, Engine = "DuckDuckGo",
                               Recall_pc  = recall, Precision_pc = precision)
      rank_rpd <- rbind(rank_rpd, rank_temp) 
      result_n <- result_n + 1
      recall <- recall + 10
    }
    else
    {
      result_n <- result_n + 1
    }
    
    if(x==10 & documents<x)
    {
      for (y in 1:(x-documents))
      {
        precision = 0
        rank_temp <- data.frame (Query = query_t, Engine = "DuckDuckGo",
                                 Recall_pc  = recall, Precision_pc = precision)
        rank_rpd <- rbind(rank_rpd, rank_temp) 
        recall <- recall + 10
      }
    }
  }
}
#Average the ranks ####
final_rank_g<- data.frame (Recall_pc  = numeric(), Precision_pc = numeric(),  
                       stringsAsFactors = FALSE)

final_rank_d<- data.frame (Recall_pc  = numeric(), Precision_pc = numeric(),  
                           stringsAsFactors = FALSE)
for (i in 0:10){
x = i*10

y <- mean(rank_rpg$Precision_pc[(rank_rpg$Recall_pc==x)])
z <- mean(rank_rpd$Precision_pc[(rank_rpd$Recall_pc==x)])

rank_g <- data.frame (Recall_pc  = x, Precision_pc = y)
final_rank_g <- rbind(final_rank_g, rank_g) 

rank_d <- data.frame (Recall_pc  = x, Precision_pc = z)
final_rank_d <- rbind(final_rank_d, rank_d) 

}
#Plot ######

plot(final_rank_g$Recall_pc, final_rank_g$Precision_pc,  col = "green", 
     type = "b", pch = 16, ylim = c(0,100), ylab = "Precision", xlab = "Recall", 
     xaxp = c(0, 100, 10), yaxp = c(0, 100, 5), lwd = 3)
lines(final_rank_d$Recall_pc, final_rank_d$Precision_pc, col = "blue", 
      type = "b", pch = 16, ylim = c(0,100), yaxp = c(0, 100, 5), lwd = 3)
axis(side = 2, tck=1, at = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100))
legend("bottomleft",                                       
       legend = c("Google", "DuckDuckGo"),
       col = c("green", "blue"),
       lty = 1)

#Ranking######
google_rank <- query[(query$Google!=-1 & query$Relevant==TRUE),]
google_rank <- google_rank[ , c("Google", "MyRanking")]  
google_rank$rank_diff <- google_rank$Google-google_rank$MyRanking

ddg_rank <- query[(query$DuckDuckGo!=-1 & query$Relevant==TRUE),]
ddg_rank <- ddg_rank[ , c("DuckDuckGo", "MyRanking")]  
ddg_rank$rank_diff <- ddg_rank$DuckDuckGo-ddg_rank$MyRanking





#BoxPlots####
duckduckgo_n <- paste("DuckDuckGo ",nrow(ddg_rank))
google_n <- paste("Google ",nrow(google_rank))
par(mfrow=c(1,1))
boxplot(ddg_rank$rank_diff, google_rank$rank_diff, main = "DDG vs Google", ylab = "Rank Difference", 
        width = c(nrow(ddg_rank), nrow(google_rank)), names = c(duckduckgo_n, google_n))
#Remove all unneeded variables
rm(google_rank, que_ran, rank_d, rank_g, query, rank_rpd, rank_rpg, rank_temp, documents, duckduckgo_n,
   google_n,i,i_e, keeps, precision, query_t, recall, result_n, true_g, w, x,y,z, final_rank_d, final_rank_g, ddg_rank, ddg, google)
