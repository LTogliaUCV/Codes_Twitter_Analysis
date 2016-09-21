#R  paquetes
library(twitteR)
library(httr)
setwd("C:/Users/Leonardo/Documents/schedulerR")
# Twitter protocolo OAuth


api_key <- "xxxxxxxxxxxxxxxxxxxxxx"

api_secret <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"

access_token <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"

access_token_secret <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
1

# extraer tweets usando el usertimeline
#Donald Trump
trump_tweets <- userTimeline("realDonaldTrump", n = 1600)
tweetst.df <- twListToDF(trump_tweets)
dim(tweetst.df)




#ahora generamos una variable llamada date  y la convertimos en character 
date<-Sys.Date
date<-as.character(date)
name<-paste(date,".RData")
#ahora  guardamos los tweets bajo el nombre de dis de la descarga 
save(tweetst.df, file =name)