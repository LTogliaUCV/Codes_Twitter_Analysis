
#R  paquetes
library(twitteR)
library(httr)
setwd("C:/Users/Leonardo/Documents/schedulerR")

api_key <- "xxxxxxxxxxxxxxxxxxxxxx"

api_secret <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"

access_token <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"

access_token_secret <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
1

# extraer tweets usando el usertimeline
#HillaryClinton
clinton_tweets <- userTimeline("HillaryClinton", n = 1600)
tweetsc.df <- twListToDF(clinton_tweets)
dim(tweetsc.df)
1




#ahora generamos una variable llamada date  y la convertimos en character 
date<-Sys.Date()
date<-as.character(date)
name<-paste(date,".RData")
#ahora  guardamos los tweets bajo el nombre de dis de la descarga 
save(tweetsc.df, file =name)