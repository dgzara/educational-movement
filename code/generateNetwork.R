folder <- paste0(getwd(),"/code")
setwd(folder)
source(file='password.R')
source(file='dbConnect.R')
source(file='functions.R')
library(stringr)

#http://stackoverflow.com/questions/18164839/get-twitter-username-with-regex-in-r
extract_user <- function(x){
  x <- tolower(x)  
  x <- unlist(strsplit(x, " "))
  x <- unlist(strsplit(x, "[.]"))
  regex1 <- "(^|[^@\\w])@(\\w{1,15})\\b" # get strings with @
  regex2 <- "[^[:alnum:]@_]"             # remove all punctuation except _ and @  
  users <- gsub(regex2, "", x[grep(regex1, x, perl = T)])
  return(users)
}
extract_user_mention <- function(x1){
  x1 <- tolower(x1)    
  
  x <- x1
  x <- unlist(strsplit(x, " rt"))
  if(length(x) > 1){
    x <- x[1]
    x <- unlist(strsplit(x, " "))
    x <- unlist(strsplit(x, "[.]"))
    regex1 <- "(^|[^@\\w])@(\\w{1,15})\\b" # get strings with @
    regex2 <- "[^[:alnum:]@_]"             # remove all punctuation except _ and @  
    users <- gsub(regex2, "", x[grep(regex1, x, perl = T)])
    
    return(users)
  }else{
    
    if(grepl("retweet",x1)){
      x <- x1
      x <- unlist(strsplit(x, "retweet"))
      x <- x[1]
      x <- unlist(strsplit(x, " "))
      x <- unlist(strsplit(x, "[.]"))
      regex1 <- "(^|[^@\\w])@(\\w{1,15})\\b" # get strings with @
      regex2 <- "[^[:alnum:]@_]"             # remove all punctuation except _ and @  
      users <- gsub(regex2, "", x[grep(regex1, x, perl = T)])    
      return(users)
    }else{
      return(c())
    }
  }
}
extract_user_retweet <- function(x1){
  x1 <- tolower(x1)    
  
  x <- x1
  x <- unlist(strsplit(x, "rt"))
  x <- x[2]
  x <- unlist(strsplit(x, " "))
  x <- unlist(strsplit(x, "[.]"))
  regex1 <- "(^|[^@\\w])@(\\w{1,15})\\b" # get strings with @
  regex2 <- "[^[:alnum:]@_]"             # remove all punctuation except _ and @  
  users <- gsub(regex2, "", x[grep(regex1, x, perl = T)])
  
  #if(grepl(value, chars)
  if(length(users) == 0 && grepl("retweet",x1)){
    x <- x1
    x <- unlist(strsplit(x, "retweet"))
    x <- x[2]
    x <- unlist(strsplit(x, " "))
    x <- unlist(strsplit(x, "[.]"))
    regex1 <- "(^|[^@\\w])@(\\w{1,15})\\b" # get strings with @
    regex2 <- "[^[:alnum:]@_]"             # remove all punctuation except _ and @  
    users <- gsub(regex2, "", x[grep(regex1, x, perl = T)])    
  }
  return(users)    
  
}

#Definitions
retweet_definition <- " tweet LIKE '%retweet @%' OR  tweet  LIKE '%_ rt @%' OR  tweet  LIKE '%RT @%' OR  tweet  LIKE 'rt @%' OR  tweet  LIKE '%Rt @%' OR  tweet  LIKE '%rT @%' OR  tweet  LIKE '%thx @%' OR  tweet  LIKE '%MT @%' OR  tweet  LIKE '%retweeting @%'"
retweet_definition_nested <- " tweet NOT LIKE '%retweet @%' AND  tweet NOT  LIKE '%_ rt @%' AND  tweet NOT  LIKE '%RT @%' AND  tweet NOT  LIKE 'rt @%' AND  tweet NOT  LIKE '%Rt @%' AND  tweet NOT  LIKE '%rT @%' AND  tweet NOT  LIKE '%thx @%' AND  tweet NOT  LIKE '%MT @%' AND  tweet NOT  LIKE '%retweeting @%'"

sql <- paste("SELECT DISTINCT account as hashtag FROM hashtag")
hashtagList <- dbGetQuery(con, sql)
hashtagList <- hashtagList$hashtag
m <- length(hashtagList)

for(i in 1:m)
{
  #First Get the mention networks in reply
  sql <- paste("SELECT id, account as hashtag, tweetuser as user, tweet as content, tweetdate as date  
               FROM hashtag
               WHERE tweet LIKE '@%' AND (",retweet_definition_nested,") AND account = '",hashtagList[i],"';",sep="")
  
  text_reply <-dbGetQuery(con, sql)
  
  # Get the mention and retweet in retweet networks
  sql <- paste("SELECT id, account as hashtag, tweetuser as user, tweet as content, tweetdate as date  
               FROM hashtag
               WHERE (",retweet_definition,") AND account = '",hashtagList[i],"';",sep="")
  
  text_reply_retweet <- dbGetQuery(con, sql)
  
  #Get the mentions in mention network
  #Does not work with @ xidaowen
  sql <- paste("SELECT id, account as hashtag, tweetuser as user, tweet as content, tweetdate as date  
               FROM hashtag
               WHERE tweet LIKE '%@%' AND account = '",hashtagList[i],"' AND tweet NOT LIKE '@%' AND (",retweet_definition_nested,");",sep="")
  text_reply_mention <- dbGetQuery(con, sql)
  
  #Get the users who participated in that hashtag
  sql <-paste("SELECT DISTINCT(tweetuser) AS user
              FROM hashtag
              WHERE account = '",hashtagList[i],"';",sep="")
  usersProtest <- dbGetQuery(con, sql)
  usersProtest <- usersProtest$user
  
  #Build replies network
  n <- nrow(text_reply)
  if(n>0){
    for(k in 1:n){
      if(is.element(text_reply[k,]$user, usersProtest)){
        source <- text_reply[k,]$user
        hashtag <- text_reply[k,]$hashtag
        id <- text_reply[k,]$id  
        date <- text_reply[k,]$date
        content <- text_reply[k,]$content
        
        users_list <- extract_user(content)
        if(length(users_list) > 0){
          for(l in 1:length(users_list)){
            user <- users_list[l]
            if(l == 1) {type <- "reply"}
            else {type <- "mention"}
            user <- gsub("'s",'',user)
            user <- gsub("@",'',user)
            if(str_length(user)>0 && is.element(user, usersProtest)){
              sql_reply <- paste("INSERT INTO hashtag_user_network (source, target, type, hashtag, date, tweetid) VALUES ('",source,"','",user,"','",type,"','",hashtag,"','",date,"','",id,"');", sep="")
              dbSendQuery(con, sql_reply)          
            }
          } 
        }
        l == 0
      }
    }
  }
  
  #Build retweets network
  n <- nrow(text_reply_retweet)
  if(n>0)
  {
    for(k in 1:n){
      if(is.element(text_reply_retweet[k,]$user, usersProtest)){
        source <- text_reply_retweet[k,]$user
        hashtag <- text_reply_retweet[k,]$hashtag
        id <- text_reply_retweet[k,]$id  
        date <- text_reply_retweet[k,]$date
        content <- text_reply_retweet[k,]$content
        
        #mention in retweet networks
        users_list_mention <- extract_user_mention(content)
        if(length(users_list_mention)>0){
          for(l in 1:length(users_list_mention)){
            user_mention <- users_list_mention[l]
            type <- "mention"
            user_mention <- gsub("'s",'',user_mention)
            user_mention <- gsub("@",'',user_mention)
            if(str_length(user_mention) > 0 && is.element(user_mention, usersProtest)){
              sql_mention <- paste("INSERT INTO hashtag_user_network (source, target, type, hashtag, date, tweetid) VALUES ('",source,"','",user,"','",type,"','",hashtag,"','",date,"','",id,"');", sep="")
              dbSendQuery(con, sql_mention)      
            }
          }    
        }
        l == 0
        
        #retweet in retweet networks
        users_list <- extract_user_retweet(content)
        if(length(users_list) > 0){
          user <- users_list[1]
          type <- "retweet"
          user <- gsub("'s",'',user)
          user <- gsub("@",'',user)
          if(str_length(user) > 0 && is.element(user, usersProtest)){
            sql_retweet <- paste("INSERT INTO hashtag_user_network (source, target, type, hashtag, date, tweetid) VALUES ('",source,"','",user,"','",type,"','",hashtag,"','",date,"','",id,"');", sep="")
            dbSendQuery(con, sql_retweet)
          }
          
        }
      }
    }
  }
  
  #Build mentions network
  n <- nrow(text_reply_mention)
  if(n>0){
    for(k in 1:n){
      if(is.element(text_reply_mention[k,]$user, usersProtest)){
        source <- text_reply_mention[k,]$user
        hashtag <- text_reply_mention[k,]$hashtag
        id <- text_reply_mention[k,]$id  
        date <- text_reply_mention[k,]$date
        content <- text_reply_mention[k,]$content
        
        users_list <- extract_user(content)
        type <- "mention"
        if(length(users_list) > 0){
          for(l in 1:length(users_list)){
            user <- users_list[l]
            user <- gsub("'s",'',user)
            user <- gsub("@",'',user)
            if(str_length(user) > 0 && is.element(user, usersProtest)){
              sql_mention_single <- paste("INSERT INTO hashtag_user_network (source, target, type, hashtag, date, tweetId) VALUES ('",source,"','",user,"','",type,"','",hashtag,"','",date,"','",id,"');", sep="")
              dbSendQuery(con, sql_mention_single)        
            }
          }   
        }
        l == 0
      }
    }
  }
}
