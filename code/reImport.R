folder <- paste0(getwd(),"/code")
setwd(folder)
source(file='password.R')
source(file='dbConnect.R')
source(file='functions.R')
source(file='listado.R')

# Obtenemos los hashtags
q <- paste('SELECT DISTINCT(account)  
           FROM hashtag',sep="")
tweets <- dbGetQuery(con, q)
tweets <- na.omit(tweets)

# Listado de hashtags
hashtags <- sort(unique(unlist(tweets$account, use.names = FALSE)))

# Borramos la base de datos
q <- "TRUNCATE hashtags_time RESTART IDENTITY;"
dbSendQuery(con, q)

for(i in hashtags){
  data <- read.csv(paste0("../data/hashtags/",i,".csv"), stringsAsFactors = FALSE)
  print(i)
  for(j in 1:nrow(data))
  {
    print(j)
    row <- data[j, ]
    
    # Formateamos las fechas
    created_at <- row$created_at
    user_created_at <- row$user_created_at
    user_profile_description <- gsub("'", "", row$user_profile_description)
    user_profile_location <- gsub("'", "", row$user_profile_location)
    user_name <- gsub("'", "", row$user_name)
    user_time_zone <- gsub("'", "", row$user_time_zone)
    user_followers_count <- if(is.na(row$user_followers_count)) 0 else row$user_followers_count
      
    text <- gsub("'", "", row$text)

    # Creamos la query
    newq <- paste("INSERT INTO hashtags_time (hashtag, created_at, favorited, in_reply_to_screen_name, permanent_link, source, text, user_profile_description, user_profile_location, user_created_at, user_followers_count, user_name, user_screen_name, user_time_zone)  VALUES ('",i,"','",created_at,"','",row$favorited,"','",row$in_reply_to_screen_name,"','",row$permanent_link,"','",row$source,"','",text,"','",user_profile_description,"','",user_profile_location,"','",user_created_at,"',",user_followers_count,",'",user_name,"','",row$user_screen_name,"','",user_time_zone,"');", sep="")
    newq <- gsub("''", "NULL", newq)
    q <- paste(q, " ", newq)
    
    # Si se completan 50, o es la ultima fila, enviamos la query
    if(j%%100 == 0 || j == nrow(data))
    {
      dbSendQuery(con, q)
      q <- ''
    }
    
    rm(row, newq, text, created_at, user_created_at, user_profile_description, user_profile_location, user_name, user_time_zone)
  }
  rm(data)
}