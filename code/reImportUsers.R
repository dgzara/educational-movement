folder <- paste0(getwd(),"/code")
setwd(folder)
source(file='password.R')
source(file='dbConnect.R')
source(file='functions.R')
source(file='listado.R')

# Lista diputados
diputados <- c('GiorgioJackson', 'camila_vallejo', 'gabrielboric')

# Borramos la base de datos
q <- "TRUNCATE users_topsy RESTART IDENTITY;"
dbSendQuery(con, q)

for(i in diputados){
  
  if(i == "GiorgioJackson"){
    data1 <- read.csv(paste0("../data/leaders/",i,".csv"), stringsAsFactors = FALSE)
    data2 <- read.csv(paste0("../data/leaders/",i,"2.csv"), stringsAsFactors = FALSE)
    data2$lang <- NULL
    data <- rbind(data1, data2)
  }
  else{
    data <- read.csv(paste0("../data/leaders/",i,".csv"), stringsAsFactors = FALSE)
  }
  
  print(i)
  
  for(j in 1:nrow(data))
  {
    row <- data[j, ]
    user_followers_count <- 0
    
    # Formateamos las fechas
    created_at <- row$created_at
    user_created_at <- row$user_created_at
    user_profile_description <- gsub("'", "", row$user_profile_description)
    user_profile_location <- gsub("'", "", row$user_profile_location)
    user_name <- gsub("'", "", row$user_name)
    user_time_zone <- gsub("'", "", row$user_time_zone)
    
    # Chequeamos las fechas
    #if(is.na(as.Date(as.character(user_created_at),format="%d/%m/%Y"))){
    #  user_created_at <- NULL
    #}
    #if(is.na(as.Date(as.character(created_at),format="%d/%m/%Y"))){
      #created_at <- NULL
    #}
    
    # Chequeamos los followers
    if(!is.na(row$user_followers_count) & !is.null(row$user_followers_count) & row$user_followers_count != ""){
      user_followers_count <- as.numeric(row$user_followers_count)
    }
    
    text <- gsub("'", "", row$text)
    
    # Creamos la query
    newq <- paste("INSERT INTO users_topsy (created_at, favorited, in_reply_to_screen_name, permanent_link, source, text, user_profile_description, user_profile_location, user_created_at, user_followers_count, user_name, user_screen_name, user_time_zone)  VALUES ('",created_at,"','",row$favorited,"','",row$in_reply_to_screen_name,"','",row$permanent_link,"','",row$source,"','",text,"','",user_profile_description,"','",user_profile_location,"','",user_created_at,"',",user_followers_count,",'",user_name,"','",row$user_screen_name,"','",user_time_zone,"');", sep="")
    newq <- gsub("''", "NULL", newq)
    newq <- gsub(",NA,", ",NULL,", newq)
    q <- paste(q, " ", newq)
    
    # Si se completan 100, o es la ultima fila, enviamos la query
    if(j%%100 == 0 || j == nrow(data))
    {
      dbSendQuery(con, q)
      q <- ''
      print(j)
    }
    
    rm(row, newq, text, created_at, user_created_at, user_profile_description, user_profile_location, user_name, user_time_zone)
  }
  rm(data)
}