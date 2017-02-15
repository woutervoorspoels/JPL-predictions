#### this function takes in a list that contains the tables 
#### of a JPL championship wiki page, and extracts the results

extract_results <- function(object,tablenr,nteams){
  temp <- object %>%
    html_nodes("table") %>%
    .[[tablenr]]%>%
    html_table(fill=T)
  colnames(temp)[1] <- 'home'
  temp$home <- colnames(temp)[2:(nteams+1)]
  
  if(str_detect(temp[2,2]," - ")){
     results <- gather(temp, away,result,2:(nteams+1)) %>%
       filter(str_detect(result," - ")) %>%
       separate(result,c("homegoals","awaygoals"),sep=" - ") %>%
       mutate(homegoals=as.numeric(homegoals),awaygoals=as.numeric(awaygoals), 
              delta = homegoals - awaygoals)
  } else {
  results <- gather(temp, away,result,2:(nteams+1)) %>%
    filter(str_detect(result,"\u2013")) %>%
    separate(result,c("homegoals","awaygoals"),sep="\u2013") %>%
    mutate(homegoals=as.numeric(homegoals),awaygoals=as.numeric(awaygoals), 
           delta = homegoals - awaygoals)
  }
  
  return(results)
  
}