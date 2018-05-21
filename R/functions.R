
# steemR user Functions ----
# These functions will be the main interface for users of the steemR package


#' getBlog
#'
#' Get all main posts from a users blog (excluding Resteems)
#'
#' @param username Username of Blog Author.
#'
#' @return Data.Table with Details of Blog Posts
#'
#' @examples
#' getBlog("eroche")
#'
#' @export
getBlog <- function(username){
  ##First Call to Retrieve Blogs Limits 100 Posts
  permlink <- ""
  results <- get_discussions_by_author_before_date(tolower(username), permlink)
  count <- length(results)

  ##Start at last permlink of Blog and get the next 100 posts. Loop until less than 100 are retrieved
  while (count == 100) {
    ##Add in logic here to skip calls if not finding any more results
    permlink <- results[[length(results)]]$permlink
    tmpresults <- get_discussions_by_author_before_date(tolower(username), permlink)
    count <- length(tmpresults)
    results <- c(results,tmpresults)
  }

  ##Convert the Raw Steem data to a cleaned data.table
  results <- cleanData(results)

  return(results)
}



# Steem API Calls ----
# These functions will be wrappers for the steem api calls

#Get the Steem posts from a user profile using the Steem API calls

get_discussions_by_author_before_date <- function(username, permlink){
  link = paste0(',"start_permlink":"',permlink,'"')
  query <- paste0('{"jsonrpc":"2.0", "method":"tags_api.get_discussions_by_author_before_date", "params":{"author":"',username,'","limit":100', link ,'}, "id":1}')

  r <- POST("https://api.steemit.com", body = query)
  data <- content(r, "parsed", "application/json")
  discussions <- data$result$discussions



  return(discussions)

}


# Get the latest steem account count from the Steem API calls
get_account_count <- function(){
  query <- '{"jsonrpc":"2.0", "method":"condenser_api.get_account_count", "params":[], "id":1}'
  r <- httr::POST("https://api.steemit.com", body = query)
  data <- httr::content(r, "parsed", "application/json")
  paste("Number of Accounts : ", data$result)
}

#Get the latest number of follow count for the specified user using the Steem API calls

get_follow_count <- function(user){
  query <- paste0('{"jsonrpc":"2.0", "method":"follow_api.get_follow_count", "params":{"account":"', user ,'"}, "id":1}')
  r <- httr::POST("https://api.steemit.com", body = query)
  data <- httr::content(r, "parsed", "application/json")
  print(paste("Number of Followers : ", data$result$follower_count))
  (paste("Number Following : ", data$result$following_count))
}


# Utility Functions ----
# These functions will process the data returned from the API calls and perform operations such as clensing ...

#Format a number as a character with comma seperators
formatNum <- function(x){
  prettyNum(format(x, digits=2, nsmall=2), scientific=FALSE, big.mark=",")
}


#Format and add a hyperlink to the data table

createLink <- function(author,link, title) {
  sprintf('<a href="https://steemit.com/@%s/%s" target="_blank">%s</a>',author, link, title)
}


#utility convert the Steem Reputation to a 2 digit number. Ported Code to R to format rep in readable format https://github.com/steemit/steem-js/blob/master/src/formatter.js

reputation.formatter <- function(rep) {
  neg = grepl("-", rep)
  rep <- sub("-","",rep)
  leadingDigits = as.numeric(substr(rep, 0, 4))
  log = log(leadingDigits) / log(10)
  n = nchar(rep) - 1
  out = n + (log - floor(log))
  out = ifelse( (out - 9)  > 0, out - 9, 0)
  out = ifelse(neg, -1 , 1) * out
  out = out * 9 + 25;
  return(out)
}

#utility to convert the Steem Data Dump to a nice Data.Frame with relevant fields for analysis

cleanData <- function(discussions){



  n <- length(discussions)

  results <- data.table(author=as.character(rep(NA,n)),
                        author_reputation=as.character(rep(NA,n)),
                        permlink=as.character(rep(NA,n)),
                        title=as.character(rep(NA,n)),
                        category=as.character(rep(NA,n)),
                        tag.1=as.character(rep(NA,n)),
                        tag.2=as.character(rep(NA,n)),
                        tag.3=as.character(rep(NA,n)),
                        tag.4=as.character(rep(NA,n)),
                        tag.5=as.character(rep(NA,n)),
                        tags=as.character(rep(NA,n)),
                        created=as.character(rep(NA,n)),
                        body=as.character(rep(NA,n)),
                        json=as.character(rep(NA,n)),
                        votes=as.numeric(rep(NA,n)),
                        voter=as.character(rep(NA,n)),
                        voter.rshares=as.character(rep(NA,n)),
                        voter.time=as.character(rep(NA,n)),
                        voter.percent=as.character(rep(NA,n)),
                        children=as.numeric(rep(NA,n)),
                        pending_payout=as.numeric(rep(NA,n)),
                        paid_payout=as.numeric(rep(NA,n)),
                        image_count=as.numeric(rep(NA,n))
  )


  for(i in 1 : n) {
    discussion <- discussions[[i]]
    results[i,"author"] <- discussion$author
    results[i,"author_reputation"] <- discussion$author_reputation
    results[i,"permlink"] <- discussion$permlink
    results[i,"title"] <- discussion$title
    results[i,"created"] <- discussion$created
    results[i,"body"] <- discussion$body
    results[i,"json"] <- discussion$json_metadata
    results[i,"image_count"] <- length(fromJSON(discussion$json_metadata)$image)

    skip=FALSE
    result = tryCatch({
      grepl("tags", results[i,"json"])
    }, error = function(e) {
      skip = TRUE
    })


    if(skip==FALSE) {
      if(grepl("tags", results[i,"json"])==TRUE){
        tags <- unlist(str_extract_all(results[i,"json"], '(\"tags\":)( )?((\\[(.*?)\\]))'))
        tags <- sub("(\"tags\":)( )?(\\[)", "", tags)
        tags <- gsub("\",( )*?\"", " ", tags)
        tags <- gsub("\"|\\]", "", tags)
        results[i, "tags"] <- tags

        ##Split tags and assign to 5 columns
        ## Assume only 5 tags max allowed, there may be more, so this does not cover all possibilities
        tags <- unlist(strsplit(tags," "))
        set(results,i,"category", tags[1])
        for(j in 1:5){
          if(is.na(tags[j])) {
            set(results,i,paste0("tag.",j), "NA")
          }
          else {
            set(results,i,paste0("tag.",j), tags[j])
          }

        }
      }
    }

##    results[i,"depth"] <- discussion$depth
    results[i,"children"] <- discussion$children

    votes <- data.table(do.call(rbind, discussion$active_votes))



    results[i,"votes"] <- nrow(votes)
    results[i,"voter"] <- paste(unlist(votes$voter),  collapse=' ')
    results[i,"voter.rshares"] <- paste(unlist(votes$rshares),  collapse=' ')
    results[i,"voter.time"] <- paste(unlist(votes$time),  collapse=' ')
    results[i,"voter.percent"] <- paste(unlist(votes$percent),  collapse=' ')

    ##results[i, "vote_details"] <- do.call(rbind, (lapply(data$result$discussions[[i]]$active_votes, unlist)))

    results[i,"pending_payout"] <- as.numeric(discussion$pending_payout_value[1])/1000

    results[i,"paid_payout"] <- as.numeric(discussion$total_payout_value[1])/1000

  }

  results[,total_payout:= paid_payout + pending_payout]

  results[,datetime:= strptime(results$created, format="%Y-%m-%dT%H:%M:%S")]
  results$created <- NULL
  results[,date:=as.Date(datetime)]
  results[, year_month:=format(date, "%Y-%b")]


  results[,author_rep:= reputation.formatter(author_reputation)]
  results$author_reputation <- NULL
  results[,comments:=children]
  results$children <- NULL


  ##Remove Duplicate posts
  results <- results[!duplicated(results$permlink)][order(-date)]

  return(results)

}


