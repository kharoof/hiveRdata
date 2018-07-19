# steemR user Functions ----
# These functions will be the main interface for users of the steemR package

#' getPost
#'
#' Get details of a post
#'
#' @param username Username of Blog Author.
#'
#' @param permlink Permlink of Blog Post
#'
#' @return List with Details of Post
#'
#' @examples
#' getPost("eroche")
#'
#' @export
getPost <- function(username, permlink){
  results <- get_content(username, permlink)

  ##Convert the Raw Steem data to a data.table and add a field with the number of images
  results <- results
  results$image_count <- length(fromJSON(results$json_metadata)$image)
  return(results)
}


#' getWitnesses
#'
#'@param limit Number of Witnesses to Return
#'
#' Get details of the witnesses
#'
#' @return List with Details of Witnesses
#'
#' @examples
#' getWitnesses() Get first 1000 Witnesses
#' getWitnesses(20) Get Top 20 Witnesses
#'
#' @export
getWitnesses <- function(limit=1000){
  results <- get_witnesses_by_vote(limit)

  ##Convert the Raw Steem data to a data.table and add a field with the number of images
  results <- data.frame(do.call(rbind, results))
  owner <- unlist(results$owner)
  version <- unlist(results$running_version)
  missed <- unlist(results$total_missed)
  votes <- unlist(results$votes)
  url <- unlist(results$url)
  created <- unlist(results$created)

  results <- data.frame(owner=owner, version=version, missed=missed, votes=votes, url=url, created=created)
  return(results)
}

#' getAccountVotes
#'
#'@param user Voting Account
#'
#' @return Data Table with list of votes and what they voted on
#'
#' @examples
#' getAccountVotes("eroche")
#'
#' @export
getAccountVotes <- function(user="eroche"){
  results <- get_account_votes(user)

  ##Convert the Raw Steem data to a data.table
  results <- data.frame(do.call(rbind, results))

  permlink <- paste0("@",unlist(results$authorperm))
  percent <- unlist(results$percent)
  time <- unlist(results$time)
  date <- as.Date(substr(time,1,10))
  time <- substr(time,12,25)

  results <- data.table(permlink=permlink, percent=percent, date = date, time=time, stringsAsFactors=F)
  return(results)
}

#' getDelegation
#'
#'@param user Account To Query
#'
#' @return Data Table with list of delegations
#'
#' @examples
#' getDelegation("eroche")
#'
#' @export
getDelegation <- function(user="eroche"){
  results <- get_vesting_delegations(user)

  ##Convert the Raw Steem data to a data.table
  results <- data.frame(do.call(rbind, results))

  delegator <- unlist(results$delegator)
  delegatee <- unlist(results$delegatee)
  vests <- unlist(results$vesting_shares)
  SP <- vests.formatter(vests)
  date <- as.Date(substr(results$min_delegation_time,1,10))
  if(length(vests)>0){
  results <- data.table(delegator=delegator, delegatee=delegatee, vests = vests, SP=SP, date=date, stringsAsFactors=F)}
  else{
    return(NULL)
  }
  return(results[order(date)])
}

#' getReplies
#'
#'@param user Account To Query
#'
#'@param permlink Account To Query
#'
#' @return Data Table with list of delegations
#'
#' @examples
#' getReplies("eroche", "data-wrangling-with-r")
#'
#' @export
getReplies <- function(user, permlink){
  results <- get_content_replies(user, permlink)

  comments <- length(results[1]$discussions)

  unlist(lapply(1:comments, function(x) results[1]$discussions[[x]]$author))

}



#' getTransactions
#'
#'@param user Account To Query
#'
#' @return The Last n number of transactions on an account
#'
#' @examples
#' getTransactions("eroche", 100)
#'
#' @export



getTransactions <- function(user,n=1000000){
  #n is maximum number of transactions

  loop=TRUE
  data = data.table(timestamp=character(n),
                    operation=character(n),
                    delegation.delegator=character(n),
                    delegation.delegatee=character(n),
                    vesting_shares=character(n), stringsAsFactors = F)


  if(n < 1000){
    x <- n-1
    y<- n-1
    print(paste(x,y))
    results <- get_account_history(user, x, y)
    for(j in 1:n){
      tryCatch({
        set(data,j,"timestamp",results[[j]][[2]]$timestamp)
        set(data,j,"operation",results[[j]][[2]]$op[[1]])
      }, error=function(e){})
      tryCatch({
        set(data,j,"delegation.delegator",results[[j]][[2]]$op[[2]]$delegator)
        set(data,j,"delegation.delegatee",results[[j]][[2]]$op[[2]]$delegatee)
        set(data,j,"vesting_shares",results[[j]][[2]]$op[[2]]$vesting_shares)
        set(data,j,"SP",vests.formatter(data[i*1000+j,"vesting_shares"]))
      },error=function(e){})
    }
  }

  if(n >=1000){
    i=0
  while (loop) {

    print(i)
    x <- 1000*i+999
    y <- 999
    print(paste(x,y))

    results <- get_account_history(user, x, y)

    for(j in 1:1000){
      tryCatch({
        set(data,i*1000+j,"timestamp",results[[j]][[2]]$timestamp)
        set(data,i*1000+j,"operation",results[[j]][[2]]$op[[1]])
        }, error=function(e){})
        tryCatch({
        set(data,i*1000+j,"delegation.delegator",results[[j]][[2]]$op[[2]]$delegator)
        set(data,i*1000+j,"delegation.delegatee",results[[j]][[2]]$op[[2]]$delegatee)
        set(data,i*1000+j,"vesting_shares",results[[j]][[2]]$op[[2]]$vesting_shares)
        set(data,i*1000+j,"SP",vests.formatter(data[i*1000+j,"vesting_shares"]))
      },error=function(e){})
    }
    tmp <- data[data$timestamp!=""]
    if(sum(duplicated(tmp))>1000){
      loop=FALSE}
    i = i +1

  }

  }

  data <- data[!duplicated(data$timestamp)]

return(data)
}

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

#' getPostsByTag
#'
#' Get posts using a specific tag in Chronological order
#'
#' @param tag tag to search.
#'
#' @param limit number of items to return
#'
#' @return Data.Table with Details of Posts
#'
#' @examples
#' getPostsByTag("letseat", 1)
#'
#' @export
getPostsByTag <- function(tag="steem", limit=1){
  results <- get_discussions_by_created(tag, limit)
  count <- length(results)


  ##Convert the Raw Steem data to a cleaned data.table
  results <- cleanData(results)

  return(results)
}


#' accountCount
#'
#' Get the count of the accounts on the Steem Blockchain
#'
#' @param NULL
#'
#' @return NULL
#'
#' @examples
#' accountCount()
#'
#' @export
accountCount <- function(){
  data <- get_account_count()
  paste("Number of Accounts : ", data$result)
}

#' getSteemProperties()
#'
#' Get the count of the accounts on the Steem Blockchain
#'
#' @param NULL
#'
#' @return NULL
#'
#' @examples
#' getSteemProperties()
#'
#' @export
getSteemProperties <- function(){
  data <- get_dynamic_global_properties()
  data <- cleanGlobalProperties(data$result)
  return(data)
}


#' getAccount
#'
#' Get the details of a user account
#'
#' @param username
#'
#' @return Account Details
#'
#' @examples
#' getAccount()
#'
#' @export
getAccount <- function(username){
  data <- get_account(username)
  data <- cleanAccount(data)
  return(data)
}


# Steem API Calls ----
# These functions will be wrappers for the steem api calls

#Get Delegations for an account
get_vesting_delegations <- function(user="eroche"){
  query <- paste0('{"jsonrpc":"2.0", "method":"condenser_api.get_vesting_delegations", "params":["',user,'",null,1000], "id":1}')

  r <- POST("https://api.steemit.com", body = query)
  data <- content(r, "parsed", "application/json")
  return(data$result)

}


#Get Votes by an Account
get_account_votes <- function(user="eroche"){
  query <- paste0('{"jsonrpc":"2.0", "method":"condenser_api.get_account_votes", "params":["',user,'"], "id":1}')

  r <- POST("https://api.steemit.com", body = query)
  data <- content(r, "parsed", "application/json")
  return(data$result)

}


#Get Posts filtered by a specific tag
get_discussions_by_created <- function(tag="steem", limit=100){
  query <- paste0('{"jsonrpc":"2.0", "method":"condenser_api.get_discussions_by_created", "params":[{"tag":"',tag,'","limit":',limit,'}], "id":1}')

  r <- POST("https://api.steemit.com", body = query)
  data <- content(r, "parsed", "application/json")
  return(data$result)

}


#Get the posts from a user profile
get_discussions_by_author_before_date <- function(username, permlink){
  link = paste0(',"start_permlink":"',permlink,'"')
  query <- paste0('{"jsonrpc":"2.0", "method":"tags_api.get_discussions_by_author_before_date", "params":{"author":"',username,'","limit":100', link ,'}, "id":1}')

  r <- POST("https://api.steemit.com", body = query)
  data <- content(r, "parsed", "application/json")
  discussions <- data$result$discussions
  return(discussions)

}

# Get the top Steem Witnesses by Vote
get_witnesses_by_vote <- function(limit=1000){
  query <- paste('{"jsonrpc":"2.0", "method":"condenser_api.get_witnesses_by_vote", "params":[null,',limit,'], "id":1}')
  r <- httr::POST("https://api.steemit.com", body = query)
  data <- httr::content(r, "parsed", "application/json")
  return(data$result)
}




# Get the latest steem account count from the Steem API calls
get_account_count <- function(){
  query <- '{"jsonrpc":"2.0", "method":"condenser_api.get_account_count", "params":[], "id":1}'
  r <- httr::POST("https://api.steemit.com", body = query)
  data <- httr::content(r, "parsed", "application/json")
  return(data)
}


# Get the latest steem global properties from the Steem API calls
get_dynamic_global_properties <- function(){
  query <- '{"jsonrpc":"2.0", "method":"database_api.get_dynamic_global_properties", "id":1}'
  r <- httr::POST("https://api.steemit.com", body = query)
  data <- httr::content(r, "parsed", "application/json")
  return(data)
}

# Get the latest steem rewards fund from the Steem API calls
get_rewards_fund <- function(){
  query <- '{"jsonrpc":"2.0", "method":"database_api.get_reward_funds", "id":1}'
  r <- httr::POST("https://api.steemit.com", body = query)
  data <- httr::content(r, "parsed", "application/json")
  paste(data$result)
}


#Get the latest number of follow count for the specified user using the Steem API calls

get_follow_count <- function(user){
  query <- paste0('{"jsonrpc":"2.0", "method":"follow_api.get_follow_count", "params":{"account":"', user ,'"}, "id":1}')
  r <- httr::POST("https://api.steemit.com", body = query)
  data <- httr::content(r, "parsed", "application/json")
  print(paste("Number of Followers : ", data$result$follower_count))
  (paste("Number Following : ", data$result$following_count))
}

#Get details of a users account
## The Steem API function allows multiple accounts to be returned. We are ignoring this for now.

get_account <- function(user){
  query <- paste0('{"jsonrpc":"2.0", "method":"condenser_api.get_accounts", "params":[["', user ,'"]], "id":1}')
  r <- httr::POST("https://api.steemit.com", body = query)
  data <- httr::content(r, "parsed", "application/json")
  data <- data$result[[1]]
  return(data)
}


#Get the authors that have been reblogged by the selected user using the Steem API calls

get_blog_authors <- function(user){
  query <- paste0('{"jsonrpc":"2.0", "method":"follow_api.get_blog_authors", "params":{"blog_account":"', user ,'"}, "id":1}')
  r <- httr::POST("https://api.steemit.com", body = query)
  data <- httr::content(r, "parsed", "application/json")
  data <- data.table::data.table(do.call(rbind, data$result$blog_authors))
  data <- data.table::data.table(reblogged=unlist(data$author), count=unlist(data$count))
  data <- data[order(-count)]
  return(data)
}

#Get the details of a specific post

get_content <- function(user, permlink){
  query <- paste0('{"jsonrpc":"2.0", "method":"tags_api.get_discussion", "params":{"author":"',user,'", "permlink":"',permlink,'"}, "id":1}')
  r <- httr::POST("https://api.steemit.com", body = query)
  data <- httr::content(r, "parsed", "application/json")
  return(data$result)
}


#Get the replies of a specific post

get_content_replies <- function(user, permlink){
  query <- paste0('{"jsonrpc":"2.0", "method":"tags_api.get_content_replies", "params":{"author":"',user,'", "permlink":"',permlink,'"}, "id":1}')
  r <- httr::POST("https://api.steemit.com", body = query)
  data <- httr::content(r, "parsed", "application/json")
  return(data$result)
}


## Get Account History

get_account_history <- function(user, start, end) {
  query <- paste0('{"jsonrpc":"2.0", "method":"condenser_api.get_account_history", "params":["',user,'",',start,',',end,'], "id":1}')
  r <- httr::POST("https://api.steemit.com", body = query)
  data <- httr::content(r, "parsed", "application/json")
  return(data[[2]])
}


## Get Reblogged by for a post

get_reblogged_by <- function(user, permlink) {
  query <- paste0('{"jsonrpc":"2.0", "method":"follow_api.get_reblogged_by", "params":{"author":"',user,'","permlink":"',permlink,'"}, "id":1}')
  r <- httr::POST("https://api.steemit.com", body = query)
  data <- httr::content(r, "parsed", "application/json")
  data <- unlist(data$result$accounts)
  return(data)
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


    results[i,"pending_payout"] <- tryCatch({ as.numeric(discussion$pending_payout_value$amount)/1000},error = function(e){ as.numeric(gsub("SBD", "", discussion$pending_payout_value))})

    results[i,"paid_payout"] <- tryCatch({ as.numeric(discussion$total_payout_value$amount)/1000},error=function(e){ as.numeric(gsub("SBD","",discussion$total_payout_value))})


  }

  results[,total_payout:= paid_payout + pending_payout]

  results[,datetime:= as.POSIXct(results$created, format="%Y-%m-%dT%H:%M:%S")]
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

## Convert Vests to Steem Power
vests.formatter <- function(x){
  conversionFactor <- getSteemProperties()$total_vesting_shares/getSteemProperties()$total_vesting_fund_steem / 1000
  return(as.numeric(gsub("VESTS", "", x))/conversionFactor)
}

##Clean accounts data, only keep key stuff
cleanAccount <- function(data){

  profile_image=""
  profile_website=""
  cover_image=""
  profile_about=""
  profile_location=""

  tryCatch({
    json <- fromJSON(data$json_metadata)
    profile_image <- json$profile$profile_image

    if (length(profile_image)==0) {profile_image=""}
      profile_location <- json$profile$location
    if (length(profile_location)==0) {profile_location=""}
      profile_website <- json$profile$profile_website
    if (length(profile_website)==0) {profile_website=""}
      profile_about <- json$profile$about
    if (length(profile_about)==0) {profile_about=""}
      cover_image <- json$profile$cover_image
    if (length(cover_image)==0) {cover_image=""}
  },error = function(e){
    }
  )

  data <- data.table(created=as.POSIXct(data$created, format="%Y-%m-%dT%H:%M:%S"),
                     rep=reputation.formatter(data$reputation),
                     post_count=data$post_count,
                     balance=as.numeric(gsub("STEEM", "", data$balance)),
                     savings=as.numeric(gsub("STEEM", "", data$savings_balance)),
                     vesting=vests.formatter( data$vesting_shares),
                     vesting_delegated=vests.formatter( data$delegated_vesting_shares),
                     vesting_received=vests.formatter( data$received_vesting_shares),
                     profile_links=paste(profile_image, profile_website, cover_image),
                     profile_about=profile_about,
                     profile_location = profile_location, stringsAsFactors = FALSE
                     )
  return(data)
}

##Clean Dynamic Global Properties
cleanGlobalProperties <- function(data){
  data <- data.table::data.table(head_block=data$head_block_number,
                     time=as.POSIXct(data$time, format="%Y-%m-%dT%H:%M:%S"),
                     current_witness=data$current_witness,
                     virtual_supply=as.numeric(data$virtual_supply[[1]]),
                     current_supply=as.numeric(data$current_supply[[1]]),
                     current_sbd_supply=as.numeric(data$current_sbd_supply[[1]]),
                     total_vesting_fund_steem=as.numeric(data$total_vesting_fund_steem[[1]]),
                     total_vesting_shares=as.numeric(data$total_vesting_shares[[1]]),
                     total_reward_fund_steem=as.numeric(data$total_reward_fund_steem[[1]]),
                     pending_rewarded_vesting_shares=as.numeric(data$pending_rewarded_vesting_shares[[1]]),
                     pending_rewarded_vesting_steem=as.numeric(data$pending_rewarded_vesting_steem[[1]])
                     )
  return(data)
}


