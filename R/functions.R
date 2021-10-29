# hiveRdata user Functions ----
# These functions will be the main interface for users of the HiveR package

#' List of Hive RPC nodes
#'
#' The functions in hiveRdata use public RPC nodes. This function returns a list of possible nodes which can be specified in the function calls.
#'
#' @param NULL
#'
#' @return List of nodes
#'
#'
#' @examples
#' getNodes()
#' @export
getNodes <- function(){
  nodes <- htmltab::htmltab("https://developers.hive.io/quickstart/hive_full_nodes.html",1)
  data.table::data.table(node=paste0("https://",nodes$URL), Owner=nodes$Owner)
}



#' Details of a Hive Post
#'
#' Get details of a post, specified by the unique link (username and permlink)
#'
#' @param username Username of Post Author.
#'
#' @param permlink Permlink of Post
#'
#' @param node Optional Argument (Hive Node to Query)
#'
#' @return List with Details of Post
#'
#'
#' @examples
#' getPost("eroche", "time-series-with-r")
#' getPost("eroche", "time-series-with-r", "https://api.openhive.network")
#'
#' @export
getPost <- function(username, permlink,node){
  if(missing(node)){
    node = "https://api.openhive.network"
  }
  results <- get_content(username, permlink,node)
  results=list(post=results)
results <- cleanData(results)
  ##Convert the Raw Hive data to a data.table and add a field with the number of images
results <- data.table(results)
  #results$image_count <- length(rjson::fromJSON(results$json_metadata)$image)
  return(results)
}





#' Top Hive Witnesses and Details
#'
#' Not all witnesses returned with this function will be active.
#'
#' @param limit Number of Witnesses to Return
#'
#' @param node Optional Argument (Hive Node to Query)
#'
#' Get details of the witnesses
#'
#' @return List with Details of Witnesses
#'
#' @examples
#' getWitnesses() Get first 1000 Witnesses
#' getWitnesses(20) Get Top 20 Witnesses
#' getWitnesses(20, node="https://api.openhive.network")
#'
#' @export
getWitnesses <- function(limit=1000, node){
  if(missing(node)){
    node = "https://api.openhive.network"
  }
  results <- get_witnesses_by_vote(limit, node)

  ##Convert the Raw Hive data to a data.table and add a field with the number of images
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

#' Current Trending Posts
#'
#' @param tag Tag to Filter
#'
#' @param limit Number of Posts to Return
#'
#' @param node Optional Argument (Hive Node to Query)
#'
#' Get details of the Trending Posts
#'
#' @return List with Details of Posts
#'
#' @examples
#' getTrending() Get first 100 Trending Posts
#' getTrending(20) Get Top 10 Trending Posts
#' getTrending(20, node="https://api.openhive.network")
#'
#' @export
getTrending <- function(tag="",limit=100, node){
  if(missing(node)){
    node = "https://api.openhive.network"
  }
  results <- get_discussions_by_trending(tag,limit, node)
results <- cleanData(results)
  return(results)
}

#' HP Delegations Made by an User
#'
#' @param user Account To Query
#'
#' @return Data Table with list of delegations
#'
#' @param node Optional Argument (Hive Node to Query)
#'
#' @examples
#' getDelegation("eroche")
#'
#' @export
getDelegations <- function(user="eroche", node){
  if(missing(node)){
    node = "https://api.openhive.network"
  }
  results <- get_vesting_delegations(user, node)

  ##Convert the Raw Hive data to a data.table
  results <- data.frame(do.call(rbind, results))

  delegator <- unlist(results$delegator)
  delegatee <- unlist(results$delegatee)
  vests <- unlist(results$vesting_shares)
  HP <- vests.formatter(vests)
  date <- as.Date(substr(results$min_delegation_time,1,10))

  if(length(vests)>0){
  results <- data.table(delegator=delegator, delegatee=delegatee, vests = vests, HP=HP, date=date, stringsAsFactors=F)
  return(results[order(date)])
  }
  else{
    print("No Delegations Found")
    #return(NULL)
  }

}




#' Blog Posts
#'
#' Get all main posts from a users blog (excluding reHives). This function may take some time to process depending on the size of the blog history on an account
#'
#' @param username Username of Blog Author.
#'
#' @param node Optional Argument (Hive Node to Query)
#'
#' @return Data.Table with Details of Blog Posts
#'
#' @examples
#' getBlog("eroche")
#'
#' @export
getBlogPosts <- function(username, node){
  if(missing(node)){
    node = "https://api.openhive.network"
  }
  ##First Call to Retrieve Blogs Limits 100 Posts
  permlink <- ""
  results <- get_discussions_by_author_before_date(tolower(username), permlink, node)
  count <- length(results)

  ##Start at last permlink of Blog and get the next 100 posts. Loop until less than 100 are retrieved
  while (count == 100) {
    ##Add in logic here to skip calls if not finding any more results
    permlink <- results[[length(results)]]$permlink
    tmpresults <- get_discussions_by_author_before_date(tolower(username), permlink, node)
    count <- length(tmpresults)
    results <- c(results,tmpresults)
  }

  ##Convert the Raw Hive data to a cleaned data.table
  results <- cleanData(results)

  return(results)
}


#' Recent Posts containing a particular tag.
#'
#' Get posts using a specific tag in Chronological order
#'
#' @param tag tag to search.
#'
#' @param limit number of items to return
#'
#' @param node Optional Argument (Hive Node to Query)
#'
#' @return Data.Table with Details of Posts
#'
#' @examples
#' getPostsByTag("letseat", 1)
#'
#' @export
getPostsByTag <- function(tag="Hive", limit=1, node){
  if(missing(node)){
    node = "https://api.openhive.network"
  }
  results <- get_discussions_by_created(tag, limit, node)
  count <- length(results)


  ##Convert the Raw Hive data to a cleaned data.table
  results <- cleanData(results)

  return(results)
}


#' Number of Accounts on Hive Blockchain
#'
#' Get the count of the number of accounts on the Hive Blockchain
#'
#' @param NULL
#'
#' @param node Optional Argument (Hive Node to Query)
#'
#' @return number of accounts
#'
#' @examples
#' accountCount()
#'
#' @export
accountCount <- function(node){
  if(missing(node)){
    node = "https://api.openhive.network"
  }
  data <- get_account_count(node)
  paste("Number of Accounts : ", data$result)
}

#' Hive Proporties
#'
#' Get details about the latest state of the Hive Blockchain.
#'
#' @param NULL
#'
#' @param node Optional Argument (Hive Node to Query)
#'
#' @return Hive properties
#'
#' @examples
#' getHiveProperties()
#'
#' @export
getHiveProperties <- function(node){
  if(missing(node)){
    node = "https://api.openhive.network"
  }
  data <- get_dynamic_global_properties(node)
  data <- cleanGlobalProperties(data)
  return(data)
}


#' USer Account Details
#'
#' Get the details of a user account
#'
#' @param username
#'
#' @param node Optional Argument (Hive Node to Query)
#'
#' @return Key Elements of Account Details; created, rep, post count, hive balance, savings balance, vesting
#'
#' @examples
#' getAccount("eroche")
#'
#' @export
getAccount <- function(username, node){
  if(missing(node)){
    node = "https://api.openhive.network"
  }
  data <- get_account(username, node)
  data <- cleanAccount(data)
  return(data)
}


# Hive API Calls ----
# These functions will be wrappers for the Hive api calls

# Delegations

#Get Delegations for an account
get_vesting_delegations <- function(user="eroche", node){
  query <- paste0('{"jsonrpc":"2.0", "method":"condenser_api.get_vesting_delegations", "params":["',user,'",null,1000], "id":1}')

  r <- httr::POST(node, body = query)
  data <- httr::content(r, "parsed", "application/json")
  return(data$result)

}





#Get Posts filtered by a specific tag
get_discussions_by_created <- function(tag="hive", limit=100, node){
  query <- paste0('{"jsonrpc":"2.0", "method":"condenser_api.get_discussions_by_created", "params":[{"tag":"',tolower(tag),'","limit":',limit,'}], "id":1}')

  r <- httr::POST(node, body = query)
  data <- httr::content(r, "parsed", "application/json")
  return(data$result)

}


#Get Trending Posts
get_discussions_by_trending <- function(tag="", limit=100, node){
  query <- paste0('{"jsonrpc":"2.0", "method":"condenser_api.get_discussions_by_trending", "params":[{"tag":"',tag,'","limit":',limit,'}], "id":1}')

  r <- httr::POST(node, body = query)
  data <- httr::content(r, "parsed", "application/json")
  return(data$result)

}


#Get the posts from a user profile
get_discussions_by_author_before_date <- function(username, permlink, node){
  link = paste0(',"start_permlink":"',permlink,'"')
  query <- paste0('{"jsonrpc":"2.0", "method":"condenser_api.get_discussions_by_author_before_date", "params":{"author":"',username,'","limit":100', link ,'}, "id":1}')

  r <- httr::POST(node, body = query)
  data <- httr::content(r, "parsed", "application/json")
  discussions <- data$result
  return(discussions)

}



# Get the top Hive Witnesses by Vote
get_witnesses_by_vote <- function(limit=1000, node){
  query <- paste('{"jsonrpc":"2.0", "method":"condenser_api.get_witnesses_by_vote", "params":[null,',limit,'], "id":1}')
  r <- httr::POST(node, body = query)
  data <- httr::content(r, "parsed", "application/json")
  return(data$result)
}


# Get the latest Hive account count from the Hive API calls
get_account_count <- function(node){
  query <- '{"jsonrpc":"2.0", "method":"condenser_api.get_account_count", "params":[], "id":1}'
  r <- httr::POST(node, body = query)
  data <- httr::content(r, "parsed", "application/json")
  return(data)
}

# Get the latest Hive global properties from the Hive API calls
get_dynamic_global_properties <- function(node){
  query <- '{"jsonrpc":"2.0", "method":"database_api.get_dynamic_global_properties", "id":1}'
  r <- httr::POST(node, body = query)
  data <- httr::content(r, "parsed", "application/json")
  return(data)
}

# Get the latest Hive rewards fund from the Hive API calls
get_rewards_fund <- function(node){
  query <- '{"jsonrpc":"2.0", "method":"database_api.get_reward_funds", "id":1}'
  r <- httr::POST(node, body = query)
  data <- httr::content(r, "parsed", "application/json")
  paste(data$result)
}


#Get the latest number of follow count for the specified user using the Hive API calls

get_follow_count <- function(user, node){
  query <- paste0('{"jsonrpc":"2.0", "method":"follow_api.get_follow_count", "params":{"account":"', user ,'"}, "id":1}')
  r <- httr::POST(node, body = query)
  data <- httr::content(r, "parsed", "application/json")
  print(paste("Number of Followers : ", data$result$follower_count))
  (paste("Number Following : ", data$result$following_count))
}

#Get details of a users account
## The Hive API function allows multiple accounts to be returned. We are ignoring this for now.

get_account <- function(user, node){
  query <- paste0('{"jsonrpc":"2.0", "method":"condenser_api.get_accounts", "params":[["', user ,'"]], "id":1}')
  r <- httr::POST(node, body = query)
  data <- httr::content(r, "parsed", "application/json")
  data <- data$result[[1]]
  return(data)
}


#Get the authors that have been reblogged by the selected user using the Hive API calls

get_blog_authors <- function(user, node){
  query <- paste0('{"jsonrpc":"2.0", "method":"follow_api.get_blog_authors", "params":{"blog_account":"', user ,'"}, "id":1}')
  r <- httr::POST(node, body = query)
  data <- httr::content(r, "parsed", "application/json")
  data <- data.table::data.table(do.call(rbind, data$result$blog_authors))
  data <- data.table::data.table(reblogged=unlist(data$author), count=unlist(data$count))
  data <- data[order(-count)]
  return(data)
}

#Get the details of a specific post

get_content <- function(user, permlink, node){
  query <- paste0('{"jsonrpc":"2.0", "method":"tags_api.get_discussion", "params":{"author":"',user,'", "permlink":"',permlink,'"}, "id":1}')
  r <- httr::POST(node, body = query)
  data <- httr::content(r, "parsed", "application/json")
  return(data$result)
}


#Get the replies of a specific post

get_content_replies <- function(user, permlink, node){
  query <- paste0('{"jsonrpc":"2.0", "method":"tags_api.get_content_replies", "params":{"author":"',user,'", "permlink":"',permlink,'"}, "id":1}')
  r <- httr::POST(node, body = query)
  data <- httr::content(r, "parsed", "application/json")
  return(data$result)
}


## Get Account History

get_account_history <- function(user, start, end, node) {
  query <- paste0('{"jsonrpc":"2.0", "method":"condenser_api.get_account_history", "params":["',user,'",',start,',',end,'], "id":1}')
  r <- httr::POST(node, body = query)
  data <- httr::content(r, "parsed", "application/json")
  return(data[[2]])
}


## Get Reblogged by for a post

get_reblogged_by <- function(user, permlink, node) {
  query <- paste0('{"jsonrpc":"2.0", "method":"follow_api.get_reblogged_by", "params":{"author":"',user,'","permlink":"',permlink,'"}, "id":1}')
  r <- httr::POST(node, body = query)
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
  sprintf('<a href="https://Hiveit.com/@%s/%s" target="_blank">%s</a>',author, link, title)
}


#utility convert the Hive Reputation to a 2 digit number. Ported Code to R to format rep in readable format https://github.com/Hiveit/Hive-js/blob/master/src/formatter.js

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

#utility to convert the Hive Data Dump to a nice Data.Frame with relevant fields for analysis

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
    results[i,"image_count"] <- tryCatch({length(rjson::fromJSON(discussion$json_metadata)$image)},error = function(e){ 0 })



#    results[i,"body"] <- discussion$body

#    results[i,"json"] <- discussion$json_metadata
#    results[i,"image_count"] <- length(rjson::fromJSON(discussion$json_metadata)$image)

    skip=FALSE

    result = tryCatch({
      grepl("tags", results[i,"json"])
    }, error = function(e) {
      skip = TRUE
    })


    if(skip==FALSE) {
      if(grepl("tags", results[i,"json"])==TRUE){
        tags <- unlist(stringr::str_extract_all(results[i,"json"], '(\"tags\":)( )?((\\[(.*?)\\]))'))
        tags <- sub("(\"tags\":)( )?(\\[)", "", tags)
        tags <- gsub("\",( )*?\"", " ", tags)
        tags <- gsub("\"|\\]", "", tags)
        results[i, "tags"] <- tags[[1]]

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
    #discussion$active_votes <- list(0) # Temporarily add this as we are getting an error debug later
    votes <- data.table(do.call(rbind, discussion$active_votes))

    #REmove votes where R Shares == 0
    votes <- votes[rshares>0]

    results[i,"votes"] <- nrow(votes)
    results[i,"voter"] <- paste(unlist(votes$voter),  collapse=' ')
    results[i,"voter.rshares"] <- paste(unlist(votes$rshares),  collapse=' ')
    results[i,"voter.time"] <- paste(unlist(votes$time),  collapse=' ')
    results[i,"voter.percent"] <- paste(unlist(votes$percent),  collapse=' ')

    ##results[i, "vote_details"] <- do.call(rbind, (lapply(data$result$discussions[[i]]$active_votes, unlist)))


    results[i,"pending_payout"] <- tryCatch({ as.numeric(discussion$pending_payout_value$amount)/1000},error = function(e){ as.numeric(gsub("HBD", "", discussion$pending_payout_value))})

    results[i,"paid_payout"] <- tryCatch({ as.numeric(discussion$total_payout_value$amount)/1000},error=function(e){ as.numeric(gsub("HBD","",discussion$total_payout_value))})


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

## Convert Vests to Hive Power
vests.formatter <- function(x){
  conversionFactor <- getHiveProperties()$total_vesting_shares/getHiveProperties()$total_vesting_fund_Hive / 1000
  return(as.numeric(gsub("VESTS", "", x))/conversionFactor)
}

##Clean accounts data, only keep key stuff.
cleanAccount <- function(data){

  profile_image=""
  profile_website=""
  cover_image=""
  profile_about=""
  profile_location=""

  tryCatch({
    json <- rjson::fromJSON(data$json_metadata)
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
                     balance=as.numeric(gsub("HIVE", "", data$balance)),
                     savings=as.numeric(gsub("HIVE", "", data$savings_balance)),
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
  data <- data.table::data.table(stack(data$result))
  return(data)
}


