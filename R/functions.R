#' Loop Through Blog
#'
#' Loop throught all the posts of a users blog
#'
#' @param username Username of Blog Author
#'
#' @return Data Frame with Details of Blog Posts
#'
#' @examples
#' loopPosts()
#'
#' @export
loopPosts <- function(username){


  link = paste0(',"start_permlink":"',"",'"')
  query <- paste0('{"jsonrpc":"2.0", "method":"tags_api.get_discussions_by_author_before_date", "params":{"author":"',tolower(username),'","limit":100', link ,'}, "id":1}')
  results <- getPosts(query)
  count <- nrow(results)

  while (count == 100) {
    ##Add in logic here to skip calls if not finding any more results
    link = paste0(',"start_permlink":"',results[nrow(results), "permlink"],'"')
    query <- paste0('{"jsonrpc":"2.0", "method":"tags_api.get_discussions_by_author_before_date", "params":{"author":"',tolower(username),'","limit":100', link ,'}, "id":1}')
    tmpresults <- getPosts(query)
    count <- nrow(tmpresults)
    results <- rbind(results,tmpresults)
  }
  results <- results[!duplicated(results$permlink)]
  results[,comments:=children]
  results[,date:=as.Date(date)]
  return(results)
}

#' Format Number
#'
#' Format A Number as a Character with comma seperators
#'
#' @param x The numeric character
#'
#' @return character representation of number
#'
#' @examples
#' None
#'
#' @export
formatNum <- function(x){
  prettyNum(format(x, digits=2, nsmall=2), scientific=FALSE, big.mark=",")
}


#' Create Hyperlink
#'
#' add a hyperlink to the data table
#'
#' @param author username of blog author
#' @param link link to create
#' @param title title visible name for link
#'
#' @return Data Frame with Details of Blog Posts
#'
#' @examples
#' None
#'
#' @export
createLink <- function(author,link, title) {
  sprintf('<a href="https://steemit.com/@%s/%s" target="_blank">%s</a>',author, link, title)
}


#' Flatten List
#'
#' utility function to flatten a multi level list
#'
#' @param lst list to flatten
#'
#' @return flattened list
#'
#' @examples
#' None
#'
#' @export
flatten <- function(lst) {
  do.call(c, lapply(lst, function(x) if(is.list(x)) flatten(x) else list(x)))
}


#' Steem Reputation Formatter
#'
#' utility convert the Steem Reputation to a 2 digit number. Ported Code to R to format rep in readable format https://github.com/steemit/steem-js/blob/master/src/formatter.js
#'
#' @param rep long form value
#'
#' @return 2 digit value
#'
#' @examples
#' None
#'
#' @export
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

#' Get Steem Posts
#'
#' utility function to get the Steem posts from a user profile. This function is a wrapper to an RPC call.
#'
#' @param query get_discussions_by_author_before_date query
#'
#' @return list of blog posts
#'
#' @examples
#' None
#'
#' @export
getPosts <- function(query){
  ##query <- paste0('{"jsonrpc":"2.0", "method":"tags_api.get_discussions_by_author_before_date", "params":{"author":"',tolower("haejin"),'","limit":10' ,'}, "id":1}')

  ##query <- paste0('{"jsonrpc":"2.0", "method":"tags_api.get_discussions_by_author_before_date", "params":{"author":"',tolower("eroche"),'","limit":10' ,'}, "id":1}')

  r <- POST("https://api.steemit.com", body = query)
  data <- content(r, "parsed", "application/json")
  discussions <- data$result$discussions
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
                        depth=as.numeric(rep(NA,n)),
                        votes=as.numeric(rep(NA,n)),
                        voter=as.character(rep(NA,n)),
                        voter.rshares=as.character(rep(NA,n)),
                        voter.time=as.character(rep(NA,n)),
                        voter.percent=as.character(rep(NA,n)),
                        ##                      vote_details=as.list(rep(NA,n)),
                        children=as.numeric(rep(NA,n)),
                        pending_payout=as.numeric(rep(NA,n)),
                        paid_payout=as.numeric(rep(NA,n)),
                        image_count=as.numeric(rep(NA,n))
  )


  for(i in 1 : n) {
    discussion <- flatten(discussions[i])
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

    results[i,"depth"] <- discussion$depth
    results[i,"children"] <- discussion$children
    results[i,"votes"] <- sum(grepl("active_votes.voter", names(discussion)))
    results[i,"voter"] <- paste(unname(do.call(rbind, discussion[grepl("active_votes.voter", names(discussion))])),collapse=" ")
    results[i,"voter.rshares"] <- paste(unname(do.call(rbind, discussion[grepl("active_votes.rshares", names(discussion))])),collapse=" ")
    results[i,"voter.time"] <- paste(unname(do.call(rbind, discussion[grepl("active_votes.time", names(discussion))])),collapse=" ")
    results[i,"voter.percent"] <- paste(unname(do.call(rbind, discussion[grepl("active_votes.percent", names(discussion))])),collapse=" ")

    ##results[i, "vote_details"] <- do.call(rbind, (lapply(data$result$discussions[[i]]$active_votes, unlist)))

    results[i,"pending_payout"] <- as.numeric(discussion$pending_payout_value1)/1000

    results[i,"paid_payout"] <- as.numeric(discussion$total_payout_value1)/1000

  }

  results[,total_payout:= paid_payout + pending_payout]
  ##alias for total_payout
  results[,payout:= total_payout]

  results[,date:= strptime(results$created, format="%Y-%m-%dT%H:%M:%S")]

  results[, year_month:=format(date, "%Y-%b")]


  results[,author_rep:= reputation.formatter(author_reputation)]


  return(results)

}




