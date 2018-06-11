#' get_emails
#' 
#' @description
#'  get information on all emails matching a query
#'
#' @param query  
#'  query to search for
#' @param max_pages
#'  max number of pages to get
#' @param by_thread 
#'  logical indicating whether to get emails by thread
#'
get_emails <- function(
    query, 
    max_pages = 1,
    by_thread = FALSE
    ) {

    # store all results
    combined_result <- list()

    # loop over pages and request results
    query_result <- list(nextPageToken = NULL)
    for( i in seq_len(max_pages) ) {
        query_result <- messages(
            query,
            page_token = query_result$nextPageToken
            )

        combined.results[[ i ]] <- query_result$messages

        if ( !('nextPageToken' %in% names(query_result)) ) break
    }

    combined_result <- unlist(combined_result)

    






}