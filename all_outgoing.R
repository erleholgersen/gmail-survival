
### DESCRIPTION ###############################################################
# Parse all outgoing email and save it as an RData object.
#
#
#

### LIBRARIES #################################################################
library(survival)
library(gmailr)
library(dplyr)

rm(list = ls(all.names = TRUE) )
options(stringsAsFactors = FALSE, error = function() traceback(2) )

source('get_thread.R')

### MAIN ######################################################################

my_email <- 'erle.holgersen@gmail.com';
query <- 'in:sent';

# get all thread IDs
all_messages <- messages(query)[[ 1 ]];
combined_thread_ids <- sapply(all_messages$messages, '[[', 'threadId');

i <- 1;
while( 100 == length(all_messages$messages) && i < 10 ) {
    cat('Page', i + 1, '\n');
    all_messages <- messages(
        query, 
        page_token = all_messages$nextPageToken
    )[[ 1 ]];
    combined_thread_ids <- c(
        combined_thread_ids, 
        sapply(all_messages$messages, '[[', 'threadId')
        );
    i <- i + 1;
}

# could have sent multiple emails in a single thread.. restrict to unique ones
combined_thread_ids <- unique(combined_thread_ids);

all_details <- list();

# loop over threads and add more detail about each email
for( thread_id in combined_thread_ids ) all_details[[ thread_id ]] <- get_thread(thread_id)

all_details <- do.call(rbind, all_details);

# turn into a KM data frame
km_data <- list();

for(thread_data in split(all_details, all_details$thread_id) ) {
    
    # get ID of thread
    thread_id <- thread_data$thread_id[1];
    
    # order by date from 
    thread_data <- thread_data[ order(thread_data$date), ];
    
    last_sent <- grepl(my_email, thread_data$from[1]);
    last_date <- thread_data$date[1];
    
    if( nrow(thread_data) > 1 ) {
        
        # loop over 
        for(i in 2:nrow(thread_data) ) {
            
            new_date <- thread_data$date[i];
            new_sent <- grepl(my_email, thread_data$from[i]);
            
            if( last_sent ) {
                km_data[[ paste0(thread_id, '_', as.character(i)) ]] <- data.frame(
                    date = last_date,
                    time = as.numeric(difftime(new_date, last_date, units = 'days')),
                    to = thread_data$to[i-1],
                    reply = !new_sent,
                    subject = thread_data$subject[i-1],
                    message = thread_data$message[i-1]
                );
            }
            
            last_date <- new_date;
            last_sent <- new_sent;
        }
    }
    
    # add final row if it was sent
    if( last_sent ) {

        km_data[[ paste0(thread_id, '_final') ]] <- data.frame(
            date = last_date,
            time = as.numeric(difftime(Sys.time(), last_date, units = 'days')),
            to = thread_data$to[ nrow(thread_data) ],
            reply = FALSE,
            subject = thread_data$subject[ nrow(thread_data) ],
            message = thread_data$message[ nrow(thread_data) ]
            )
    }
    
}

km_data <- do.call(rbind, km_data)

save(
    km_data,
    file = 'data/all_outgoing.RData'
    )

