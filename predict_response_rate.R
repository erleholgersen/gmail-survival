
### DESCRIPTION ###############################################################
# Model probability of getting a response within a specified time frame
#
#

### LIBRARIES #################################################################
library(stringr)
library(caret)
library(dplyr)
library(lime)
library(pROC)


set.seed(7);

rm(list = ls(all.names = TRUE));

load('data/all_outgoing.RData')
source('helper-functions/get_predictors.R')

### MAIN ######################################################################

# Time frame of interest, in days
cutoff <- 7;

# parse "to" field
# remove names, keep email addresses only and convert to lowercase
parsed.to <- sapply(
    km_data$to,
    function(field) {

        # wrapped in brackets
        if( grepl('<', field) ) {
            components <- sort( stringr::str_extract_all(field, '<[^<>]+>')[[1]] );
            parsed.field <- paste(components, collapse = ',')
        } else {
            parsed.field <- field;
        }
        return( tolower( gsub('<|>', '', parsed.field) )); 
        }
    );

# parse to field
km_data <- km_data %>% mutate(to = parsed.to);

# add features, and convert to binary classification problem
rf_data <- km_data %>%
    filter(time > cutoff || reply) %>%
    mutate(
        response = as.factor(ifelse(reply & time < cutoff, 'yes', 'no' )),
        to = forcats::fct_lump(to, 30)
        ) %>%
    select(response, to, message, subject) %>%
    get_predictors %>%
    select(-c(message, subject)) %>%
    na.omit;

X <- rf_data %>% select(-response);
Y <- rf_data %>% pull(response);

# train/test split
train_index <- createDataPartition(
    Y,
    p = 0.7,
    list = FALSE,
    times = 1
    );

train_X <- X[ train_index, ];
test_X <- X[-train_index, ];

train_Y <- Y[train_index];
test_Y <- Y[-train_index];

# Train random forest
rf_model <- train(
    x = train_X,
    y = train_Y,
    method = 'rf',
    tuneGrid = data.frame(mtry = 1:9),
    trControl = trainControl(method = 'cv')
    );

# save data
output_file <- file.path(
    'data', 
    hedgehog::datestamp.filename( paste0('model_response_', cutoff, '_days.RData') )
    )

save(
    train_X,
    test_X,
    train_Y,
    test_Y,
    rf_model,
    file = output_file
    )

cat('Wrote model and training/ testing data to file', output_file, '\n')

