
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

### FUNCTIONS #################################################################

get_predictors <- function(
    input_data = NULL,
    message = NULL,
    subject = NULL,
    to = NULL
    ) {

    if( is.null(input_data) & is.null(message) & is.null(subject) ) {
        stop('Must provide either input data or message/ subject')
    }

    if( is.null(input_data) ) {
        input_data <- data.frame(
            message = message,
            subject = subject,
            to = ifelse(!is.null(to), to, ''),
            stringsAsFactors = FALSE
            )
    }

    n_sentences <- sapply(
        input_data$message,
        function(x) length( tokenizers::tokenize_sentences(x)[[1]] )
        );

    predictors <- input_data %>% 
        mutate(
            n_words_subject = str_count(subject, '\\w+'),
            question_subject = grepl('\\?', subject),
            n_exclamations_subject = str_count(subject, '!'),
            n_smileys_subject = str_count(subject, ':\\)|:D|:p|:P|;\\)'),
            question = grepl('\\?', message), 
            n_exclamations = str_count(message, '!'),
            n_smileys = str_count(message, ':\\)|:D|:p|:P|;\\)'),
            n_paragraph_breaks = str_count(message, '(\\\r|\\\n)\\\n'),
            n_line_breaks = str_count(message, '\\\n'),
            n_words = str_count(message, '\\w+'),
            n_uppercase_words = str_count(message, '\\b[A-Z]{2,}\\b'),
            n_sentences = n_sentences,
            avg_word_length = nchar(message)/str_count(message, '\\w+'),
            avg_paragraph_length = n_words/n_paragraph_breaks,
            avg_words_sentence = n_words/n_sentences,
            n_links = str_count(message, 'http')
        ) %>% 
        mutate_if(is.numeric, funs(replace(., is.infinite(.) | is.na(.), 0) ) )


    return(predictors)

}



### MAIN ######################################################################

# Time frame of interest, in days
cutoff <- 1;

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
    method = 'rf'
    );

probabilities <- predict(
    rf_model,
    test_X,
    type = 'prob'
    )[, 2];

roc_object <- roc(test_Y, probabilities)

# get AUC
plot(roc_object);
print( ci(roc_object) );


new_message <- get_predictors(
    message = "How are you?", 
    subject = 'Hello!',
    to = 'Other'
    );

new_message$to <- factor(
    new_message$to,
    levels = levels(rf_data$to)
    )

prediction <- predict(
    rf_model,
    newdata = new_message, 
    type = 'prob'
    );    

explainer <- lime(
    train.X, 
    rf_model
    );

explanation <- lime::explain(
    new_message[, -c(1:2)],
    explainer,
    n_labels = 1, 
    n_features = 10
    );

plot_features(explanation);