### LIBRARIES #################################################################
library(randomForestSRC)
library(tidytext)
library(survival)
library(stringr)
library(splines)
library(dplyr)


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
            to = to
            )
    }


    predictors <- input_data %>% 
        mutate(
            n_words_subject = str_count(message, '\\w+'),
            n_questions = str_count(message, '\\?'), 
            n_exclamations = str_count(message, '!'),
            n_smileys = str_count(message, ':\\)|:D'),
            n_whitespace = str_count(message, '\\\r'),
            n_words = str_count(message, '\\w+'),
            avg_word_length = nchar(message)/str_count(message, '\\w+')
        ) %>% 
        mutate_if(is.numeric, funs(replace(., is.infinite(.) | is.na(.), 0) ) )


    return(predictors)

}




### MAIN ######################################################################

# add features
km_data <- km_data %>%
    mutate(
        n_words_subject = str_count(message, '\\w+'),
        n_questions = str_count(message, '\\?'), 
        n_exclamations = str_count(message, '!'),
        n_smileys = str_count(message, ':\\)|:D'),
        n_whitespace = str_count(message, '\\\r'),
        n_words = str_count(message, '\\w+'),
        avg_word_length = nchar(message)/str_count(message, '\\w+')
    ) %>% 
    mutate_if(is.numeric, funs(replace(., is.infinite(.) | is.na(.), 0) ) );

# km_tokens <- km_data %>%
#     unnest_tokens(word, message)

# word_counts <- km_tokens %>%
#     # change this to message ID
#     count(date, word, sort = TRUE) %>%
#     ungroup() %>% 
#     rename(count = n) %>%
#     bind_tf_idf(word, date, count)

# dtm <- word_counts %>% 
#     cast_dtm(date, word, count)


# figure out top email recipients
top_recipients <- km_data %>% 
    group_by( to = tolower(to) ) %>%
    summarize(n = n()) %>%
    arrange(-n)

cox_model <- coxph(
    Surv(time, reply) ~ subject_length + n_questions + n_exclamations + n_smileys + n_whitespace + avg_word_length + n_words + strata(to),
    km_data
)

rf_data <- km_data %>%
    select(time, reply, length, subject_length, n_questions, n_exclamations, n_smileys, n_whitespace, avg_word_length, n_words)

rf_model <- rfsrc(
    Surv(time, reply) ~ length + subject_length + n_questions + n_exclamations + n_smileys + n_whitespace + avg_word_length + n_words,
    rf_data,
    importance = TRUE
    );