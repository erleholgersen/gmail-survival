
#
# Input variables:
#   input_data
#   message
#   subject
#   to
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
            n_words_subject = stringr::str_count(subject, '\\w+'),
            question_subject = grepl('\\?', subject),
            n_exclamations_subject = stringr::str_count(subject, '!'),
            n_smileys_subject = stringr::str_count(subject, ':\\)|:D|:p|:P|;\\)'),
            question = grepl('\\?', message), 
            n_exclamations = stringr::str_count(message, '!'),
            n_smileys = stringr::str_count(message, ':\\)|:D|:p|:P|;\\)'),
            n_paragraph_breaks = stringr::str_count(message, '(\\\r|\\\n)\\\n'),
            n_line_breaks = stringr::str_count(message, '\\\n'),
            n_words = stringr::str_count(message, '\\w+'),
            n_uppercase_words = stringr::str_count(message, '\\b[A-Z]{2,}\\b'),
            n_sentences = n_sentences,
            avg_word_length = nchar(message)/stringr::str_count(message, '\\w+'),
            avg_paragraph_length = n_words/n_paragraph_breaks,
            avg_words_sentence = n_words/n_sentences,
            n_links = stringr::str_count(message, 'http')
        ) %>% 
        mutate_if(is.numeric, funs(replace(., is.infinite(.) | is.na(.), 0) ) )


    return(predictors)

}