
### DESCRIPTION ###############################################################
# Improve a draft of an email to get a better predicted response rate.
#

### LIBRARIES #################################################################
library(caret)
library(dplyr)
library(lime)
library(pROC)

set.seed(7);

rm(list = ls(all.names = TRUE));
options(stringsAsFactors = TRUE, error = function() traceback(2) );

source('helper-functions/get_predictors.R')

load('data/2018-06-14_model_response_7_days.RData')

### FUNCTIONS #################################################################

# Input variables:
#   message     email message
#   subject     subject 
#   to          recipient
#   model       modelused to train
#
improve_email <- function(
    message,
    subject,
    to,
    model
    ) {

    # convert to model prediction format
    new_message <- get_predictors(
        message = message, 
        subject = subject,
        to = to
        )

    # make sure factor levels match
    new_message$to <- factor(
        new_message$to,
        levels = levels( model$trainingData$to )
        )

    prediction <- predict(
        model,
        newdata = new_message, 
        type = 'prob'
        );  
    
    prob_response <-  prediction[,2]
    cat('Estimated probability of response:', prob_response, '\n')

    explainer <- lime::lime(
        model$trainingData, 
        model
        )
    
    explanation <- lime::explain(
        new_message %>% select(-c(message, subject)),
        explainer,
        n_labels = 1, 
        n_features = 10
        );

    # figure out how email can be improved
    # assume recipient can't be changed
    explanation <- explanation[ 'to' != explanation$feature, ];
    
    # WIP
    reversal_key <- c(
        'question is false' = 'adding a question',
        'question is true' = 'removing all questions',
        'avg_paragraph_length <' = 'writing longer paragraphs',
        '< avg_paragraph_length' = 'writing shorter paragraphs',
        'n_exclamations <' = 'using more exclamation marks',
        '< n_exclamations' = 'using fewer exclamation marks',
        'question_subject is true' = 'removing all questions from the subject line',
        'question_subject is false' = 'adding a question to the subject line',
        'avg_words_sentence <' = 'writing longer sentences',
        '< avg_words_sentence' = 'writing shorter sentences',
        'n_words <' = 'writing a longer email',
        '< n_words' = 'shortening the email',
        'n_exclamations_subject <' = 'adding more exclamation marks to the subject line',
        '< n_exclamations_subject' = 'using fewer exclamation marks in the subject line',
        'n_smileys <' = 'using more smileys',
        '< n_smileys' = 'using fewer smileys',
        'n_paragraph_breaks <' = 'adding more paragraph breaks',
        '< n_paragraph_breaks' = 'using fewer paragraph breaks' 
        )
    
    # figure out features to fix
    if( prob_response < 0.5 ) {
        features_to_fix <- explanation$feature_desc[ explanation$feature_weight > 0 ]
    } else {
        features_to_fix <- explanation$feature_desc[ explanation$feature_weight < 0 ]
    }

    cat('Improve these chances by:\n')

    for( feature in features_to_fix ) {
        description_matches <- sapply(
            names(reversal_key),
            grepl,
            x = feature
            )

        if( sum(description_matches) == 1 ) {
            matched_description <- names(description_matches)[description_matches]
            cat('-', reversal_key[ matched_description ], '\n' )
        }
    }

    plot_features(explanation)
}

improve_email(
    message = ":):):):):):D:P:D",
    subject = 'Tralalalallalalalala!',
    to = 'Other',
    model = rf_model
    )





# plot_features(explanation);