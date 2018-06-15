
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

load('data/2018-06-15_model_response_7_days.RData')

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
        # subject
        'n_words_subject <' = 'using a longer subject line',
        '< n_words_subject' = 'shortening the subject line',
        'question_subject is true' = 'removing all questions from the subject line',
        'question_subject is false' = 'adding a question to the subject line',
        'n_exclamations_subject <' = 'adding more exclamation marks to the subject line',
        '< n_exclamations_subject' = 'using fewer exclamation marks in the subject line',
        'n_smileys_subject <' = 'using more smileys in the subject line',
        '< n_smileys_subject' = 'using fewer smileys in the subject line',
        # message
        'question is false' = 'adding a question',
        'question is true' = 'removing all questions',
        'n_exclamations <' = 'using more exclamation marks',
        '< n_exclamations' = 'using fewer exclamation marks',
        'n_smileys <' = 'using more smileys',
        '< n_smileys' = 'using fewer smileys',
        'n_paragraph_breaks <' = 'adding more paragraph breaks',
        '< n_paragraph_breaks' = 'using fewer paragraph breaks',
        'n_line_breaks <' = 'adding more line breaks',
        '< n_line_breaks' = 'using fewer line breaks',
        'n_words <' = 'writing a longer email',
        '< n_words' = 'shortening the email',
        'n_uppercase_words <' = 'adding more uppercase words',
        '< n_uppercase_words' = 'using fewer uppercase words',
        'n_sentences <' = 'adding more sentences',
        '< n_sentences' = 'removing some sentences',
        'avg_word_length <' = 'using longer words',
        '< avg_word_length' = 'using shorter words',
        'avg_paragraph_length <' = 'writing longer paragraphs',
        '< avg_paragraph_length' = 'writing shorter paragraphs',
        'avg_words_sentence <' = 'writing longer sentences',
        '< avg_words_sentence' = 'writing shorter sentences',
        'n_links <' = 'including more links',
        '< n_links' = 'including fewer links'
        )

       
    
    # figure out features to fix
    if( prob_response < 0.5 ) {
        features_to_fix <- explanation$feature_desc[ explanation$feature_weight > 0 ]
    } else {
        features_to_fix <- explanation$feature_desc[ explanation$feature_weight < 0 ]
    }

    # remove any that are "in range" – want clear-cut higher or lower
    features_to_fix <- features_to_fix[ stringr::str_count(features_to_fix, '<') < 2 ];

    cat('Improve these chances by:\n')

    for( feature in features_to_fix[1:3] ) {
        # get mathched improvement suggestion
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

    return(explanation);
}

explanation <- improve_email(
    message = "http://clincancerres.aacrjournals.org/content/24/11/2530",
    subject = 'Inferring IDH1 mutation status from imaging data',
    to = 'Other',
    model = rf_model
    )
