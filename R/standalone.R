#source("R/MSA.R")
options(shiny.error = browser)
debug_locally <- !grepl("shiny-server", getwd())


#' Standalone MSA
#'
#' This function launches a standalone testing session for the MSA.
#' This can be used for data collection, either in the laboratory or online.
#' @param title (Scalar character) Title to display during testing.
#' @param num_items (Scalar integer) Number of items to be adminstered.
#' @param with_id (Scalar boolean) Indicates, if ID should be asked for. Defaults to TRUE
#' @param with_feedback (Scalar boolean) Indicates if performance feedback will be given at the end of the test. Defaults to  FALSE
#' @param with_welcome (Scalar boolean) Indicates, if a welcome page shall be displayed. Defaults to TRUE
#' @param take_training (Scalar boolean) Enable practice session before the actual session. Defaults to FALSE
#' @param admin_password (Scalar character) Password for accessing the admin panel.
#' @param researcher_email (Scalar character)
#' If not \code{NULL}, this researcher's email address is displayed
#' at the bottom of the screen so that online participants can ask for help.
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include English (\code{"en"}) and German (\code{"de"}).
#' The first language is selected by default
#' @param dict The psychTestR dictionary used for internationalisation.
#' @param validate_id (Character scalar or closure) Function for validating IDs or string "auto" for default validation
#' which means ID should consist only of  alphanumeric characters.
#' @param balance_over (Character vector) Indicates how items are selected from the item pool. Balance means that the proportion of items for each parameter is equal.
#' Possible parameters are
#' "target_instrument": the target instrument; balancing = equal proportion of the four different instruments ('Lead Voice', 'Piano', 'Guitar', 'Bass').
#' "complexity": the musical complexity, i.e. number of instruments within the mixture; balancing = equal proportion of items with 'three' and 'six' instruments playing in the mixture.
#' "level": the level-ratio between target and the mixture; balancing = equal proportion of items with '0', '-5', '-10', '-15' level-ratios.
#' Default is a fully balanced design: c("target_instrument", "complexity", "level").
#' Note: By default, there is always an equal proportion of "with target instrument" and "without target" items in the pool.
#' @param ... Further arguments to be passed to \code{\link{MSA}()}.
#' @export


MSA_standalone  <- function(title = NULL,
                           num_items = 18L,
                           with_id = TRUE,
                           with_feedback = TRUE,
                           with_welcome = TRUE,
                           take_training = FALSE,
                           admin_password = "password",
                           researcher_email = "put.your.email-adress@here",
                           languages = c("en", "de"),
                           dict = MSA::MSA_dict,
                           validate_id = "auto",
                           balance_over = c("target_instrument", "complexity", "level"),
                           ...) {
  feedback <- NULL
  # key <- NULL
  if (with_feedback) {
    # feedback <- MSA::MSA_feedback_with_graph()
    feedback <- MSA::MSA_feedback_with_score()
    # feedback <- MSA::MSA_feedback_graph_normal_curve()
  }
  elts <- psychTestR::join(
    if (with_id)
      psychTestR::new_timeline(
        psychTestR::get_p_id(prompt = psychTestR::i18n("ENTER_ID"),
                             button_text = psychTestR::i18n("CONTINUE"),
                             validate = validate_id),
        dict = dict),
    if (take_training)
      MSA::MSA(num_items = num_items,
               with_welcome =  FALSE,
               with_finish = FALSE,
               feedback = feedback,
               dict = dict,
               with_feedback = with_feedback,
               take_training = TRUE,
               balance_over = balance_over,
               # adaptive = adaptive, ## future proof
               ...)
    else
      # if(with_welcome) MSA_welcome_page(dict = dict),
      MSA::MSA(
        num_items = num_items,
        with_welcome =  with_welcome,
        with_finish = FALSE,
        feedback = feedback,
        with_feedback = with_feedback,
        dict = dict,
        take_training = FALSE,
        balance_over = balance_over,
        ...
      ),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    MSA_final_page(dict = dict)
  )
  if (is.null(title)) {
    #extract title as named vector from dictionary
    title <-
      MSA::MSA_dict  %>%
      as.data.frame() %>%
      dplyr::filter(key == "TESTNAME") %>%
      dplyr::select(-key) %>%
      as.list() %>%
      unlist()
    names(title) <- tolower(names(title))
  }

  psychTestR::make_test(
    elts,
    opt = psychTestR::test_options(title = title,
                                   admin_password = admin_password,
                                   researcher_email = researcher_email,
                                   demo = FALSE,
                                   languages = tolower(languages)))
}

