#' Demo MSAT
#'
#' This function launches a demo for the RAT.
#'
#' @param num_items (Integer scalar) Number of items in the test.
#' @param feedback (Function) Defines the feedback to give the participant
#' at the end of the test. Defaults to a graph-based feedback page.
#' @param admin_password (Scalar character) Password for accessing the admin panel.
#' Defaults to \code{"demo"}.
#' @param researcher_email (Scalar character)
#' If not \code{NULL}, this researcher's email address is displayed
#' at the bottom of the screen so that online participants can ask for help.
#' Defaults to \email{longgold@gold.uc.ak},
#' the email address of this package's developer.
#' @param dict The psychTestR dictionary used for internationalisation.
#' @param language The language you want to run your demo in.
#' Possible languages include English (\code{"en"}), German (\code{"de"})).
#' The first language is selected by default
#' @param with_target_in_mix (Character scalar) Indicates how items are selected from the item pool.
#' Possible values are ("balanced") = equal proportion of items for this variable,
#' and ("random") =  pick items randomly; Default to "random"
#' @param target_instrument (Character scalar) Indicates how items are selected from the item pool.
#' Possible values are ("balanced") = equal proportion of items for this variable,
#' and ("random") =  pick items randomly; Default to "random"
#' @param complexity (Character scalar) Indicates how items are selected from the item pool.
#' Possible values are ("balanced") = equal proportion of items for this variable,
#' and ("random") =  pick items randomly; Default to "random"
#' @param level (Character scalar) Indicates how items are selected from the item pool.
#' Possible values are ("balanced") = equal proportion of items for this variable,
#' and ("random") =  pick items randomly; Default to "random"
#' @param ... Further arguments to be passed to \code{\link{MSAT}()}.
#' @export
#'
MSAT_demo <- function(num_items = 3L,
                     feedback = MSAT::MSAT_feedback_with_score(),
                     admin_password = "password",
                     researcher_email = "example@e-mail.com",
                     dict = MSAT::MSAT_dict,
                     language = "en",
                     with_target_in_mix = "random",
                     target_instrument = "random",
                     complexity = "random",
                     level = "random",
                     ...) {
  elts <- psychTestR::join(
    MSAT_welcome_page(dict = dict),
    MSAT::MSAT(num_items = num_items,
             with_welcome = FALSE,
             feedback = feedback,
             dict = dict,
             with_target_in_mix = with_target_in_mix,
             target_instrument = target_instrument,
             complexity = complexity,
             level = level,
             ...),
      MSAT_final_page(dict = dict)
  )

  psychTestR::make_test(
    elts,
    opt = psychTestR::test_options(title = "Musical Scene Analysis Test - Demo",
                                   admin_password = admin_password,
                                   researcher_email = researcher_email,
                                   demo = TRUE,
                                   languages = tolower(language)))
}
