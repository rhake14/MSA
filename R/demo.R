#' Demo MSA
#'
#' This function launches a demo for the MSA.
#'
#' @param num_items (Integer scalar) Number of items in the test.
#' @param feedback (Function) Defines the feedback to give the participant
#' at the end of the test. Defaults to a graph-based feedback page.
#' @param admin_password (Scalar character) Password for accessing the admin panel.
#' Defaults to \code{"demo"}.
#' @param researcher_email (Scalar character)
#' If not \code{NULL}, this researcher's email address is displayed
#' at the bottom of the screen so that online participants can ask for help.
#' Defaults to \email{robin.hake@uni-oldenburg.de},
#' the email address of this package's developer.
#' @param dict The psychTestR dictionary used for internationalisation.
#' @param language The language you want to run your demo in.
#' Possible languages include English (\code{"en"}), German (\code{"de"}), formal German (\code{"de"}), and French (\code{"fr"})).
#' The first language is selected by default
#' @param adaptive (Scalar boolean) Indicates whether you want to use the adaptive MSA2 (TRUE)
#' or the non-adaptive MSA (FASLE). Default is adaptive = TRUE.
#' @param long_version (Scalar boolean) Indicates whether you want to use the musical excerpts that include 8 seconds of music prior to the task (TRUE):
#' If (TRUE), each trail has the following procedure:
#' Part 1: the initial music segment (8s);
#' Part 2: after 1s pause, a single instrument (lead voice, bass, guitar, or piano) plays;
#' Part 3: after 1s pause, a mix of instruments plays, possibly including the target instrument.
#' If (FALSE), Part 1 is skipped (the original version; Default).
#' @param ... Further arguments to be passed to \code{\link{MSA}()}.
#' @export

MSA_demo <- function(num_items = 4L,
                     feedback = MSA::MSA_feedback_with_score(),
                     admin_password = "password",
                     researcher_email = "example@e-mail.com",
                     dict = MSA::MSA_dict,
                     language = "en",
                     adaptive = TRUE,
                     long_version = FALSE,
                     ...) {
  elts <- psychTestR::join(
    MSA_welcome_page(dict = dict),
    MSA::MSA(num_items = num_items,
             with_welcome = FALSE,
             feedback = feedback,
             dict = dict,
             long_version = long_version,
             adaptive = adaptive,
             ...),
      MSA_final_page(dict = dict)
  )

  psychTestR::make_test(
    elts,
    opt = psychTestR::test_options(title = "Musical Scene Analysis Test - Demo",
                                   admin_password = admin_password,
                                   researcher_email = researcher_email,
                                   demo = TRUE,
                                   languages = tolower(language)))
}
