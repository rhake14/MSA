#' MSAT feedback (with score)
#'
#' Here the participant is given textual feedback at the end of the test.
#' @param dict The psychTestR dictionary used for internationalisation.
#' @export

#' @examples
#' \dontrun{
#' MSAT_demo(feedback = MSAT_feedback_with_score())}

MSAT_feedback_with_score <- function(dict = MSAT::MSAT_dict) {
  psychTestR::new_timeline(
    psychTestR::reactive_page(function(state, ...) {
      #browser()
      results <- psychTestR::get_results(state = state,
                                         complete = TRUE,
                                         add_session_info = FALSE) %>% as.list()
      #sum_score <- sum(purrr::map_lgl(results[[1]], function(x) x$correct))
      #num_question <- length(results[[1]])
      #messagef("Sum scores: %d, total items: %d", sum_score, num_question)
      # browser()
      if (is.null(results$MSAT$score)) {
        num_correct <- sum(attr(results$MSAT$ability, "metadata")$results$score)
        # num_question <- nrow(results)
        num_question <- results$MSAT$num_questions
      }
      else {
        num_correct <- round(results$MSAT$score * results$MSAT$num_question)
        # num_question <- nrow(results)
        num_question <- results$MSAT$num_questions
      }
      text_finish <- psychTestR::i18n("COMPLETED",
                                      html = TRUE,
                                      sub = list(num_question = num_question,
                                                 num_correct = num_correct))
      psychTestR::page(
        ui = shiny::div(
          shiny::p(text_finish)
        )
      )
    }
    ),
    dict = dict
  )
}

MSAT_feedback_graph_normal_curve <- function(perc_correct, x_min = 40, x_max = 160, x_mean = 100, x_sd = 15) {
  results <- psychTestR::get_results(state = state,
                                     complete = TRUE,
                                     add_session_info = FALSE) %>% as.list()
  num_correct <- round(results$MSAT$score * results$MSAT$num_question)
  num_question <- results$MSAT$num_questions
  perc_correct <- num_correct/num_question
  # browser()
  x <- NULL
  q <-
    ggplot2::ggplot(data.frame(x = c(x_min, x_max)), ggplot2::aes(x)) +
    ggplot2::stat_function(fun = stats::dnorm, args = list(mean = x_mean, sd = x_sd)) +
    ggplot2::stat_function(fun = stats::dnorm, args=list(mean = x_mean, sd = x_sd),
                           xlim = c(x_min, (x_max - x_min) * perc_correct + x_min),
                           fill = "lightblue4",
                           geom = "area")
  q <- q + ggplot2::theme_bw()
  #q <- q + scale_y_continuous(labels = scales::percent, name="Frequency (%)")
  #q <- q + ggplot2::scale_y_continuous(labels = NULL)
  x_axis_lab <- sprintf(" %s %s", psychTestR::i18n("TESTNAME"), psychTestR::i18n("VALUE"))
  title <- psychTestR::i18n("SCORE_TEMPLATE")
  fake_IQ <- (x_max - x_min) * perc_correct + x_min
  main_title <- sprintf("%s: %.0f", title, round(fake_IQ, digits = 0))

  q <- q + ggplot2::labs(x = x_axis_lab, y = "")
  q <- q + ggplot2::ggtitle(main_title)
  plotly::ggplotly(q, width = 600, height = 450)
}
#' MSAT feedback (with graph)
#'
#' Here the participant is given textual and graphical feedback at the end of the test.
#' @param dict The psychTestR dictionary used for internationalisation.
#' @export
#' @examples
#' \dontrun{
#' MSAT_demo(feedback = MSAT_feedback_with_score())}
MSAT_feedback_with_graph <- function(dict = MSAT::MSAT_dict) {
  psychTestR::new_timeline(
    psychTestR::reactive_page(function(state, ...) {
      #browser()
      results <- psychTestR::get_results(state = state,
                                         complete = TRUE,
                                         add_session_info = FALSE) %>% as.list()

      #sum_score <- sum(purrr::map_lgl(results[[1]], function(x) x$correct))
      #printf("Sum scores: %d, total items: %d perc_correct: %.2f", sum_score, num_question, perc_correct)
      #browser()
      if (is.null(results$MSAT$score)) {
        num_correct <- sum(attr(results$MSAT$ability, "metadata")$results$score)
        num_question <- results$MSAT$num_items
        perc_correct <- (results$MSAT$ability+4)/8
      }
      else {
        num_correct <- round(results$MSAT$score * results$MSAT$num_questions)
        num_question <- nrow(results)
        perc_correct <- num_correct/num_question
      }
      text_finish <- psychTestR::i18n("COMPLETED",
                                      html = TRUE,
                                      sub = list(num_question = num_question,
                                                 num_correct = num_correct))
      norm_plot <- MSAT_feedback_graph_normal_curve(perc_correct)
      psychTestR::page(
        ui = shiny::div(
          shiny::p(text_finish),
          shiny::p(norm_plot),
          shiny::p(psychTestR::trigger_button("next", psychTestR::i18n("CONTINUE")))
        )
      )
    }
    ),
    dict = dict
  )

}

