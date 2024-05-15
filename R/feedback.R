#' MSA feedback (with score)
#'
#' Here the participant is given textual feedback at the end of the test.
#' @param dict The psychTestR dictionary used for internationalisation.
#' @export

#' @examples
#' \dontrun{
#' MSA_demo(feedback = MSA_feedback_with_score())}

MSA_feedback_with_score <- function(dict = MSA::MSA_dict) {
  psychTestR::new_timeline(
    psychTestR::reactive_page(function(state, ...) {
      #browser()
      results <- psychTestR::get_results(state = state,
                                         complete = TRUE,
                                         add_session_info = FALSE) %>% as.list()
      results2 <- psychTestR::get_results(state = state,
                                         complete = TRUE,
                                         add_session_info = FALSE) %>% as_tibble()
      #sum_score <- sum(purrr::map_lgl(results[[1]], function(x) x$correct))
      #num_question <- length(results[[1]])
      #messagef("Sum scores: %d, total items: %d", sum_score, num_question)
      # browser()
      if (is.null(results$MSA$score)) {
        num_correct <- sum(attr(results$MSA$ability, "metadata")$results$score)

        # here you can derive some metadata infos:
        # attr(results$MSA$ability, "metadata")$results
        # num_question <- nrow(results)

        # num_question <- results$MSA$num_questions # old1

        # strangely complicated code but should work
        num_question <- sum(count(attr(results$MSA$ability, "metadata")$results$score)$freq)

        print(attr(results$MSA$ability, "metadata")$results)
      }
      else {
        num_correct <- round(results$MSA$score * results$MSA$num_question)
        # num_question <- nrow(results)
        num_question <- results$MSA$num_questions
      }
      text_finish <- psychTestR::i18n("COMPLETED",
                                      html = TRUE,
                                      sub = list(num_question = num_question,
                                                 num_correct = num_correct))

      psychTestR::page(
        ui = shiny::div(
          shiny::p(text_finish),
          shiny::p(psychTestR::trigger_button("next", psychTestR::i18n("CONTINUE")))
        )
      )
    }
    ),
    dict = dict
  )
}

MSA_feedback_graph_normal_curve <- function(perc_correct, x_min = 40, x_max = 160, x_mean = 100, x_sd = 15) {

  # make the plot beautiful ;)
  theme_clean <- function() {
    ggplot2::theme_minimal(base_family = "") +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
            plot.title = ggplot2::element_text(face = "bold"),
            axis.title = ggplot2::element_text(face = "bold", size = "14"),
            axis.text = ggplot2::element_text(face = "bold", size = "12"),
            strip.text = ggplot2::element_text(face = "bold", size = ggplot2::rel(1), hjust = 0),
            strip.background = ggplot2::element_rect(fill = "grey80", color = NA),
            legend.title = ggplot2::element_text(face = "bold")
            # legend.text = ggplot2::element_text(face = "bold", size = "12")
            )
  }

  # some in-between calculations
  fake_IQ <- (x_max - x_min) * perc_correct + x_min
  fake_percentage <- round(1 - pnorm(fake_IQ, mean = x_mean, sd = x_sd, lower.tail = FALSE),
                           digits = 3) * 100
  x_axis_lab <- sprintf("%s %s %s %s", psychTestR::i18n("FEEDBACK_X1"), "\n", fake_percentage, psychTestR::i18n("FEEDBACK_X2"))
  main_title <- sprintf("%s - %s %s",
                                   psychTestR::i18n("TESTNAME"),
                                   psychTestR::i18n("VALUE"),
                                   psychTestR::i18n("FEEDBACK_TITLE1"))
  label_score <- sprintf("%s %s", psychTestR::i18n("SCORE_TEMPLATE"), round(fake_IQ, digits = 0))

  plot <-
    ggplot2::ggplot(data.frame(x = c(x_min, x_max)), ggplot2::aes(x)) +
    ggplot2::stat_function(fun = stats::dnorm, args = list(mean = x_mean, sd = x_sd),
                           alpha = 0.5,
                           fill = "lightblue4",
                           geom = "area") +
    ggplot2::stat_function(fun = stats::dnorm, args = list(mean = x_mean, sd = x_sd),
                           xlim = c(x_min, (x_max - x_min) * perc_correct + x_min),
                           fill = "lightblue4",
                           geom = "area") +
    ggplot2::scale_y_continuous(labels = scales::percent,
                                name = psychTestR::i18n("FEEDBACK_Y")) +
    ggplot2::scale_x_continuous(name = x_axis_lab,
                                breaks = c(55, 70, 85, 100, 115, 130, 145)) +
    # include MSA score within plot
    ggplot2::annotate("text",
                      x = fake_IQ,
                      y = 0.028,
                      label = label_score) +
    ggplot2::ggtitle(main_title) +
   # include dotted line
    ggplot2::geom_segment(
      ggplot2::aes(
          x=fake_IQ,
          xend = fake_IQ,
          y=0,
          yend=0.0268),
      linetype = "dotted") +
    # cool themes
    theme_clean()
    # ggokabeito::scale_fill_okabe_ito() + # super nice colours for colour-blind, but not essential
    # ggokabeito::scale_color_okabe_ito()

  shiny::div(plotly::ggplotly(plot, width = 600, height = 400),
             style = "margin-left:auto; margin-rigth:auto")
  # browser()

  # potential adaptations:
    # ggplot2::theme_bw() +
    # ggplot2::scale_x_continuous(position = "left")
    # q <- q + ggplot2::scale_y_continuous(labels = NULL)
    # x_axis_lab <- sprintf("This score indicates that you perform better than %s %s",
    #  fake_percentage, "percent of the test takers")
    # x_axis_lab <- sprintf("Your MSA %s is: %s; %s",
    #   psychTestR::i18n("VALUE"),
    #    round(fake_IQ,
    #     digits = 0),
    #     sprintf("This score indicates that you perform better than %s %s", fake_percentage, "percent of the test takers"))
}

#' MSA feedback (with graph)
#'
#' Here the participant is given textual and graphical feedback at the end of the test.
#' @param dict The psychTestR dictionary used for internationalisation.
#' @export
#' @examples
#' \dontrun{
#' MSA_demo(feedback = MSA_feedback_with_score())}

MSA_feedback_with_graph <- function(dict = MSA::MSA_dict) {
  # browser()
  psychTestR::new_timeline(
    psychTestR::reactive_page(function(state, ...) {

      results <- psychTestR::get_results(state = state,
                                         complete = TRUE,
                                         add_session_info = FALSE) %>% as.list()

      #sum_score <- sum(purrr::map_lgl(results[[1]], function(x) x$correct))
      #printf("Sum scores: %d, total items: %d perc_correct: %.2f", sum_score, num_question, perc_correct)
      # browser()
      if (is.null(results$MSA$score)) {
        num_correct <- sum(attr(results$MSA$ability, "metadata")$results$score)
        num_question <- results$MSA$num_items

        # fix this, this only translates the ability estimates as a quick and dirty version
        # need to include true normal distribution from brms!
        perc_correct <- (results$MSA$ability+4)/8
      }
      else {
        num_correct <- round(results$MSA$score * results$MSA$num_questions)
        num_question <- nrow(results)
        perc_correct <- num_correct/num_question
      }
      text_finish <- psychTestR::i18n("COMPLETED",
                                      html = TRUE,
                                      sub = list(num_question = num_question,
                                                 num_correct = num_correct))
      norm_plot <- MSA_feedback_graph_normal_curve(perc_correct)
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

