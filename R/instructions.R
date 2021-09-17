info_page <- function(id, style = "text-align:justify; margin-left:20%;margin-right:20%") {
  psychTestR::one_button_page(shiny::div(psychTestR::i18n(id, html = TRUE),
                                         style = style),
                              button_text = psychTestR::i18n("CONTINUE"))
}

# two audio examples for instructions
# DE:"In diesem Beispiel spielt das Zielinstrument (Hauptstimme) in der Instrumentenmixtur!"
demo_sample1 <- "voc_Item2436_set462_wit_lvl0_comp3_t350_song5.wav"
# DE:"In diesem Musikausschnitt spielt das Zielinstrument (Hauptstimme) nicht in der Mixtur."
demo_sample2 <- "voc_Item3403_set877_wot_lvl0_comp4_t62_song94.wav"


instructions <- function(audio_dir) {
  c(
    psychTestR::code_block(function(state, ...) {
      psychTestR::set_local("do_intro", TRUE, state)
    }),
    info_page("INSTRUCTIONS"),
    show_sample_page(audio_dir),
    show_second_sample_page(audio_dir),
    psychTestR::while_loop(
      test = function(state, ...) psychTestR::get_local("do_intro", state),
      logic = practice(audio_dir)
    ),
    psychTestR::one_button_page(psychTestR::i18n("MAIN_INTRO"),
                                button_text = psychTestR::i18n("CONTINUE"))
  )
}

show_sample_page <- function(audio_dir){
  audio_url <- file.path(audio_dir, sprintf(demo_sample1))
  # audio_url <- file.path(audio_dir, sprintf("%s.mp3", demo_sample))
  audio <- get_audio_element(url = audio_url, autoplay = F)
  body <- shiny::div(
    shiny::div(psychTestR::i18n("SAMPLE1a"),
               style = "text-align: justify; margin-left:20%;
               margin-right:20%; margin-bottom:20px"),
    shiny::p(audio)
  )
  psychTestR::one_button_page(
    body = body,
    button_text = psychTestR::i18n("CONTINUE")
  )

}

show_second_sample_page <- function(audio_dir){
  audio_url <- file.path(audio_dir, sprintf(demo_sample2))
  # audio_url <- file.path(audio_dir, sprintf("%s.mp3", demo_sample))
  audio <- get_audio_element(url = audio_url, autoplay = F)
  body <- shiny::div(
    shiny::div(psychTestR::i18n("SAMPLE1b"),
               style = "text-align: justify; margin-left:20%;
               margin-right:20%; margin-bottom:20px"),
    shiny::p(audio)
  )
  psychTestR::one_button_page(
    body = body,
    button_text = psychTestR::i18n("CONTINUE")
  )

}
