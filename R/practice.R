#### Version 1 ----------------------------------------------------------------

### example stimuli for practice/training session
sample_data <- list(
  label =
    c(
      "voc_Item3567_set948_wit_lvl0_comp6_t230_song14",
      "b_Item501_set206_wit_lvl0_comp3_t310_song18",
      "git_Item1536_set469_wit_lvl-15_comp3_t158_song46",
      "voc_Item2436_set462_wit_lvl0_comp3_t350_song5",
      "voc_Item2330_set427_wot_lvl0_comp7_t100_song19-exception",
      "voc_Item3228_set786_wot_lvl0_comp4_t32_song16"
    ),
  sample_audios =
    c(
      "voc_Item3567_set948_wit_lvl0_comp6_t230_song14.wav",
      "b_Item501_set206_wit_lvl0_comp3_t310_song18.wav",
      "git_Item1536_set469_wit_lvl-15_comp3_t158_song46.wav",
      "voc_Item2436_set462_wit_lvl0_comp3_t350_song5.wav",
      "voc_Item2330_set427_wot_lvl0_comp7_t100_song19.wav",
      "voc_Item3228_set786_wot_lvl0_comp4_t32_song16.wav"
    ),
  sample_audios_answers = c(1, 1, 1, 1, 1, 2)
) %>% as_tibble() %>% dplyr::slice_sample(n = 6) %>%
  as.list() # randomize list


ask_repeat <- function(prompt) {
  # browser()
  psychTestR::NAFC_page(
    label = "ask_repeat",
    prompt = prompt,
    choices = c("go_back", "continue"),
    labels = lapply(c("GOBACK", "CONTINUE"), psychTestR::i18n),
    save_answer = FALSE,
    arrange_vertically = FALSE,
    on_complete = function(state, answer, ...) {
      psychTestR::set_local("do_intro", identical(answer, "go_back"), state)
    }
  )
  # browser()
}

make_practice_page <- function(page_no, audio_dir) {
  psychTestR::reactive_page(function(answer, ...) {
    # browser()
    correct <- "INCORRECT"
    if (length(answer) > 2) {
      answer = answer[-1,] # Fix this: gets "NA" otherswise...
    }

    if (page_no > 1 && answer$correct) {
      correct <- "CORRECT"}
    feedback <- psychTestR::i18n(correct)
    get_practice_page(page_no, feedback, audio_dir)
    # browser()
  })
}

# shiny::div(
#   shiny::div(psychTestR::i18n("SAMPLE1b"),
#              style = "text-align: justify; margin-left:20%;
#                margin-right:20%; margin-bottom:20px; display:block"),
#   shiny::p(audio)
# )


get_practice_page <- function(page_no, feedback, audio_dir) {
  # browser()
  key <- sprintf("PRACTICE%d", page_no)
  if (page_no == 5) {
    # browser()
    key <- "TRANSITION"
  }
  prompt <- shiny::div(
    psychTestR::i18n(key, html = T, sub = list(feedback = feedback)),
    style = "text-align: justify; margin-left:20%;margin-right:20%; margin-bottom:20px; display:block")

  if (page_no == 5) {
    page <- ask_repeat(prompt)
  }
  else{
    # browser()
    page <- MSA_item(
      # label = sprintf("training%s", page_no),
      label = sample_data$label[[page_no]],
      correct_answer = sample_data$sample_audios_answers[page_no],
      prompt = prompt,
      audio_dir = audio_dir,
      audio_file = sample_data$sample_audios[[page_no]],
      adaptive = FALSE,
      # adaptive = TRUE,
      save_answer = FALSE,
      instruction_page = FALSE
    )

  }
  # browser()
  page
}

practice <- function(audio_dir) {
  lapply(1:5, make_practice_page, audio_dir) %>% unlist()
}


###### Version Long --------------------------------------------------------------

## example stimuli for practice/training session
sample_data_long <- list(
  label =
    c(
      "bass_long_wit_level(-10)_complexity(3)_time(18)_songnr(80)",
      "lead_long_wit_level(0)_complexity(3)_time(122)_songnr(20)",
      "bass_long_wit_level(0)_complexity(6)_time(180)_songnr(37)",
      "lead_long_wit_level(0)_complexity(3)_time(52)_songnr(93)",
      "lead_long_wit_level(-10)_complexity(6)_time(102)_songnr(49)",
      "bass_long_wit_level(0)_complexity(6)_time(138)_songnr(49)"
    ),
  sample_audios =
    c(
      "bass_long_wit_level(-10)_complexity(3)_time(18)_songnr(80).wav",
      "lead_long_wit_level(0)_complexity(3)_time(122)_songnr(20).wav",
      "bass_long_wit_level(0)_complexity(6)_time(180)_songnr(37).wav",
      "lead_long_wit_level(0)_complexity(3)_time(52)_songnr(93).wav",
      "lead_long_wit_level(-10)_complexity(6)_time(102)_songnr(49).wav",
      "bass_long_wit_level(0)_complexity(6)_time(138)_songnr(49).wav"
    ),
  sample_audios_answers = c(1, 1, 1, 1, 1, 1)
) %>% as_tibble() %>% dplyr::slice_sample(n = 6) %>%
  as.list() # randomize list




get_practice_page_long <- function(page_no, feedback, audio_dir) {
  # browser()
  key <- sprintf("PRACTICE%d", page_no)
  if (page_no == 5) {
    # browser()
    key <- "TRANSITION"
  }
  prompt <- shiny::div(
    psychTestR::i18n(key, html = T, sub = list(feedback = feedback)),
    style = "text-align: justify; margin-left:20%;margin-right:20%; margin-bottom:20px; display:block")

  if (page_no == 5) {
    page <- ask_repeat(prompt)
  }
  else{
    # browser()
    page <- MSA_item(
      # label = sprintf("training%s", page_no),
      label = sample_data_long$label[[page_no]],
      correct_answer = sample_data_long$sample_audios_answers[page_no],
      prompt = prompt,
      audio_dir = audio_dir,
      audio_file = sample_data_long$sample_audios[[page_no]],
      adaptive = FALSE,
      # adaptive = TRUE,
      save_answer = FALSE,
      instruction_page = FALSE
    )

  }
  # browser()
  page
}

make_practice_page_long <- function(page_no, audio_dir) {
  psychTestR::reactive_page(function(answer, ...) {
    # browser()
    correct <- "INCORRECT"
    # answer = answer[-1,] # Fix this: gets "NA" otherswise...
    if (page_no > 1 && answer$correct) {
      correct <- "CORRECT"}
    feedback <- psychTestR::i18n(correct)
    get_practice_page_long(page_no, feedback, audio_dir)
    # browser()
  })
}

practice_long <- function(audio_dir) {
  lapply(1:5, make_practice_page_long, audio_dir) %>% unlist()
}
