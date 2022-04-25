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
    if (page_no > 1 && answer$correct) {
      correct <- "CORRECT"}
    feedback <- psychTestR::i18n(correct)
    get_practice_page(page_no, feedback, audio_dir)
    # browser()
  })
}

get_practice_page <- function(page_no, feedback, audio_dir) {
  # browser()
  key <- sprintf("PRACTICE%d", page_no)
   if (page_no == 5) {
    key <- "TRANSITION"
    }
     prompt <- psychTestR::i18n(key, html = T, sub = list(feedback = feedback))

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

###### Version 2 --------------------------------------------------------------
# item_bank <- MSA::MSA_item_bank # load whole item bank
# tmp_item_bank_practice <- item_bank %>% dplyr::filter(practice_item == "yes") # include only practice items
# tmp_item_bank_practice <- tmp_item_bank_practice %>% dplyr::slice_sample(n = 3) # choose 3 items randomly
# item_sequence <- charmatch(tmp_item_bank_practice$item_number, item_bank$item_number) # get item indexes
# item_sequence <- item_sequence[sample(1:length(tmp_item_bank_practice$item_number))] # randomize order
