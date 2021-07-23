training_answers  <- c(1, 2, 1)

ask_repeat <- function(prompt) {
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
}

make_practice_page <- function(page_no, audio_dir) {
  psychTestR::reactive_page(function(answer, ...) {
    correct <- "INCORRECT"
    if (page_no > 1 && answer == training_answers[page_no-1]) correct <- "CORRECT"
    feedback <- psychTestR::i18n(correct)
    get_practice_page(page_no, feedback, audio_dir)
  })
}

get_practice_page <- function(page_no, feedback, audio_dir){
  key <- sprintf("PRACTICE%d", page_no)

  if(page_no == 3) key <- "TRANSITION"
  prompt <- psychTestR::i18n(key, html = T, sub = list(feedback = feedback))

  if(page_no == 3){
    page <- ask_repeat(prompt)
  }
  else{
    # browser()
    # page <- MSAT_item(label = sprintf("training%s", page_no),
    page <- MSAT_item(label = sprintf("training%s", page_no),
                     correct_answer = training_answers[page_no],
                     prompt = prompt,
                     audio_dir = audio_dir,
                     audio_file = sprintf("i%s.mp3", page_no + 1),
                     save_answer = FALSE,
                     instruction_page = FALSE,
                     # adaptive = TRUE
                     )
  }
  page
}


# for(i in 1:length(item_sequence)){
#   item <- MSAT::MSAT_item_bank[]
#   # emotion <- psychTestR::i18n(item[1,]$emotion_i18)
#   #printf("Emotion %s ", emotion)
#   item_page <-
#     MSAT_item(label = item$item_number[1],
#               correct_answer = item$correct[1],
#               prompt = get_prompt(i, num_items),
#               audio_file = item$audio_file[1],
#               audio_dir = audio_dir,
#               save_answer = TRUE)
#   elts <- psychTestR::join(elts, item_page)
# }
# elts


practice <- function(audio_dir) {
  lapply(1:3, make_practice_page, audio_dir) %>% unlist()
}
