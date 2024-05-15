# MSA_options <- function() {
#
# }

# for the adaptive test:
MSA_options <- function(next_item.criterion,
                        next_item.estimator,
                        next_item.prior_dist = next_item.prior_dist,
                        next_item.prior_par = next_item.prior_par,
                        final_ability.estimator,
                        constrain_answers = FALSE,
                        eligible_first_items = eligible_first_items,
                        item_bank) {
  psychTestRCAT::adapt_test_options(
    next_item.criterion = next_item.criterion,
    next_item.estimator = next_item.estimator,
    next_item.prior_dist = next_item.prior_dist,
    next_item.prior_par = next_item.prior_par,
    final_ability.estimator = final_ability.estimator,
    # eligible_first_items = get_eligible_first_items_MSA(),
    eligible_first_items = eligible_first_items,
    constrain_answers = constrain_answers,
    avoid_duplicates = "set_nr" # make sure the same stimuli is not presented twice
  )
}
