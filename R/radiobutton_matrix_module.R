#' Audio N-Alternative Choice Matrix Module
#'
#' This function defines a module of several Audio N-Alternative Choice Matrix
#' pages for incorporation into a \code{psychTestR} timeline.
#' Use this function if you want to include the \code{NAC_matrix_module} in a
#' battery of other tests, or if you want to add custom psychTestR pages to your
#' test timeline.
#'
#' @export
audio_NAC_matrix_module <- function(label = "EM01",
                                    questionnaire = "EMO1",
                                    response_scale = "L7",
                                    items_prefix_pattern = "s%02d",
                                    # item_order = NULL,
                                    stimuli_order = NULL,
                                    num_stimuli = 10,
                                    num_rating_items = 6,
                                    anchors = FALSE,
                                    header = "double",
                                    header_style = NULL,
                                    reduce_labels = TRUE,
                                    style = default_style,
                                    allow_na = TRUE,
                                    audio_url = "https://s3.eu-west-1.amazonaws.com/media.gold-msi.org/test_materials/GAR/EMO1",
                                    audio_type = "wav",
                                    dict = GAR::GAR_dict,
                                    random_order = FALSE,
                                    randomize_stimuli = FALSE,
                                    show_controls = TRUE,
                                    allow_download = FALSE,
                                    question_header_offset = 0,
                                    question_header_max = num_stimuli,
                                    ...) {
  # browser()
  dots <- list(...)
  MAS_IE <- FALSE
  if("MAS_IE" %in% names(dots)){
    MAS_IE <- dots$MAS_IE
  }

  quest <- get_questionnaires()
  if(!(questionnaire %in% quest$id)){
    stop(sprintf("Unknown questionnaire: %s", questionnaire))
  }

  num_rating_items <-
    max(1, min(num_rating_items, quest[quest$id == questionnaire,]$max_items))
  scale_length <-
    as.numeric(stringr::str_extract(response_scale, "[0-9]+"))

  preamble_key <- sprintf("TGAR_%s_PREAMBLE", questionnaire)
  prompt_key <- sprintf("TGAR_%s_%%04d_PROMPT", questionnaire)
  label_key <- sprintf("TGAR_%s_CHOICE%%01d", response_scale)

  if(!is.null(stimuli_order)){
    stopifnot(length(stimuli_order) == num_stimuli)
  }

  pages <- function(num_stimuli,
                    label,
                    audio_url,
                    stimuli_order,
                    items_prefix_pattern,
                    audio_type,
                    state,
                    anchors,
                    header,
                    header_style,
                    reduce_labels,
                    style,
                    num_rating_items,
                    prompt_key,
                    scale_length,
                    label_key,
                    show_controls,
                    allow_download,
                    allow_na,
                    dict,
                    ...) {
    lapply(
      1:num_stimuli,
      function(id) {
        page_label <- sprintf("%s_%02d", label, id)
        stimulus_url <-
          file.path(
            audio_url,
            sprintf(
              "%s.%s",
              ifelse(
                is.null(stimuli_order),
                sprintf(items_prefix_pattern, id),
                stimuli_order[id]
              ),
              audio_type)
          )
        psychTestR::new_timeline(
          psychTestR::join(
            psychTestR::reactive_page(
              fun = function(state, ...) {
                matrix_item_order <- access_matrix_item_order(label = label,
                                                              state = state)
                audio_NAC_matrix_page(
                  label = page_label,
                  url = stimulus_url,
                  instruction = psychTestR::i18n(preamble_key),
                  anchors = anchors,
                  header = header,
                  header_style = header_style,
                  sublabel_type = "directed",
                  reduce_labels = reduce_labels,
                  style = style,
                  trigger_button_text = psychTestR::i18n("CONTINUE"),
                  failed_validation_message = psychTestR::i18n("ANSWER_MISSING"),
                  items = sapply(1:num_rating_items, function(x) psychTestR::i18n(sprintf(prompt_key, x)), simplify = T, USE.NAMES = T),
                  choices = 0:(scale_length-1),
                  labels = sapply(1:scale_length, function(x) psychTestR::i18n(sprintf(label_key, x)), simplify = T, USE.NAMES = T),
                  # random_order = random_order,
                  matrix_item_order = matrix_item_order,
                  show_controls = show_controls,
                  allow_download = allow_download,
                  allow_na = allow_na
                )
              },
              next_elt = TRUE
            ),
            psychTestR::elt_save_results_to_disk(complete = TRUE)
          ),
          dict = dict
        )
      }
    )
  }


  save_stimuli <- function(label){
    function(order, state, ...){
      #browser()
      if(!is.null(stimuli_order)){
        stimuli <- sprintf("%s", stimuli_order[1:num_stimuli])[order]
      }
      else{
        stimuli <- sprintf(items_prefix_pattern, 1:num_stimuli)[order]
      }#
      message(sprintf("Saving stimulus order for  %s (length: %d): %s",
                      label, length(order),
                      paste(stimuli, collapse = ", ")))
      psychTestR::save_result(state, label, stimuli)
    }
  }

  if (randomize_stimuli) {
    psychTestR::join(
      psychTestR::begin_module(label = label),
      matrix_item_order_module(num_rating_items = num_rating_items,
                               random_order = random_order),
      psychTestR::randomise_at_run_time(
        label = label,
        logic = pages(
          num_stimuli = num_stimuli,
          label = label,
          audio_url = audio_url,
          stimuli_order = stimuli_order,
          items_prefix_pattern = items_prefix_pattern,
          audio_type = audio_type,
          state = state,
          anchors = anchors,
          header = header,
          header_style = header_style,
          reduce_labels = reduce_labels,
          style = style,
          num_rating_items = num_rating_items,
          prompt_key = prompt_key,
          scale_length = scale_length,
          label_key = label_key,
          show_controls = show_controls,
          allow_download = allow_download,
          allow_na = allow_na,
          dict = dict,
          ...
        ),
        save_order = save_stimuli(sprintf("%s_stimulus_order", label))
      ),
      psychTestR::end_module()
    )
  } else {
    psychTestR::join(
      psychTestR::begin_module(label = label),
      matrix_item_order_module(num_rating_items = num_rating_items,
                               random_order = random_order),
      psychTestR::order_at_run_time(
        label = label,
        logic = pages(
          num_stimuli = num_stimuli,
          label = label,
          audio_url = audio_url,
          stimuli_order = stimuli_order,
          items_prefix_pattern = items_prefix_pattern,
          audio_type = audio_type,
          state = state,
          anchors = anchors,
          header = header,
          header_style = header_style,
          reduce_labels = reduce_labels,
          style = style,
          num_rating_items = num_rating_items,
          prompt_key = prompt_key,
          scale_length = scale_length,
          label_key = label_key,
          show_controls = show_controls,
          allow_download = allow_download,
          allow_na = allow_na,
          dict = dict,
          ...
        ),
        get_order = function(...) 1:num_stimuli,
        save_order = save_stimuli(sprintf("%s_stimulus_order", label))
      ),
      psychTestR::end_module()
    )
  }
}

matrix_item_order_module <- function(num_rating_items,
                                     random_order = FALSE,
                                     label = "matrix_item_order") {
  stopifnot(num_rating_items > 0,
            is.logical(random_order))

  psychTestR::join(
    psychTestR::begin_module(label = label),
    psychTestR::code_block(
      function(state, ...) {
        if (random_order) {
          order <- sample(num_rating_items)
        } else {
          order <- 1:num_rating_items
        }
        psychTestR::save_result(
          place = state,
          label = "order",
          value = order
        )
      }
    ),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    psychTestR::end_module()
  )
}

access_matrix_item_order <- function(label,
                                     sublabel = "matrix_item_order",
                                     subsublabel = "order",
                                     state) {
  res <-
    psychTestR::get_results(state = state, complete = FALSE)
  res[[paste(label, sublabel, sep = ".")]][[subsublabel]]
}


audio_NAC_matrix_module2 <- function(label = "EM01",
                                    questionnaire = "EMO1",
                                    response_scale = "L7",
                                    items_prefix_pattern = "s%02d",
                                    # item_order = NULL,
                                    stimuli_order = NULL,
                                    num_stimuli = 10,
                                    num_rating_items = 6,
                                    anchors = FALSE,
                                    header = "double",
                                    header_style = NULL,
                                    reduce_labels = TRUE,
                                    style = default_style,
                                    allow_na = TRUE,
                                    audio_url = "https://s3.eu-west-1.amazonaws.com/media.gold-msi.org/test_materials/GAR/EMO1",
                                    audio_type = "wav",
                                    dict = GAR::GAR_dict,
                                    random_order = FALSE,
                                    randomize_stimuli = FALSE,
                                    show_controls = TRUE,
                                    allow_download = FALSE,
                                    question_header_offset = 0,
                                    question_header_max = num_stimuli,
                                    ...) {
  # browser()
  dots <- list(...)
  MAS_IE <- FALSE
  if("MAS_IE" %in% names(dots)){
    MAS_IE <- dots$MAS_IE
  }

  quest <- get_questionnaires()
  if(!(questionnaire %in% quest$id)){
    stop(sprintf("Unknown questionnaire: %s", questionnaire))
  }

  num_rating_items <-
    max(1, min(num_rating_items, quest[quest$id == questionnaire,]$max_items))
  scale_length <-
    as.numeric(stringr::str_extract(response_scale, "[0-9]+"))

  preamble_key <- sprintf("TGAR_%s_PREAMBLE", questionnaire)
  prompt_key <- sprintf("TGAR_%s_%%04d_PROMPT", questionnaire)
  label_key <- sprintf("TGAR_%s_CHOICE%%01d", response_scale)

  if(!is.null(stimuli_order)){
    stopifnot(length(stimuli_order) == num_stimuli)
  }

  pages <-
    lapply(
      1:num_stimuli,
      function(id) {
        page_label <- sprintf("%s_%02d", label, id)
        stimulus_url <-
          file.path(
            audio_url,
            sprintf(
              "%s.%s",
              ifelse(
                is.null(stimuli_order),
                sprintf(items_prefix_pattern, id),
                stimuli_order[id]
              ),
              audio_type)
          )
        psychTestR::new_timeline(
          psychTestR::join(
            psychTestR::reactive_page(
              fun = function(state, ...) {
                matrix_item_order <- access_matrix_item_order(label = label,
                                                              state = state)
                audio_NAC_matrix_page(
                  label = page_label,
                  url = stimulus_url,
                  instruction = psychTestR::i18n(preamble_key),
                  anchors = anchors,
                  header = header,
                  header_style = header_style,
                  sublabel_type = "directed",
                  reduce_labels = reduce_labels,
                  style = style,
                  trigger_button_text = psychTestR::i18n("CONTINUE"),
                  failed_validation_message = psychTestR::i18n("ANSWER_MISSING"),
                  items = sapply(1:num_rating_items, function(x) psychTestR::i18n(sprintf(prompt_key, x)), simplify = T, USE.NAMES = T),
                  choices = 0:(scale_length-1),
                  labels = sapply(1:scale_length, function(x) psychTestR::i18n(sprintf(label_key, x)), simplify = T, USE.NAMES = T),
                  # random_order = random_order,
                  matrix_item_order = matrix_item_order,
                  show_controls = show_controls,
                  allow_download = allow_download,
                  allow_na = allow_na
                )
              },
              next_elt = TRUE
            ),
            psychTestR::elt_save_results_to_disk(complete = TRUE)
          ),
          dict = dict
        )
      }
    )


  save_stimuli <- function(label){
    function(order, state, ...){
      #browser()
      if(!is.null(stimuli_order)){
        stimuli <- sprintf("%s", stimuli_order[1:num_stimuli])[order]
      }
      else{
        stimuli <- sprintf(items_prefix_pattern, 1:num_stimuli)[order]
      }#
      message(sprintf("Saving stimulus order for  %s (length: %d): %s",
                      label, length(order),
                      paste(stimuli, collapse = ", ")))
      psychTestR::save_result(state, label, stimuli)
    }
  }

  if (randomize_stimuli) {
    psychTestR::join(
      psychTestR::begin_module(label = label),
      matrix_item_order_module(num_rating_items = num_rating_items,
                               random_order = random_order),
      psychTestR::randomise_at_run_time(
        label = label,
        logic = pages,
        save_order = save_stimuli(sprintf("%s_stimulus_order", label))
      ),
      psychTestR::end_module()
    )
  } else {
    psychTestR::join(
      psychTestR::begin_module(label = label),
      matrix_item_order_module(num_rating_items = num_rating_items,
                               random_order = random_order),
      psychTestR::order_at_run_time(
        label = label,
        logic = pages,
        get_order = function(...) 1:num_stimuli,
        save_order = save_stimuli(sprintf("%s_stimulus_order", label))
      ),
      psychTestR::end_module()
    )
  }
}
