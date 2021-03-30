#' @export
calc_topic_word_freq <-
  function(twd, components, lambda = .6) {
    lambda * log(twd) +
      (1 - lambda) * log(t(t(twd) / (colSums(components) / sum(components))))
  }

#' @export
hc_to_d3 <-
  function(hclust_object,
           cluster_range,
           topic_size,
           top_words_size) {
    hoh <- hclust_object$height

    if(is.null(cluster_range)) {
      cluster_range <- 1
    }

    hoh_df <-
      tibble(uncertainty = hoh) %>%
      slice(cluster_range) %>%
      mutate(
        lg = lag(uncertainty, default = 0),
        uncertainty = uncertainty - lg,
        uncertainty = (scale(
          uncertainty, center = min(uncertainty), scale = diff(range(uncertainty))
        ) + 1)[,1],
        uncertainty = 2 * uncertainty,
        colname = as.character(cluster_range)
      ) %>%
      mutate(uncertainty = ifelse(is.nan(uncertainty), 1, uncertainty))

    cluster_struct <-
      cutree(hclust_object, cluster_range) %>%
      as_tibble(.name_repair = "unique") %>%
      mutate(across(.fns = ~ paste("cluster", .)))

    if(length(cluster_range) == 1)
      names(cluster_struct) <- cluster_range

    uncertainty_df <-
      cluster_struct %>%
      pivot_longer(cols = everything(), names_to = "colname") %>%
      left_join(hoh_df, "colname") %>%
      distinct(colname, uncertainty) %>%
      add_row(colname = "topic",
              uncertainty = 1)

    nested_df <-
      bind_cols(cluster_struct,
                tibble(
                  topic = 1:length(hclust_object$order),
                  #uncertainty = 0,
                  size = topic_size,
                  labels = top_words_size
                )) %>%
      d3r::d3_nest(value_cols = c("size", "labels"), json = FALSE) %>%
      mutate(uncertainty = 0)

    add_uncertainty(x = nested_df, uncertainty_values = uncertainty_df) %>%
      d3r::d3_json() %>%
      rjson::fromJSON()

  }

#' @export
add_uncertainty <-
  function(x, uncertainty_values) {
    x$children <- map(x$children,
                      function(z) {
                        z <- left_join(z, uncertainty_values, c("colname"))
                          if ("children" %in% names(z)) {

                            z <- add_uncertainty(z, uncertainty_values = uncertainty_values)

                          }
                        z
                      })
    x
  }
