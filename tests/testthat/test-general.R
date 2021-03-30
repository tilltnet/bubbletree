library(bubbletree)
library(text2vec)
library(tidyverse)

data("movie_review")
N = 500
tokens = word_tokenizer(tolower(movie_review$review[1:N]))
it = itoken(tokens, ids = movie_review$id[1:N])
v = create_vocabulary(it)
v = prune_vocabulary(v, term_count_min = 5, doc_proportion_max = 0.2)
dtm = create_dtm(it, vocab_vectorizer(v))
lda_model = LDA$new(n_topics = 10)
doc_topic_distr = lda_model$fit_transform(dtm, n_iter = 20)

lda_model2 = LDA$new(n_topics = 100)
doc_topic_distr2 = lda_model2$fit_transform(dtm, n_iter = 20)
# run LDAvis visualisation if needed (make sure LDAvis package installed)
# lda_model$plot()

cluster_data <- hclust(as.dist(sim2(t(doc_topic_distr))))
cluster_data2 <- hclust(as.dist(sim2(t(doc_topic_distr2))))

topic_word_freq <-
  calc_topic_word_freq(lda_model$topic_word_distribution,
                       lda_model$components) %>%
  t() %>%
  as_tibble(rownames = "word") %>%
  mutate(across(.fns = ~ ifelse(is.infinite(.), NA, .)))

topic_word_freq2 <-
  calc_topic_word_freq(lda_model2$topic_word_distribution,
                       lda_model2$components) %>%
  t() %>%
  as_tibble(rownames = "word") %>%
  mutate(across(.fns = ~ ifelse(is.infinite(.), NA, .)))


top_words <- lda_model$get_top_words(5, lambda = .4)
top_words2 <- lda_model2$get_top_words(5, lambda = .4)


labels <-
  top_words %>%
  as_tibble(.name_repair = "unique") %>%
  map2(1:ncol(.),
       ~filter(topic_word_freq, word %in% .x) %>%
         .[c(1, .y + 1)] %>%
         rename(text = word, size = 2) %>%
         split(.$text) %>%
         map(~list(text = .x$text,
                   size = (.x$size + 5)*3)))
labels
names(labels) <- NULL
labels <- map(labels, unname)

labels2 <-
  top_words2 %>%
  as_tibble(.name_repair = "unique") %>%
  map2(1:ncol(.),
       ~filter(topic_word_freq2, word %in% .x) %>%
         .[c(1, .y + 1)] %>%
         rename(text = word, size = 2) %>%
         split(.$text) %>%
         map(~list(text = .x$text,
                   size = as.integer(.x$size + 5)*2)))

labels2
names(labels2) <- NULL
labels2 <- map(labels2, unname)

data("labels")
data("cluster_data")
data("labels2")
data("cluster_data2")

sum_tdp_df <-
  apply(doc_topic_distr, 2, sum) %>%
  enframe("topic", "sum_tdp")

sum_tdp_df2 <-
  apply(doc_topic_distr2, 2, sum) %>%
  enframe("topic", "sum_tdp")

td <- hc_to_d3(hclust_object = cluster_data,
         cluster_range = c(2, 4, 6),
         topic_size = sum_tdp_df$sum_tdp,
         top_words_size = labels)

td2 <- hc_to_d3(hclust_object = cluster_data2,
                cluster_range = 6:7,
                sum_tdp_df2$sum_tdp,
                top_words_size = labels2)


bubbletree(td, settings = list(padding = 1, resizing_factor = 20, max_textsize = 0.05))
bubbletree(td, settings = list(padding = 2, min_textsize_scale = 6, max_textsize = 4))


#target_indicator <- readRDS("inst/target_indicator.RDS")
target_indicator <- readRDS("inst/validation_set(1).RDS")

ti_df <-
  target_indicator$label %>%
  t() %>%
  as_tibble(.name_repair = "unique")

ti0_df <-
  ti_df %>%
  mutate(across(.fns = ~""))

ti_df <-
  ti_df %>%
  add_row(ti0_df) %>%
  add_row(ti0_df) %>%
  add_row(ti0_df) %>%
  add_row(ti0_df)


ti_labels <-
  map(ti_df, ~{
    tibble(text = .x, size = 1) %>%
      split(1:5) %>%
      map(~list(text = .x$text,
                size = as.integer(.x$size + 5)*200))
  })

names(ti_labels) <- NULL
ti_labels <- map(ti_labels, unname)

ti <- hc_to_d3(target_indicator, c(1, 5, 17), topic_size = 1, ti_labels)
bubbletree(ti, settings = list(padding = 2, resizing_factor = 3, max_textsize = 4, min_textsize_scale = 3.5))


# testing hc_to_d3 with one or none cutoff --------------------------------

tdx <- hc_to_d3(hclust_object = cluster_data,
               cluster_range = NULL,
               topic_size = sum_tdp_df$sum_tdp,
               top_words_size = labels)
bubbletree(tdx, settings = list(padding = 2, resizing_factor = 20, max_textsize = 1, min_textsize_scale = 6))
