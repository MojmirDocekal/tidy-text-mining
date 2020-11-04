## ----setup, include=FALSE-------------------------------------------------
library(knitr)
knitr::opts_chunk$set(cache = TRUE, warning = FALSE, message = FALSE,
                      echo = TRUE, dpi = 300, cache.lazy = FALSE,
                      tidy = "styler", fig.width = 8, fig.height = 5)
options(cli.width = 85)
options(crayon.enabled = FALSE)
library(ggplot2)
theme_set(theme_light())


## ----text-----------------------------------------------------------------
text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

text


## ----text_df, dependson = "text"------------------------------------------
library(dplyr)
text_df <- tibble(line = 1:4, text = text)

text_df


## A token is a meaningful unit of text, most often a word, that we are interested in using for further analysis, and tokenization is the process of splitting text into tokens.


## ----dependson = "text_df", R.options = list(dplyr.print_max = 10)--------
library(tidytext)

text_df %>%
  unnest_tokens(word, text)


## ----tidyflow-ch1, echo = FALSE, out.width = '100%', fig.cap = "A flowchart of a typical text analysis using tidy data principles. This chapter shows how to summarize and visualize text using these tools."----
knitr::include_graphics("images/tmwr_0101.png")


## ----original_books-------------------------------------------------------
library(janeaustenr)
library(dplyr)
library(stringr)

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

original_books


## ----tidy_books_raw, dependson = "original_books"-------------------------
library(tidytext)
tidy_books <- original_books %>%
  unnest_tokens(word, text)

tidy_books


## ----tidy_books, dependson = "tidy_books_raw"-----------------------------
data(stop_words)

tidy_books <- tidy_books %>%
  anti_join(stop_words)


## ----dependson = "tidy_books"---------------------------------------------
tidy_books %>%
  count(word, sort = TRUE)


## ----plotcount, dependson = "tidy_books", fig.width=6, fig.height=5, fig.cap="The most common words in Jane Austen's novels"----
library(ggplot2)

tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


## To learn more about gutenbergr, check out the [package's tutorial at rOpenSci](https://ropensci.org/tutorials/gutenbergr_tutorial.html), where it is one of rOpenSci's packages for data access.


## ----eval = FALSE---------------------------------------------------------
## library(gutenbergr)
##
## hgwells <- gutenberg_download(c(35, 36, 5230, 159))


## ----hgwells, echo = FALSE------------------------------------------------
load("data/hgwells.rda")


## ----tidy_hgwells, dependson = "hgwells"----------------------------------
tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)


## ----dependson = "tidy_hgwells"-------------------------------------------
tidy_hgwells %>%
  count(word, sort = TRUE)


## ----eval = FALSE---------------------------------------------------------
## bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))


## ----echo = FALSE---------------------------------------------------------
load("data/bronte.rda")


## ----tidy_bronte, dependson = "bronte"------------------------------------
tidy_bronte <- bronte %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)


## ----dependson = "tidy_bronte"--------------------------------------------
tidy_bronte %>%
  count(word, sort = TRUE)


## ----frequency, dependson = c("tidy_bronte", "tidy_hgwells", "tidy_books")----
library(tidyr)

frequency <- bind_rows(mutate(tidy_bronte, author = "Brontë Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"),
                       mutate(tidy_books, author = "Jane Austen")) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `Brontë Sisters`:`H.G. Wells`)


## ----plotcompare, dependson = "frequency", fig.width=10, fig.height=5.5, fig.cap="Comparing the word frequencies of Jane Austen, the Brontë sisters, and H.G. Wells"----
library(scales)

# expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, y = `Jane Austen`, color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Jane Austen", x = NULL)


## ----cor_test, dependson = "frequency"------------------------------------
cor.test(data = frequency[frequency$author == "Brontë Sisters",],
         ~ proportion + `Jane Austen`)
cor.test(data = frequency[frequency$author == "H.G. Wells",],
         ~ proportion + `Jane Austen`)

