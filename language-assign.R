## Description: Example script for splitting documents into parts based on
##  word counts, and detecting the language by comparison with stop word files
## Author: Isidor Zeuner
## dh2021@quidecco.de
## For license see: https://mit-license.org
## -------------------------------------------------------------------

library(tidyverse)
library(pracma)
library(koRpus)

split_text <- function(filename) {
split_at <- 1000

file_base <- filename %>% basename %>% str_replace(
    pattern = "[.]txt$",
    replacement = ""
)

lines <- readLines(filename)

words <- map(
    lines,
    function (line) {
        sapply(strsplit(line, " "), length)
    }
)

## for each line, the number of words up to that line
cumulated_words <- cumsum(words)

split_directory <- file.path(getwd(), "split")

if (! dir.exists(split_directory)){
    dir.create(split_directory)}

## split into parts of about `split_at` words, but only at line end
(
    1 : ceil(cumulated_words[[length(cumulated_words)]] / split_at)
) %>% map(
    function (to) {
        from <- to - 1
        from_words <- from * split_at
        to_words <- to * split_at
        part_lines <- lines[
            which(
                from_words < cumulated_words & cumulated_words <= to_words
            )
        ]
        part_name <- file.path(
            split_directory,
            paste0(file_base, "-", to, ".txt")
        )
        writeLines(part_lines, part_name)
    }
)
}
## compute what fraction of the `words` are in the stopword list
##  of the `language`
language_match <- function (words, language) {
    wordlist <- stopwords::stopwords(language, source = "stopwords-iso")
    length(which(words %in% wordlist)) / length(words)
}

## compute which of the `languages` has the highest fraction of the `words`
##  in its stopword list
language_best_match <- function (words, languages) {
    languages[[
        which.max(map(languages, partial(language_match, words = words)))
    ]]
}

assign_language <- function(country_code = "be") {
    split_directory <- file.path(getwd(), "split")   
    parts <- Sys.glob(file.path(split_directory, sprintf("%s*.txt",
                                                         country_code)))

    sample_assignment <- data.frame(
        filename = parts,
        language = parts %>% map(
            function (filename) {
                language_best_match(
                    tokenize(filename, tag = FALSE),
                    c("en", "fr", "nl")
                )
            }
            ) %>% unlist
    )
    for ( i in 1:length(sample_assignment$filename)) {
        file.rename(sample_assignment$filename[i],
                    str_replace(sample_assignment$filename[i],
                                "\\.txt",
                                sprintf("-%s.txt",sample_assignment$language[i])))
}

sample_assignment
}
