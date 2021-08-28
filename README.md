# LISP (Racket) flavored toolbox for scraping from goodreads

This repository contains a set of tools for extracting data from Goodreads.

## goodreads-all-ratings https://www.goodreads.com/book/show/{ID}

Given a book URL with the above format as an argument, outputs a CSV listing of ratings in descending order. Each tuple in the output has the form `(rating, user-url)` where `rating` is a number 0..5 signifying the # of stars given (0 being unrated).

# Disclaimer

No warranty or support provided. All tools only read/parse publicly available data. 
