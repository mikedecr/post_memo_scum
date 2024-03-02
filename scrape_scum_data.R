# ----------------------------------------------------------------------------
#  Scrape SCUM data from KillJamesBond.com
#  by Michael DeCrescenzo (github.com/mikedecr)
# ----------------------------------------------------------------------------

# We take the following approach.
# 1. There is a table at https://www.killjamesbond.com/scum-rankings
#      containing links to each film's SCUM data.
#    We use this table to build an index of URLs to walk
# 2. We the define several functions to get SCUM and other metadata
#      from a given movie's URL.
# 3. Build a data frame by applying the functions to each url in our index.

library(httr2)
library(readr)
library(rvest)
library(stringr)
library(memoise)
library(dplyr)
library(tidyr)
library(purrr)

# ----------------------------------------------------------------------------
#  parse movie URLs from index table
# ----------------------------------------------------------------------------

# URL for the index page
# we use this specific snapshot from 2024 / 01 / 17
archive_url = "https://web.archive.org/web/20240117205016"
archive_req = request(archive_url)

# we can create the extra url contents here
home_url = "https://www.killjamesbond.com"
home_req = request(home_url)
film_index_url <- req_url_path_append(home_req, path = "scum-rankings")$url

archive_film_index_url = req_url_path_append(archive_req, film_index_url)$url

# index HTML source
film_index_html <- rvest::read_html(archive_film_index_url)

# ---- links to each movie come from items in a grid --------

# this gives us the grid
grid_class <- ".gallery-grid-image-link"
# gets each item
film_grid_nodes <- rvest::html_elements(film_index_html, css = grid_class)

# extract data from the grid items
(film_titles <- film_grid_nodes |> rvest::html_nodes("img") |> rvest::html_attr("alt"))

(film_title_stubs = film_grid_nodes |>
    rvest::html_attr("href") |>
    stringr::str_split(paste0(home_url, "/")) |>
    lapply(function(x) x[2]) |>
    unlist()
)

# named vec from sapply
(film_urls <- sapply(film_title_stubs, function(x) req_url_path_append(home_req, path = x)$url))

(archive_film_urls = sapply(film_urls,
                           function(x) req_url_path_append(archive_req, path = x)$url
))


# ----------------------------------------------------------------------------
#  parse data from each movie page
# ----------------------------------------------------------------------------

# We're going to make a quick interface by writing lots of functions.
# If any one function breaks, we can fix it in isolation.

# memoize the reading of per-page html
read_html <- memoize(rvest::read_html)

# this element exposes the title, actor, and year per page
# we query the results for each field
get_page_content <- function(url) {
    read_html(url) |>
    html_elements(".content")
}

movie_title <- function(url) {
    get_page_content(url) |>
    html_nodes("h1") |>
    html_text() |>
    trimws()
}
 
# non-title info is under various h3 headers
expose_misc_content <- function(url) {
    get_page_content(url) |>
    html_nodes("h3")
}
 
# they list a sum of scum
movie_scum_total <- function(url) {
    expose_misc_content(url)[2] |>
    html_text() |>
    parse_number()
}

# actor and year
expose_movie_attributes <- function(url) {
    expose_misc_content(url)[1] |>
    html_text() |>
    str_split_fixed(" - ", 2)
}
 
movie_actor <- function(url) {
    expose_movie_attributes(url)[, 1]
}

movie_year <- function(url) {
    expose_movie_attributes(url)[, 2] |>
    parse_number()
}

# 1-row data frame of scum data
movie_scum_data <- function(url) {
    html <- read_html(url)
    # column titles from the table
    names <- html |>
        html_elements(".tg-soxh") |>
        html_text()
    # values from the table
    values <- html |>
        html_elements(".tg-3fto") |>
        html_text() |>
        parse_number()
    # named vector to data frame
    dplyr::bind_rows(rlang::set_names(values, names))
}

# ----------------------------------------------------------------------------
#  combine into data frame
# ----------------------------------------------------------------------------

# initialize data frame
df <-
    tibble(
        index_title = film_titles,
        stub = film_title_stubs,
        url = archive_film_urls
    )


# add scraped attributes
# memoization of read_html makes this much faster
df <- df |>
    mutate(
        title = map_chr(url, movie_title),
        actor = map_chr(url, movie_actor),
        year = map_dbl(url, movie_year),
        scum_total_raw = map_dbl(url, movie_scum_total),
        scum_components = map(url, movie_scum_data)
    ) |>
    unnest(cols = c(scum_components))


# ----------------------------------------------------------------------------
#  calculate total SCUM for all films
# ----------------------------------------------------------------------------

df <- df |>
    mutate(scum = Smarm + `Cultural Insensitivity`
                  + `Unprovoked Violence` + Misogyny)

#' @export
data <- df |>
    rename_all(tolower) |>
    rename_all(str_replace_all, " ", "_") |>
    select(title, actor, year, scum_total_raw, smarm:misogyny, scum)

