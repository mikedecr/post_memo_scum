library(httr2)
library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(memoise)


###############################
#    list of grid elements    #
###############################

home_url = "https://www.killjamesbond.com"
archive_url = "https://web.archive.org/web/20240117205016"
archive_home_url = append_url(archive_url, home_url)

#############################################
#    url slug and title from table nodes    #
#############################################

append_url = function(url, path) {
    req_url_path_append(request(url), path)$url
}

href_slug = function(obj) {
    obj |> html_attr("href") |> str_remove("/")
}

get_film_urls = function(home) {
    grid_class <- ".gallery-grid-image-link"
    film_index_url = append_url(home, "scum-rankings")
    grid_nodes = film_index_url |>
        read_html() |>
        html_elements(css = grid_class)
    slugs = Map(href_slug, grid_nodes)
    sapply(slugs, function(s) append_url(home, s))
}


################################
#    url to film attributes    #
################################

memo_read_html = memoize(read_html)

get_page_content_block = function(url) {
    memo_read_html(url) |>
    html_elements(".content")
}

# this element exposes the title, actor, and year per page
# we query the results for each field
get_page_content <- function(url) {
    memo_read_html(url) |>
    html_elements(".content")
}

movie_title <- function(url) {
    get_page_content(url) |>
    html_nodes("h1") |>
    html_text() |>
    trimws()
}

# non-title info is under various h3 headers
h3_content <- function(url) {
    url |> get_page_content() |> html_nodes("h3")
}

# they list their own scum score
movie_scum_total <- function(url) {
    url |> h3_content() |> html_text() |> item(2) |> readr::parse_number()
}


# actor and year
expose_movie_attributes <- function(url) {
    url |> h3_content() |> item(1) |> html_text() |> str_split(" - ") |> item(1)
}

movie_actor <- function(url) {
    url |> expose_movie_attributes() |> item(1)
}

movie_year <- function(url) {
    url |> expose_movie_attributes() |> item(2) |> readr::parse_number()
}


#########################
#    scum components    #
#########################

# 1-row data frame of scum data
movie_scum_data <- memoize(function(url) {
    html <- memo_read_html(url)
    # column titles from the table
    names <- html |>
        html_elements(".tg-soxh") |>
        html_text() |>
        tolower() |>
        str_replace_all(" ", "_")
    # values from the table
    values <- html |>
        html_elements(".tg-3fto") |>
        html_text() |>
        readr::parse_number()
    # assert values are all ints
    remainders = values %% 1
    stopifnot(all(remainders == 0))
    # return
    setNames(values, names) |> as.list()
})


movie_smarm = function(url) {
    movie_scum_data(url) |> item("smarm")
}

movie_cultural_insensitivity = function(url) {
    movie_scum_data(url) |> item("cultural_insensitivity")
}

movie_unprovoked_violence = function(url) {
    movie_scum_data(url) |> item("unprovoked_violence")
}

movie_misogyny = function(url) {
    movie_scum_data(url) |> item("misogyny")
}


#############################
#    put it all together    #
#############################

df = tibble(
    film_url = get_film_urls(home_url),
    title = map_chr(film_url, movie_title),
    year = map_dbl(film_url, movie_year),
    actor = map_chr(film_url, movie_actor),
    scum_raw = map_dbl(film_url, movie_scum_total),
    smarm = map_dbl(film_url, movie_smarm),
    cultural_insensitivity = map_dbl(film_url, movie_cultural_insensitivity),
    unprovoked_violence = map_dbl(film_url, movie_unprovoked_violence),
    misogyny = map_dbl(film_url, movie_misogyny),
    scum = smarm + cultural_insensitivity + unprovoked_violence + misogyny
)

# rbenchmark::benchmark(read_html(u))
# rbenchmark::benchmark(memo_read_html(u))

