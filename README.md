# Forking Paths, Open Doors references
Joseph V. Casillas
2026-11-20

``` r
# Load bib files
bib1 <- suppressWarnings(ReadBib("ch1.bib", check = FALSE))
bib2 <- suppressWarnings(ReadBib("ch2.bib", check = FALSE))
bib3 <- suppressWarnings(ReadBib("ch3.bib", check = FALSE))
bib4 <- suppressWarnings(ReadBib("ch4.bib", check = FALSE))
bib5 <- suppressWarnings(ReadBib("ch5.bib", check = FALSE))
bib6 <- suppressWarnings(ReadBib("ch6.bib", check = FALSE))

bib <- c(bib1, bib2, bib3, bib4, bib5, bib6)

# Create csv of citekeys
cite_key_list_ch1 <- bind_cols(
  bib1$key |> unlist() |> tibble::enframe(name = NULL), 
  bib1$bibtype |> unlist() |> tibble::enframe(name = NULL), 
  bib1$year |> unlist() |> tibble::enframe(name = NULL)
  ) |> 
  rename(citekey = value...1, type = value...2, year = value...3) |> 
  write_csv(here("cite_key_list_ch1.csv"))

# Set bib options for printing
BibOptions(bib.style = "authoryear", style = "text", max.names = 10, 
           first.inits = TRUE, check.entries = FALSE)

# Convert to dataframe for analyses
dat <- bind_rows(
  as_tibble(bib1) |> mutate(ch = "ch1"), 
  as_tibble(bib2) |> mutate(ch = "ch2"), 
  as_tibble(bib3) |> mutate(ch = "ch3"), 
  as_tibble(bib4) |> mutate(ch = "ch4"), 
  as_tibble(bib5) |> mutate(ch = "ch5"), 
  as_tibble(bib6) |> mutate(ch = "ch6")
  ) |> 
  purrr::map_df(.f = textutils::HTMLdecode) |> 
  mutate(year = as.numeric(year)) |> 
  distinct()
```

## Citation Types

## Journals

``` r
datj <- dat |> 
  group_by(bibtype) |> 
  count(journal)

dat |> 
  mutate(journal = if_else(journal == "Journal of Second Language Acquisition and Teaching: Arizona working papers in SLAT", "Journal of Second Language Acquisition and Teaching:\nArizona working papers in SLAT", journal)) |> 
  group_by(journal) |> 
  summarize(counts = n(), .groups = "drop") |> 
  na.omit() |> 
  mutate(journal = fct_reorder(journal, counts)) |> 
  ggplot(aes(x = journal, y = counts, label = counts)) + 
    geom_bar(stat = "identity", color = 'black', 
             fill = 'darkred', width = 0.1) + 
    geom_point(pch = 21, size = 6, color = 'black', fill = 'lightgrey') + 
    geom_text(size = 3) + 
    labs(y = "Count", x = "Journal") + 
    coord_flip() + 
    my_theme()
```

## Authors

``` r
# Initialize list
authors <- list()

# For each element in list, get last name of author and store in 
# 'authors' list
for (i in 1:length(bib)) {
  authors[[i]] <- bib[i]$author$family |> unlist()
}

# Convert to tibble and plot
purrr::map(authors, textutils::HTMLdecode) |> 
  unlist() |> 
  tibble::enframe() |> 
  group_by(value) |> 
  summarize(counts = n(), .groups = "drop") |> 
  mutate(value = fct_reorder(value, counts)) |> 
  ggplot(aes(y = value, x = counts, label = counts)) + 
    geom_bar(stat = "identity", color = 'black', 
             fill = 'darkred', width = 0.05) + 
    geom_point(pch = 21, size = 5, color = 'black', fill = 'lightgrey') + 
    geom_text(size = 2) + 
    scale_y_discrete(expand = c(0.02, 0, 0.02, 0)) + 
    labs(y = "Count", x = "Author", 
         title = "Most frequent researchers") + 
    my_theme(base_size = 8) + 
    theme(axis.text.y = element_text(size = 4))
```

## Publication Years

``` r
prod <- dat |> 
  select(year) |> 
  na.omit() |> 
  group_by(year) |> 
  summarize(counts = n(), .groups = "drop") 
 

year_max <- max(prod$counts)
year_current <- prod$year |> unique() |> max()

prod |> 
  ggplot(aes(x = year, y = counts, label = counts)) + 
    geom_bar(stat = "identity", color = 'black', 
             fill = 'black', width = 0.1) + 
    geom_point(pch = 21, size = 6, color = "black", fill = "#cc0033") + 
    geom_text(color = 'white', size = 3) + 
    scale_fill_brewer(name = NULL, palette = 'Set1') + 
    scale_x_continuous(breaks = seq(from = 1940, to = 2040, by = 10)) + 
    labs(y = "Count", x = "Year") + 
    expand_limits(y = c(0, year_max + 5)) + 
    my_theme()
```

# Reference list

<div id="refs_ch1">

</div>
