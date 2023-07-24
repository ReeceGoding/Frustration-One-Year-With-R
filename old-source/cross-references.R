tackle_title <- function(title) {
  text <- sub("#", "", title) |> trimws()
  sec <- sprintf("{#sec-%s}", snakecase::to_any_case(tolower(title), sep_out = "-"))
  sprintf("%s %s", title, sec)
}

tackle_line <- function(line) {
  gsub("\\]\\(\\#", "](#sec-", line)
}

tackle_file <- function(file) {
  filelines <- brio::read_lines(file)
  headings <- which(grepl("^#", filelines))
  filelines[headings] <- purrr::map_chr(filelines[headings], tackle_title)

  filelines <- purrr::map_chr(filelines, tackle_line)

  brio::write_lines(filelines, file)
}

purrr::walk(fs::dir_ls(glob = "*.qmd"), tackle_file)
quarto::quarto_render()
