# from the babeldown package
fakify_xml <- function(nodes_list) {
  temp_file <- withr::local_tempfile()
  lines <- paste(
    readLines(system.file("template.xml", package = "babeldown")),
    collapse = "\n"
  )
  fill <- if (inherits(nodes_list, "xml_nodeset")) {
    paste(as.character(nodes_list), collapse = "\n")
  } else {
    paste(
      purrr::map_chr(nodes_list, ~ paste(as.character(xml2::xml_children(.x)), collapse = "\n")),
      collapse = "\n"
    )
  }
  lines <- sub("FILLHERE", fill, lines, fixed = TRUE)
  brio::write_lines(lines, temp_file)
  xml2::read_xml(temp_file)
}

yarn <- tinkr::yarn$new(file.path("old-source", "Frustration-One-Year-With-R.Rmd"))

children <- xml2::xml_children(yarn$body)
headings <- which(
  xml2::xml_name(children) == "heading" &
    xml2::xml_attr(children, "level") == 1
)
seqs <- purrr::map2(
  headings,
  c(headings[-1] - 1, length(children)),
  seq
)
treat_one_chapter <- function(index, seqs, children) {
  yarn2 <- yarn
  yarn2$body <- fakify_xml(children[seqs[index][[1]]])
  title <- xml2::xml_text(children[seqs[index][[1]]][[1]]) |> tolower() |> snakecase::to_snake_case()

  filename <- if (title == "introduction") {
    "index.qmd"
  } else {
    sprintf("%s.qmd", title)
  }

  yarn$yaml <- ""
  yarn$write(filename)
  return(filename)
}

chapters_files <- purrr::map_chr(seq_along(headings), treat_one_chapter, seqs = seqs, children = children)

config <- c(
  "project:",
  "  type: book",
  "book:",
  '  title: "Frustration: One Year With R"',
  '  author: Reece Goding',
  '  description: "What follows is an account of my experiences from about one year of roughly daily R usage."',
  "  chapters:",
  paste(sprintf("  - %s", chapters_files), collapse = "\n")
)
brio::write_lines(
  config,
  "_quarto.yml"
)
quarto::quarto_render()
