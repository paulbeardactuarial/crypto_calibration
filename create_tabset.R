check_is_rendering <- function() {
  Sys.getenv("QUARTO_PROJECT_ROOT") != ""
}

create_tabset_for_console <- function(x, title = NULL) {
  if (!is.list(x) || is.null(names(x))) {
    stop("Input must be a *named* list.")
  }

  if (!is.null(title)) {
    cat("##", title, "{.tabset}\n\n")
  } else {
    cat("## Output {.tabset}\n\n")
  }

  purrr::iwalk(x, function(value, name) {
    cat("###", name, "\n\n")

    if (is.character(value)) {
      cat(paste(value, collapse = "\n"), "\n\n")
    } else if (is.data.frame(value)) {
      print(gt::gt(value))
    } else {
      print(value)
    }

    cat("\n")
  })
}


create_tabset_for_render <- function(list) {
  chunk <- purrr::imap_chr(list, function(x, y) {
    knitr::knit_child(
      text = c(
        "## `r y`",
        "",
        "```{r}",
        "#| echo: false",
        "x",
        "```",
        "",
        ""
      ), envir = environment(), quiet = TRUE
    )
  })
  cat(chunk, sep = "\n")
}


create_tabset <- function(list, is_rendering = check_is_rendering()) {
  if (is_rendering) {
    create_tabset_for_render(list)
  } else {
    create_tabset_for_console(list)
  }
}
