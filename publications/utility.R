revert_bib_title <- function(title) {
  chars <- strsplit(title, "")[[1]]
  n <- length(chars)
  out_chars <- character(0)
  inside_out <- logical(0)
  depth <- 0
  j <- 0
  for (i in seq_len(n)) {
    ch <- chars[i]
    if (ch == "{") {
      depth <- depth + 1
      next
    } else if (ch == "}") {
      depth <- max(depth - 1, 0)
      next
    } else {
      j <- j + 1
      out_chars[j] <- ch
      inside_out[j] <- depth > 0
    }
  }
  out_chars <- out_chars[seq_len(j)]
  inside_out <- inside_out[seq_len(j)]
  orig_chars <- out_chars
  out_chars <- tolower(out_chars)
  out_chars[1] <- toupper(out_chars[1])
  out_chars[inside_out] <- orig_chars[inside_out]
  return(paste0(out_chars, collapse = ""))
}

revert_bib_author <- function(author) {
  parts <- strsplit(author, "\\s+and\\s+", perl = TRUE)[[1]]
  parts <- trimws(parts)
  return(as.list(parts))
}

parse_bib_entry <- function(entry_lines) {
  entry_lines <- trimws(entry_lines)
  key <- sub("^@[a-zA-Z]+\\{([^,]+),.*", "\\1", entry_lines[1])
  fields <- entry_lines[grepl("=", entry_lines)]
  res <- list()
  for (ln in fields) {
    field <- sub("^([a-zA-Z]+)\\s*=.*", "\\1", ln)
    value <- sub("^[a-zA-Z]+\\s*=\\s*\\{?(.*?)\\}?,?$", "\\1", ln)
    if (field == "title") {
      value <- revert_bib_title(value)
    } else if (field == "author") {
      value <- revert_bib_author(value)
    }
    res[[field]] <- value
  }
  res <- list(res)
  names(res) <- key
  return(res)
}

read_bibtex <- function(file) {
  lines <- trimws(readLines(file, warn = FALSE))
  init <- grep("^@", lines)
  bib <- data.frame(
    entry = sub("^@([a-zA-Z]+)\\{.*", "\\1", lines[init]),
    init = init,
    end = c(init[-1] - 1, length(lines)))
  res <- sapply(seq_len(nrow(bib)), function(k) {
    entry_lines <- lines[seq(bib$init[k], bib$end[k])]
    entry_lines <- entry_lines[entry_lines != ""]
    parse_bib_entry(entry_lines)
  })
  return(res)
}

print_entry <- function(key, file = "publications/refs.bib") {
  lines <- readLines(file, warn = FALSE)
  pattern <- paste0("^@", ".*\\{", key, "\\s*,")
  init <- grep(pattern, lines)
  end <- init
  while (end <= length(lines) && !grepl("^}\\s*,?\\s*$", lines[end])) {
    end <- end + 1
  }
  cat("```bibtex\n")
  cat(lines[init:end], sep = "\n")
  cat("```\n")
}
