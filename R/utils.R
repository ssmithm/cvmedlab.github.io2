#' Add a hyperlink
#'
#' Writes html code for a URL link to R Markdown format
#'
#' @param url_path string, URL
#' @param url_text string, URL link text to display
#'
#' @return html URL link
#' @export
#'
#' @examples
#' add_link("https://www.cvmedlab.org/", "cvmedlab")
#'
add_link <- function(url_path, url_text) {
  html <- glue::glue("<a href="{url_path}">{url_text}</a>")
  cat(html, sep="")
}

#' Add a link and icon
#'
#' Writes html for a URL link to R Markdown with FontAwesome icon
#'
#' @param url_path string, URL (web address)
#' @param url_text string, URL link text to display
#' @param icon_class string, font awesome icon class
#'
#' @return html URL link with icon and text
#' @export
#'
#' @examples
#' add_link_icon("https://www.cvmedlab.org", "cvmedlab", "fas fa-globe")
#'
add_link_icon <- function(url_path, url_text, icon_class) {
  html <- glue::glue('<a href="{url_path}"><i class="{icon_class}">{url_text}</i></a>')
  cat("  ",html, sep="")
}

#' Generate a publication list from a bib file
#'
#' @description This function accepts a .bib file as input, along with a specially formatted .yml file and embeds a publication list into an R Markdown document.
#'
#' @param bib a .bib file
#' @param yml a formatted .yml file see example
#' @param pdf_dir relative path to directory containing pdfs
#' @param base_url_to_pdfs url to path containing pdfs
#'
#' @return publication list with links
#' @export
#'
#' @examples
#' \dontrun{
#' pub_list("lab_pubs.bib",
#' "lab_pubs.yml",
#' "lab_pubs/files",
#' "https://www.crumplab.com/Crump/files")
#' }
pub_list <- function(bib, yml, pdf_dir, base_url_to_pdfs) {
  
  # load bib file to df
  pubs <- bib2df::bib2df(bib)
  
  # clean {{}} from entries
  # to do: improve this part
  pubs$TITLE <- stringi::stri_replace_all_regex(pubs$TITLE, "[\\{\\}]", "")
  pubs$JOURNAL <- stringi::stri_replace_all_regex(pubs$JOURNAL, "[\\{\\}]", "")
  pubs$BOOKTITLE <- stringi::stri_replace_all_regex(pubs$BOOKTITLE, "[\\{\\}]", "")
  
  # sort descending by year
  # to do: add sort options
  pubs <- pubs[order(pubs$YEAR, decreasing=T),]
  
  # read yml with links for bib entries
  yml_links <- yaml::read_yaml(yml)
  
  # print entries
  for (i in 1:dim(pubs)[1] ){
    
    # convert row to .bib entry
    # to do: make row to bib entry a function
    t_bib_entry <- paste0(capture.output(bib2df::df2bib(pubs[i,])), collapse="")
    # generate markdown text for citation
    t_md_citation<- paste0(stevemisc::print_refs(t_bib_entry,
                                                 csl = "jama.csl",
                                                 spit_out = FALSE,
                                                 delete_after = FALSE), 
                           collapse=" ")
    cat(t_md_citation)
    
    cat("<span class='publinks'>")
    
    ### add pdf links
    if( !is.na(pubs$FILE[i]) ) { #check pdf exists
      
      pdf_name <- basename(pubs$FILE[i])
      rel_path_to_pdf <- list.files(here::here(pdf_dir),
                                    basename(pubs$FILE[i]),
                                    recursive=T)
      build_url <- paste0(base_url_to_pdfs, "/", rel_path_to_pdf,collapse="")
      crumplab::add_link_icon(build_url, "pdf", "fas fa-file-pdf")
      
    }
    
    ## add all other links
    if( exists(pubs$BIBTEXKEY[i],yml_links) ) { # check yml bib entry exists
      
      link_list <- yml_links[[pubs$BIBTEXKEY[i]]]
      
      for(l in link_list){
        add_link_icon(l$url,l$name,l$icon)
      }
      
    }
    cat("</span>")
    cat("\n\n")
  }
  
}


#' Print and Format \code{.bib} Entries as References
#'
#' @description \code{print_refs()} is a convenience function that prints and formats \code{.bib}
#' entries as if they were references. This function is useful if you want to load a \code{.bib}
#' entry or set of entries and print them in the middle of a document in R Markdown.
#'
#' @details \code{print_refs()} assumes an active internet connection in the absence of the appropriate CSL file in the
#' working directory. The citation style language (CSL) file supplied by the user must match a file in the
#' massive Github repository of CSL files. Users interested in potential outputs should read more about Pandoc (\url{https://pandoc.org/MANUAL.html}).
#' The Github repository of CSL files is available here: \url{https://github.com/citation-style-language/styles}.
#'
#' @param bib a valid \code{.bib} entry
#' @param csl a CSL file, matching one available on the Github repository, that the user wants to format the references. Default is "american-political-science-association.csl".
#' @param toformat the output wanted by the user. Default is "markdown_strict".
#' @param cslrepo a directory of CSL files. Defaults to the one on Github.
#' @param spit_out logical, defaults to TRUE. If TRUE, wraps ("spits out") formatted citations in a \code{writeLines()} output for the console. If `FALSE`, returns a character vector.
#' @param delete_after logical, defaults to TRUE. If TRUE, deletes CSL file when it's done. If FALSE, retains CSL for (potential) future use.
#'
#' @return  \code{print_refs()} takes a \code{.bib} entry and returns the
#' requested formatted reference or references from it.
#'
#' @examples
#'
#' \donttest{
#' example <- "@Book{vasquez2009twp, Title = {The War Puzzle Revisited},
#' Author = {Vasquez, John A}, Publisher = {New York, NY: Cambridge University Press},
#' Year = {2009}}"
#'
#' print_refs(example)
#' }

print_refs <- function(bib, 
                       csl="jama.csl",
                       toformat="markdown_strict",
                       cslrepo="https://raw.githubusercontent.com/citation-style-language/styles/master",
                       spit_out = TRUE,
                       delete_after = TRUE) {
  
  if (any(class(bib) %in% c("data.frame")) == TRUE) {
    
    tmpbib <- c()
    
    # bib <- capture.output(df2bib(bib))
    
    not_all_na <- function(x) any(!is.na(x))
    
    bib %>%
      group_split(.data$BIBTEXKEY) -> group_split_cites
    
    lapply(group_split_cites, function(x) select_if(x, not_all_na)) -> group_split_cites
    
    
    suppressWarnings(
      for(i in 1:length(group_split_cites)) {
        group_split_cites[[i]]$AUTHOR <- paste(unlist(group_split_cites[[i]]$AUTHOR), collapse=" and ")
        group_split_cites[[i]]$EDITOR <- paste(unlist(group_split_cites[[i]]$EDITOR), collapse=" and ")
      }
    )
    
    
    lapply(group_split_cites, function(x) mutate(x,  EDITOR = ifelse(.data$EDITOR == "", NA, .data$EDITOR))) -> group_split_cites
    lapply(group_split_cites, function(x) select_if(x, not_all_na)) -> group_split_cites
    
    
    for(i in 1:length(group_split_cites)) {
      tibble(x = names(unlist(group_split_cites[[i]])),
             y = unlist(group_split_cites[[i]])) -> hold_this
      
      hold_this %>% filter((x %in% c("BIBTEXKEY", "CATEGORY"))) -> hold_this_a
      hold_this %>% filter(!(x %in% c("BIBTEXKEY", "CATEGORY"))) -> hold_this_b
      
      hold_this_cite <- capture.output(cat(paste0("@", hold_this_a$y[1],
                                                  "{", hold_this_a$y[2],",\n",
                                                  paste0("  ",
                                                         hold_this_b$x,
                                                         " = {",
                                                         hold_this_b$y,
                                                         "}",
                                                         collapse = ",\n"),"}"),
                                           collapse = "\n\n",
                                           #fill=TRUE,
                                           file = "",
                                           append = TRUE))
      #invisible(file)
      tmpbib <- c(tmpbib, hold_this_cite)
    }
    
    bib <- tmpbib
  }
  
  
  
  
  if (!file.exists(bib)) {
    message("I'm going to assume this is a .bib entry...")
    tmpbib <- tempfile(fileext = ".bib")
    on.exit(unlink(tmpbib), add=TRUE)
    if(!validUTF8(bib)) {
      bib <- iconv(bib, to="UTF-8")
    }
    writeLines(bib, tmpbib)
    bib <- tmpbib
  }
  
  if (tools::file_ext(csl)!="csl") {
    warning("End the CSL file in '.csl', you knob.")
  }
  
  if (!file.exists(csl)) {
    cslurl <- file.path(cslrepo, csl)
    message(paste("Downloading CSL from", cslurl))
    cslresp <- GET(cslurl, write_disk(csl))
    if(http_error(cslresp)) {
      stop(paste("Could not download CSL.", "Code:", status_code(cslresp)))
    }
  }
  
  tmpcit <- tempfile(fileext = ".md")
  on.exit(unlink(tmpcit), add=TRUE)
  
  writeLines(c("---","nocite: '@*'","---"), tmpcit)
  find_pandoc()
  command <- paste(shQuote(file.path(find_pandoc()$dir, "pandoc")),
                   "--citeproc",
                   "--to", shQuote(toformat),
                   "--csl", shQuote(csl),
                   "--bibliography", shQuote(bib),
                   shQuote(tmpcit))
  .with_pandoc_safe_environment({
    result <- system(command, intern = TRUE)
    Encoding(result) <- "UTF-8"
  })
  
  if (file.exists(csl) && delete_after == TRUE) {
    #Delete file if it exists
    file.remove(csl)
  }
  
  if (toformat == "latex") {
    result <- str_subset(result, "\\leavevmode|\\\\begin|\\\\end|\\\\hyper", negate=TRUE)
    result <- str_replace(result, "\\{``", '"')
    result <- str_replace(result, "''\\}", '"')
    
  }
  
  if (spit_out == TRUE) {
    writeLines(result, sep="\n")
  } else {
    return(result)
  }
}


#' @keywords internal
#' @export
# https://github.com/cran/rmarkdown/blob/d53194ce5eb633397c40d1c7d3462fc4a0eb61ff/R/pandoc.R
.with_pandoc_safe_environment <- function(code) {
  
  lc_all <- Sys.getenv("LC_ALL", unset = NA)
  
  if (!is.na(lc_all)) {
    Sys.unsetenv("LC_ALL")
    on.exit(Sys.setenv(LC_ALL = lc_all), add = TRUE)
  }
  
  lc_ctype <- Sys.getenv("LC_CTYPE", unset = NA)
  
  if (!is.na(lc_ctype)) {
    Sys.unsetenv("LC_CTYPE")
    on.exit(Sys.setenv(LC_CTYPE = lc_ctype), add = TRUE)
  }
  
  if (Sys.info()['sysname'] == "Linux" &&
      is.na(Sys.getenv("HOME", unset = NA))) {
    stop("The 'HOME' environment variable must be set before running Pandoc.")
  }
  
  if (Sys.info()['sysname'] == "Linux" &&
      is.na(Sys.getenv("LANG", unset = NA))) {
    # fill in a the LANG environment variable if it doesn't exist
    Sys.setenv(LANG = .detect_generic_lang())
    on.exit(Sys.unsetenv("LANG"), add = TRUE)
  }
  
  if (Sys.info()['sysname'] == "Linux" &&
      identical(Sys.getenv("LANG"), "en_US")) {
    Sys.setenv(LANG = "en_US.UTF-8")
    on.exit(Sys.setenv(LANG = "en_US"), add = TRUE)
  }
  
  force(code)
}


#' @keywords internal
#' @export

.detect_generic_lang <- function() {
  
  locale_util <- Sys.which("locale")
  
  if (nzchar(locale_util)) {
    locales <- system(paste(locale_util, "-a"), intern = TRUE)
    locales <- suppressWarnings(
      strsplit(locales, split = "\n", fixed = TRUE)
    )
    if ("C.UTF-8" %in% locales)
      return("C.UTF-8")
  }
  
  # default to en_US.UTF-8
  "en_US.UTF-8"
}
