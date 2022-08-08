create_foldername <- function(x, pubNo){
  author1 <- str_split(x["author"], " and ")[[1]][1]
  allblanks <- str_locate_all(author1, " ")[[1]]
  author1 <- str_trim(str_sub(author1, allblanks[nrow(allblanks), 2], nchar(author1)))
  paste(pubNo, x[["year"]], author1, abbreviate(x[["journal"]], use.classes = TRUE), sep = "-")
}

# function to create the filename
create_filename <- function(x, ext = ".md") {
  author1 <- str_split(x["author"], " and ")[[1]]
  multiple_authors <- ifelse(length(author1)>1, TRUE, FALSE)
  author1 <- author1[1]
  allblanks <- str_locate_all(author1, " ")[[1]]
  author1 <- str_trim(str_sub(author1, max(allblanks), nchar(author1)))
  paste0(paste(author1, ifelse(multiple_authors, "etal", ""), x[["year"]], sep = "_"), ext)
} 

create_md <- function(x, 
                      pubNo,
                      lastOfyear,
                      folder.path,
                      open = TRUE,
                      abstract = TRUE,
                      url_pdf = NULL,
                      overwrite = FALSE)
  {
  # define a date and create filename by appending date 
  # and start of title
  if (!is.na(x[["year"]])) {
    x[["date"]] <- paste(x[["year"]], 
                         stringr::str_pad(lubridate::month(lubridate::now()), 2, pad = "0") ,
                         stringr::str_pad(lubridate::day(lubridate::now()), 2, pad = "0"),
                         sep = "-")
  } else {
    x[["date"]] <- "2999-01-01"
  }
  
  filename <- create_filename(x, ".md")
  
  # start writing
  if (!file.exists(file.path("content/publication", folder.path, filename)) | overwrite) {
    
    fileConn <- file.path("content/publication", folder.path, filename)
    write("+++", fileConn)
    
    # Title and date
    write(paste0("title = \"", stringr::str_to_sentence(x[["title"]]), "\""), fileConn, append = T)
    write(paste0("date = ", anydate(x[["date"]])), fileConn, append = T)
    
    # Authors. Comma separated list, e.g. `["Bob Smith", "David Jones"]`.
    auth_hugo <- str_split(str_split(x["author"], " and ")[[1]], " ")
    auth_hugo <- do.call(c, purrr::map(.x = auth_hugo, .f = ~{
      initials <- toupper(paste0(do.call(c, purrr::map(.x = .x[1:length(.x)-1], .f = ~stringr::str_sub(.x, 1, 1))), collapse = ""))
      paste0(.x[length(.x)], " ", initials)
    }))
    
    auth_bouchet <- which(grepl(pattern = "bouchet", ignore.case = TRUE, x = auth_hugo))
    auth_hugo[auth_bouchet] <- paste0("<b>", auth_hugo[auth_bouchet], "</b>")
    write(paste0("author_list = ", "\"", paste0(auth_hugo, collapse = ", "), "\""), fileConn, append = T)
    
    # Publication type. Legend:
    # 0 = Uncategorized, 1 = Conference paper, 
    # 2 = Journal article
    # 3 = Manuscript, 4 = Report, 5 = Book,  6 = Book
    # section
    write(paste0("publication_types = [\"", x[["pubtype"]], "\"]"), fileConn, append = T)
    
    # Publication details: journal, volume, issue, 
    # page numbers and doi link
    volume <- x[["volume"]]
    write(paste0("volume = ", volume), fileConn, append = T)
    
    pages <- x[["pages"]]
    write(paste0("pages = ", ifelse(is.null(pages), '""', pages)), fileConn, append = T)
    
    write(paste0("rank = ", pubNo), fileConn, append = T)
    
    write(paste0("open_access = ", tolower(open)), fileConn, append = T)
    
    write(paste0("last_of_year = ", ifelse(lastOfyear, "true", "false")), fileConn, append = T)
    
    # Abstract and optional shortened version.
    if (abstract) {
      write(paste0("abstract = \"", x[["abstract"]], "\""), fileConn, append = T)
    } else {
      write("abstract = \"\"", fileConn, append = T)
    }
    write(paste0("abstract_short = \"", "\""), fileConn, append = T)
    
    write(paste0("publication = ", "\"", x[["journal"]], "\""), fileConn, append = T)
    write(paste0("doi = ", "\"", x[["doi"]], "\""), fileConn, append = T)
    
    write(paste0("lay_brief = \"", "\""), fileConn, append = T)
    write(paste0("shields_badge = \"", "\""), fileConn, append = T)
    write(paste0("url_pdf = ", ifelse(is.null(url_pdf), "\"\"", url_pdf)), fileConn, append = T)
    write("featured = false", fileConn, append = T)
    
    # other possible fields are kept empty. They can be
    # customized later by editing the created md
    
    write("image_preview = \"\"", fileConn, append = T)
    write("selected = false", fileConn, append = T)
    write("projects = []", fileConn, append = T)
    write("tags = []", fileConn, append = T)
    
    #links
    write("url_preprint = \"\"", fileConn, append = T)
    write("url_code = \"\"", fileConn, append = T)
    write("url_dataset = \"\"", fileConn, append = T)
    write("url_project = \"\"", fileConn, append = T)
    write("url_slides = \"\"", fileConn, append = T)
    write("url_video = \"\"", fileConn, append = T)
    write("url_poster = \"\"", fileConn, append = T)
    write("url_source = \"\"", fileConn, append = T)
    write("url_supinfo = \"\"", fileConn, append = T)
    
    #other stuff
    write("math = true", fileConn, append = T)
    write("highlight = true", fileConn, append = T)
    
    # Featured image
    write("[header]", fileConn, append = T)
    write("image = \"\"", fileConn, append = T)
    write("caption = \"\"", fileConn, append = T)
    
    write("+++", fileConn, append = T)
  }
}

bibtex2academic <- function(bibfile, lastOfyear, ...) {
  
  require(RefManageR)
  require(dplyr)
  require(stringr)
  require(anytime)
  library(magrittr)
  
  all.dirs <- list.dirs(file.path("content/publication"))
  pubNo <- length(all.dirs)
  
  if(lastOfyear){
  previousfile <- readLines(file.path(all.dirs[pubNo], list.files(all.dirs[pubNo], pattern = ".md")))
  previousfile <- gsub("last_of_year = true", "last_of_year = false", previousfile)
  writeLines(previousfile, file.path(all.dirs[pubNo], list.files(all.dirs[pubNo], pattern = ".md")))
  }
  
  # Import the bibtex file and convert to data.frame
  mypubs <- RefManageR::ReadBib(bibfile, check = "warn", .Encoding = "UTF-8")
  
  folder.out <- create_foldername(data.frame(mypubs), pubNo)
  dir.create(path = paste0("content/publication/", folder.out))
  
  RefManageR::WriteBib(mypubs, file = file.path("content", "publication", 
    folder.out, create_filename(data.frame(mypubs), ".bib")), biblatex = TRUE)
  
  # assign "categories" to the different types of
  # publications
  mypubs <- mypubs %>%
    as.data.frame() %>%
    dplyr::mutate(
      pubtype = dplyr::case_when(
        bibtype == "Article" ~ "2",
        bibtype == "Article in Press" ~ "2",
        bibtype == "InProceedings" ~ "1",
        bibtype == "Proceedings" ~ "1",
        bibtype == "Conference" ~ "1",
        bibtype == "Conference Paper" ~ "1",
        bibtype == "MastersThesis" ~ "3",
        bibtype == "PhdThesis" ~ "3",
        bibtype == "Manual" ~ "4",
        bibtype == "TechReport" ~ "4",
        bibtype == "Book" ~ "5",
        bibtype == "InCollection" ~ "6",
        bibtype == "InBook" ~ "6",
        bibtype == "Misc" ~ "0",
        TRUE ~ "0"
      ))
  
  create_md(x = mypubs, folder.path = folder.out, lastOfyear = lastOfyear, pubNo = pubNo, ...)
}