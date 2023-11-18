library(httr)
library(rvest)
library(xml2)

# UA used to scrap target.
user_agent <- "Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/77.0.3865.90 Safari/537.36"

# Root url where we can find the Pfizer Docs.
docs_url <- 'https://phmpt.org/multiple-file-downloads/'
print(paste("Getting index on", docs_url))

# Send a GET request
res <- GET(docs_url, user_agent(user_agent))

# Check if the request is successful
if(http_error(res)){
  stop(paste("Failed to get", docs_url))
}

# Parse the content
content <- content(res, as="text")
tree <- read_html(content)

# Create zip_data and xpt_data directories if they don't exist
prod_path <- "prod_data"
if (!dir.exists(prod_path)) {
  dir.create(prod_path)
}

# Downloads each file.
trs <- html_nodes(tree, 'tbody tr')
for(i in 1:length(trs)){
  tr <- trs[i]
  tds <- html_nodes(tr, 'td')
  if(length(tds) > 0){
    file_name <- html_text(tds[1])
	print(file_name)
    file_name <- tolower(file_name)
    file_date <- html_text(tds[2])
    file_size <- html_text(tds[3])
    file_url <- html_attr(html_node(tds[4], 'a'), 'href')
    online_file <- basename(file_url)
    file_ext <- tools::file_ext(online_file)
    if(grepl("pd-production*", file_name)){
      local_file <- ifelse(file_ext == "zip", paste0(prod_path, "/", online_file), 
                      stop(paste0("Unknown extension : [", file_ext, "] on file [", file_name, "] (", online_file, "), contact the script authors to obtain an update.")))
      if (!file.exists(local_file)) {
        print(paste0("Downloading [", local_file, "] from [", file_url, "]"))
        download.file(file_url, local_file, mode = "wb")
      }
    }
  }
}


