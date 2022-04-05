library(jsonlite)
library(rtweet)
library(R.utils)
library(dplyr)
library(tidytext)

## gzunip files
setwd("/home/CAMPUS/mwl04747/github/beginnersluck/settler_ecology/data")
files = dir(pattern = "*.gz$")

#gunzip("file.gz", remove=FALSE)

for (i in 1:length(files)){
  start = Sys.time()
  gunzip(files[i], remove=FALSE, skip=TRUE)
  end = Sys.time()
  message(files[i], ": ", round(difftime(end, start, units = "mins"), 2), 
    " minutes for ", files[i], " uncompressing")
}


## As many JSON fields as you want go here, for instance:
cols = c(
  "datePublished",
  "isPartOf",
  "abstract",
  "fullText",
  "id"
)

files = dir(pattern = "*.jsonl$")
## Loop for all jsonl files -> csv
for (i in 1:length(files)){
  start = Sys.time()
  lines = readLines(files[i])
  temp = do.call(
    rbind, 
    lapply(
      lines, function(x)
        unlist(jsonlite::fromJSON(x))[cols]
    )
  )
  colnames(temp) = cols
  y = as.data.frame(temp)
  save_as_csv(y, sub("jsonl", "csv", files[i]), prepend_ids = T, fileEncoding = "UTF-8")
  end = Sys.time()
  message(files[i], ": ", round(difftime(end, start, units = "mins"), 2), 
          " minutes for ", length(lines), " articles")
  # file.remove(files[i])
  ## Uncomment above if you want the jsonl files to be permanently deleted (like if space is an issue)
}

#BotGaz <- read.csv("4b8a40f2-fa6f-ab9c-7719-d039a3c4cc81-csv.jsonl.csv")




