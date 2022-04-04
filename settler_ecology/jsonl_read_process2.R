library(jsonlite)
library(rtweet)

## Replace '...' with the directory that has your files
setwd("/home/CAMPUS/mwl04747/github/beginnersluck/settler_ecology")
files = dir(pattern = "*.jsonl$")

## As many JSON fields as you want go here, for instance:
cols = c(
  "datePublished",
  "fullText",
  "id"
)

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

BotGaz <- read.csv("4b8a40f2-fa6f-ab9c-7719-d039a3c4cc81-csv.jsonl.csv")

library(dplyr)
library(tidytext)

BotGaz %>%
  count(fullText, sort = TRUE)

sum(+(grepl("Indian", BotGaz$fullText)))
BotGaz$Indian = +(grepl("Indian\\b", BotGaz$fullText) & !grepl("India\\b", BotGaz$fullText)); 
sum(BotGaz$Indian)
BotGaz$Native = +(grepl("Native", BotGaz$fullText))
BotGaz$native = +(grepl("native", BotGaz$fullText))
BotGaz$indigenous = +(grepl("indigenous", ignore.case = TRUE, BotGaz$fullText))
sum(BotGaz$indigenous)
library(stringr)

new_vector <- ifelse(str_detect(urls, "apples"), 1, 0)

plot(indigenous~datePublished, BotGaz)
plot(Indian~datePublished, BotGaz)
str(BotGaz)

BotGaz[BotGaz$Indian==1,c(1,2)]
head(BotGaz[order(BotGaz$Native, decreasing=T),])
head(BotGaz[order(BotGaz$indigenous, decreasing=T),])
