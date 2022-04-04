library(jsonlite)
library(R.utils)
library(ndjson)

# gunzip("/home/CAMPUS/mwl04747/github/beginnersluck/settler_ecology/4b8a40f2-fa6f-ab9c-7719-d039a3c4cc81-jsonl.jsonl.gz")


library(RJSONIO)
JSONL = "/home/CAMPUS/mwl04747/github/beginnersluck/settler_ecology/TorreyBotClub.jsonl"
TorreyBotClub_Raw <-fromJSON(JSONL)

str(TorreyBotClub_Raw)

TorreyBotClub <- TorreyBotClub_Raw[['fullText']]

library(jsonlite)
library(rtweet)

## Replace '...' with the directory that has your files
setwd("...")
files = dir(pattern = "*.jsonl$")

## As many JSON fields as you want go here, for instance:
cols = c(
  "created_at",
  "id",
  "id_str",
  "full_text"
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
          " minutes for ", length(lines), " tweets")
  # file.remove(files[i])
  ## Uncomment above if you want the jsonl files to be permanently deleted (like if space is an issue)
}

with open(JSONL, mode='r') as infile:
  for line in infile:
  json_line = loads(line)
# now do the thing






read_json_lines <- function(file){
  con <- file(file, open = "r")
  on.exit(close(con))
  jsonlite::stream_in(con, verbose = FALSE)
}

test <- read_json_lines("/home/CAMPUS/mwl04747/github/beginnersluck/settler_ecology/4b8a40f2-fa6f-ab9c-7719-d039a3c4cc81-jsonl.jsonl")

df <- ndjson::stream_in('huge_file.jsonl')

library(jsonlite)

# link to the API output as a JSON file
url_json <- "https://site.web.api.espn.com/apis/fitt/v3/sports/football/nfl/qbr?region=us&lang=en&qbrType=seasons&seasontype=2&isqualified=true&sort=schedAdjQBR%3Adesc&season=2019"

# get the raw json into R
raw_json <- jsonlite::fromJSON(url_json)

# get names of the QBR categories
category_names <- pluck(raw_json, "categories", "labels", 1)

# Get the QBR stats by each player (row_n = player)
get_qbr_data <- function(row_n) {
  purrr::pluck(raw_json, "athletes", "categories", row_n, "totals", 1) %>% 
    as.double() %>% 
    set_names(nm = category_names)
}

# create the dataframe and tidy it up
ex_output <- pluck(raw_json, "athletes", "athlete") %>%
  as_tibble() %>%
  select(displayName, teamName:teamShortName, headshot) %>%
  mutate(data = map(row_number(), get_qbr_data)) %>% 
  unnest_wider(data) %>% 
  mutate(headshot = pluck(headshot, "href"))

glimpse(ex_output)
                