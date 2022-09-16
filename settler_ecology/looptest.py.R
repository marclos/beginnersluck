

# Python program to illustrate
# while loop
count = 0
while (count < 3):   
  count = count + 1
  print("Hello Geek")
  
  
  #Python program to illustrate
  # combining else with while
  count = 0
  while (count < 3):   
    count = count + 1
  print("Hello Geek")
  else:
    print("In Else Block")
  
  
  n = 4
  for i in range(0, n):
    print(i)
library(jsonlite)
read_json_lines <- function(file){
  con <- file(file, open = "r")
  on.exit(close(con))
  jsonlite::stream_in(con, verbose = FALSE)
}

test.json <- read_json_lines("/home/CAMPUS/mwl04747/github/beginnersluck/settler_ecology/4b8a40f2-fa6f-ab9c-7719-d039a3c4cc81-jsonl.jsonl")
