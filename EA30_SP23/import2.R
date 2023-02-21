## This documents how to read a csv file into R

# find the path and file name of our csv

file.choose()

## use the file path and name to create an object to reference in the read function

sonde.csv = "/home/mwl04747/beginnersluck/EA30_SP23/Field Data Recording (master sheet) - Data.csv"

## read data into R... many statements in R are have specific vocabulary

sonde = read.csv(sonde.csv)

