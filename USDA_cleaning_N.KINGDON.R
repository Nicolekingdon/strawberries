##Nicole Kingdon##
#Pesticides & Strawberries#

library(knitr)  
library(kableExtra)
library(tidyverse)
library(stringr)

strawberry <- read_csv("strawberry.csv", col_names = TRUE)

drop_one_value_col <- function(df){
  drop <- NULL
  for(i in 1:dim(df)[2]){
    if((df |> distinct(df[,i]) |> count()) == 1){
      drop = c(drop, i)
    } }
  
  if(is.null(drop)){return("none")}else{
    
    print("Columns dropped:")
    print(colnames(df)[drop])
    strawberry <- df[, -1*drop]
  }
}

strwb_census <- strawberry |> filter(Program == "CENSUS")

strwb_census <- strwb_census |>
  separate_wider_delim(  cols = `Data Item`,
                         delim = ",",
                         names = c("Fruit",
                                   "temp1",
                                   "temp2",
                                   "temp3"),
                         too_many = "error",
                         too_few = "align_start"
  )

strwb_census <- strwb_census |>
  separate_wider_delim(  cols = temp1,
                         delim = " - ",
                         names = c("crop_type",
                                   "prop_acct"),
                         too_many = "error",
                         too_few = "align_start"
  )

strwb_census$crop_type <- str_trim(strwb_census$crop_type, side = "both")

strwb_census$temp2 <- str_trim(strwb_census$temp2, side = "both")

strwb_census$temp3 <- str_trim(strwb_census$temp3, side = "both")

strwb_census <- strwb_census |> mutate(`Fresh Market` = temp2, .after = temp2)

strwb_census$`Fresh Market` <- strwb_census$`Fresh Market` |> str_replace( "^MEA.*", "")

strwb_census$`Fresh Market` <- strwb_census$`Fresh Market` |> str_replace( "^P.*", "")

strwb_census$`Fresh Market`[is.na(strwb_census$`Fresh Market`)] <- ""  

strwb_census$temp2 <- strwb_census$temp2 |> str_replace("^F.*", "")

strwb_census$`Fresh Market` <- strwb_census$`Fresh Market` |> str_replace("^FRESH MARKET - ", "")

strwb_census <- strwb_census |> mutate(`Process Market` = temp2, .after = temp2)

strwb_census$`Process Market` <-  strwb_census$`Process Market` |> str_replace("^MEA.*", "")

strwb_census$`Process Market`[is.na(strwb_census$`Process Market`)] <- ""

strwb_census$temp2 <- strwb_census$temp2 |> str_replace("^P.*", "")

strwb_census$`Process Market` <-  strwb_census$`Process Market` |> str_replace("PROCESSING - ", "") 

strwb_census$prop_acct[is.na(strwb_census$prop_acct)] <- "" 

strwb_census$temp2[is.na(strwb_census$temp2)] <- "" 

strwb_census$temp3[is.na(strwb_census$temp3)] <- "" 

vals <- strwb_census$Value
c <- vals |> str_replace_all(",", "")
c <- as.numeric(c)

dcomma <- function(c){
  xnew = as.numeric(gsub(",", "", c))
  fns = unique(c[is.na(xnew)])
  vtran = list("new_vec" = xnew, "footnotes" = fns)
  return(vtran)
}

v <- strwb_census$Value
fn_i <- v |> str_detect("^\\([:upper:]\\)$")
v1 <- dcomma(v)
na_i <- is.na(v1)
length(v) == sum(na_i == fn_i)
dcomma <- function(c){
  xnew = as.numeric(gsub(",", "", c))
  fns = unique(c[is.na(xnew)])
  vtran = list("new_vec" = xnew, "footnotes" = fns)
  return(vtran)
}
v_trns <- dcomma(v)


a <- v_trns$new_vec

v_trns$footnotes

##Strawberry Survey

strwb_survey <- strawberry |> filter(Program == "SURVEY")
c8 <- distinct(strwb_survey[,8])
c8 |> kable()

per_c <- strwb_survey |> select(Period) |> distinct()
per_c <- unlist(per_c)

strwb_survey <- strwb_survey |>
  separate_wider_delim(  cols = Period,
                         delim = ",",
                         names = c("type",
                                   "subtype"),
                         too_many = "error",
                         too_few = "align_start"
  )

strwb_survey <- strwb_survey |> 
  separate_wider_delim(  cols = `Data Item`,
                         delim = ",",
                         names = c("temp1",
                                   "temp2",
                                   "temp3",
                                   "temp4"),
                         too_many = "error",
                         too_few = "align_start"
  )

strwb_survey <- strwb_survey |>
  separate_wider_delim(  cols = temp1,
                         delim = " - ",
                         names = c("temp1a",
                                   "temp1b"),
                         too_many = "error",
                         too_few = "align_start"
  )

strwb_survey <- strwb_survey |>
  separate_wider_delim(  cols = Domain,
                         delim = ",",
                         names = c("temp22",
                                   "temp23"),
                         too_many = "error",
                         too_few = "align_start"
  )


t22 <- unique(strwb_survey$temp22)

t23 <- unique(strwb_survey$temp23)

strwb_survey <- strwb_survey |>
  separate_wider_delim(  cols = `Domain Category`,
                         delim = ",",
                         names = c("temp42",
                                   "temp43",
                                   "temp44",
                                   "temp45"),
                         too_many = "error",
                         too_few = "align_start"
  )

strwb_survey <- strwb_survey |>
  filter(temp42 == "CHEMICAL") |>
  select(1:3, 7, 17, 23, 24, 26, 29) |>
  arrange(State)

strwb_survey <- strwb_survey |>
  separate_wider_delim(  cols = temp43,
                         delim = ":",
                         names = c("temp43",
                                   "chemical"
                                  ),
                         too_many = "error",
                         too_few = "align_start"
  )

strwb_survey <- strwb_survey |>
  select(1, 2, 4, 5, 7, 9, 10)

strwb_survey <- strwb_survey |>
  separate_wider_delim(  cols = chemical,
                         delim = "=",
                         names = c("chemical",
                                   "pc#"
                         ),
                         too_many = "error",
                         too_few = "align_start"
  )

strwb_survey$chemical <- str_replace(strwb_survey$chemical, "\\(", "")
strwb_survey$"pc#" <- str_replace(strwb_survey$"pc#", "\\)", "")

strwb_survey <- strwb_survey |>
  na.omit()

strwb_survey <- strwb_survey |>
  arrange(chemical)

strwb_survey <- strwb_survey |>
  rename(type = temp23)
