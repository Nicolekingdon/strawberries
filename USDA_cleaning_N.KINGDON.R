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

strwb_survey$chemical <- str_trim(strwb_survey$chemical, side = "both")

strwb_survey <- strwb_survey |>
  mutate(toxicity = case_when(
    chemical %in% c("ABAMECTIN") ~ "Highly hazardous",
    chemical %in% c("ACEQUINOCYL", "BIFENTHRIN", "CARBARYL", "CHLORPYRIFOS", "CLOMAZONE", "COPPER HYDROXIDE", "CYPERMETHRIN", "DIAZINON", "DIFENOCONAZOLE", "DIMETHENAMID", "DIMETHYL DISULFIDE (DMDS)", "DODINE", "EMAMECTIN BENZOATE", "ENDOSULFAN", "FENAZAQUIN", "FENPROPATHRIN", "FENPYROXIMATE", "FLONICAMID", "FLUPYRADIFURONE", "FLUTRIAFOL", "GLUFOSINATE-AMMONIUM", "IMIDACLOPRID", "LAMBDA-CYHALOTHRIN", "METALDEHYDE", "METAM-SODIUM", "MYCLOBUTANIL", "NALED", "PARAQUAT", "PENDIMETHALIN", "PROPICONAZOLE", "PYRETHRINS", "PYRIDABEN", "SULFOXAFLOR", "TETRACONAZOLE", "THIAMETHOXAM", "THIRAM", "TRIFLUMIZOLE", "ZETA-CYPERMETHRIN") ~ "Moderately hazardous",
    chemical %in% c("BIFENAZATE", "BOSCALID", "CAPTON", "CHLORANTRANILIPROLE", "CHLOROTHALONIL", "CYANTRANILIPROLE", "CYAZOFAMID", "FENHEXAMID", "FLUDIOXONIL", "FLUROXYPYR 1-MHE", "FLUTOLANIL", "FOSETYL-AL", "HEXYTHIAZOX", "MANCOZEB", "NAPROPAMIDE", "NOVALURON", "OXYFLUORFEN", "PIPERONYL BUTOXIDE", "PYDIFLUMETOFEN", "PYRIPROXYFEN", "SIMAZINE", "SPINETORAM", "TERBACIL", "THIOPHANATE-METHYL", "TRIFLOXYSTROBIN") ~ "Unlikely to present acute hardzard",
    chemical %in% c("BUPROFEZIN", "CLOPYRALID MONO SALT", "CYFLUMETOFEN", "ETHEPHON", "ETOXAZOLE", "FENBUTATIN-OXIDE", "FLUBENDIAMIDE", "FLUMIOXAZIN", "FLUOPYRAM", "FLUXAPYROXAD", "GLYPHOSATE ISO. SALT", "GLYPHOSATE ISO. SALT", "IPRODIONE", "MALATHION", "PENTHIOPYRAD", "PYRIMETHANIL", "SPINOSAD", "SPIROTETRAMAT") ~ "Slightly hazardous",
    chemical %in% c("CHLOROPICRIN") ~ "Fatal if inhaled",
    chemical %in% c("DICHLOROPROPENE", "METHYL BROMIDE") ~ "Toxic if inhaled",
    TRUE ~ "Not specified"  
  ))

strwb_survey <- strwb_survey |>
  select(1:7, 9)

strwb_survey <- strwb_survey |>
  mutate("CAS#" = case_when(
    chemical %in% c("ABAMECTIN") ~ "71751-41-2",
    chemical %in% c("ACEQUINOCYL") ~ "57960-19-7",
    chemical %in% c("ACETAMIPRID") ~ "135410-20-7",
    chemical %in% c("ACIBENZOLAR-S-METHYL") ~ "135158-54-2",
    chemical %in% c("AUREOBASIDIUM PULLULANS DSM 14941") ~ "67891-88-7",
    chemical %in% c("AZADIRACHTIN") ~ "108168-76-9",
    chemical %in% c("AZOXYSTROBIN") ~ "131860-33-8",
    chemical %in% c("BIFENAZATE") ~ "149877-41-8",
    chemical %in% c("BIFENTHRIN") ~ "82657-04-3",
    chemical %in% c("BORAX DECAHYDRATE") ~ "1303-96-4",
    chemical %in% c("BOSCALID") ~ "188425-85-6",
    chemical %in% c("BUPROFEZIN") ~ "69327-76-0",
    chemical %in% c("CANOLA OIL") ~ "120962-03-0",
    chemical %in% c("CAPRIC ACID") ~ "334-48-5",
    chemical %in% c("CAPRYLIC ACID") ~ "124-07-2",
    chemical %in% c("CAPSAICIN") ~ "404-86-4",
    chemical %in% c("CAPTAN") ~ "133-06-2",
    chemical %in% c("CARBARYL") ~ "63-25-2",
    chemical %in% c("CARFENTRAZONE-ETHYL") ~ "128639-02-1",
    chemical %in% c("CHLORANTRANILIPROLE") ~ "500008-45-7",
    chemical %in% c("CHLOROPICRIN") ~ "76-06-2",
    chemical %in% c("CHLOROTHALONIL") ~ "1897-45-6",
    chemical %in% c("CHLORPYRIFOS") ~ "2921-88-2",
    chemical %in% c("CLETHODIM") ~ "99129-21-2",
    chemical %in% c("CLOMAZONE") ~ "81777-89-1",
    chemical %in% c("CLOPYRALID MONO SALT") ~ "57754-85-5",
    chemical %in% c("COPPER ETHANOLAMINE") ~ "14215-52-2",
    chemical %in% c("COPPER HYDROXIDE") ~ "20427-59-2",
    chemical %in% c("COPPER OCTANOATE") ~ "20543-04-8",
    chemical %in% c("COPPER OXIDE") ~ "12158-97-3",
    chemical %in% c("CYAZOFAMID") ~ "120116-88-3",
    chemical %in% c("CYFLUFENAMID") ~ "180409-60-3",
    chemical %in% c("CYFLUMETOFEN") ~ "400882-07-7",
    chemical %in% c("CYPERMETHRIN") ~ "52315-07-8",
    chemical %in% c("CYPRODINIL") ~ "121552-61-2",
    chemical %in% c("CYTOKININS") ~ "525-79-1",
    chemical %in% c("DECYLDIMETHYLOCTYL") ~ "32426-11-2",
    chemical %in% c("DIAZINON") ~ "333-41-5",
    chemical %in% c("DICHLOROPROPENE") ~ "78-88-6",
    chemical %in% c("DIDECYL DIM. AMMON.") ~ "148788-55-0",
    chemical %in% c("DIFENOCONAZOLE") ~ "	119446-68-3",
    chemical %in% c("DIMETHENAMID") ~ "87674-68-8",
    chemical %in% c("DIMETHYL DISULFIDE (DMDS)") ~ "624-92-0",
    chemical %in% c("DIMETHYLDIOCTYL") ~ "5538-94-3",
    chemical %in% c("DODINE") ~ "112-65-2; 15880-99-6; 2439-10-3; 51426-08-5; 96923-04-5",
    chemical %in% c("EMAMECTIN BENZOATE") ~ "155569-91-8",
    chemical %in% c("ENDOSULFAN") ~ "	115-29-7",
    TRUE ~ "Unknown"  
  ))