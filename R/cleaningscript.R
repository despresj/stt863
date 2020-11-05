library(tidyverse)

# Read and Clean ----------------------------------------------------------
files <- list.files(path = here::here("raw_data"), pattern = ".csv")
files

cleaning <- function(df){
  df <- pivot_longer(df, cols = -country, names_to = "year")
}

data <- files %>% 
  map(function(x) read_csv(paste0("raw_data/", x))) %>% 
  setNames(gsub("\\.csv$", "", files)) %>% 
  map(cleaning) %>% 
  bind_rows(.id = "id") %>% 
  pivot_wider(names_from = id)



# Filtering ---------------------------------------------------------------


# data <- read_csv(here::here("data", "df.csv")) 
OECD <- tibble::tribble(
          ~Country,            ~Application,      ~Negotiations,            ~Invitation,    ~`Membership[1]`, ~Geographic.location,                                                                                                                                                                           ~Notes,
       "Australia",                      NA,                 NA,                     NA,       "7 June 1971",            "Oceania",                                                                                                                                                                               NA,
         "Austria",                      NA,                 NA,                     NA, "29 September 1961",             "Europe",                                                                                                                                                                "OEEC member.[6]",
         "Belgium",                      NA,                 NA,                     NA, "13 September 1961",             "Europe",                                                                                                                                                                "OEEC member.[6]",
          "Canada",                      NA,                 NA,                     NA,     "10 April 1961",      "North America",                                                                                                                                                                               NA,
           "Chile", "November 2003[57][58]",  "16 May 2007[26]", "15 December 2009[59]",        "7 May 2010",      "South America",                                                                                                                                                                               NA,
        "Colombia",   "24 January 2011[60]",  "30 May 2013[30]",      "25 May 2018[61]",     "28 April 2020",      "South America",                                                                                                                                                                               NA,
  "Czech Republic",      "January 1994[62]",  "8 June 1994[63]", "24 November 1995[62]",  "21 December 1995",             "Europe",                                                                                                 "Was a member of the rival Comecon from 1949 to 1991 as part of Czechoslovakia.",
         "Denmark",                      NA,                 NA,                     NA,       "30 May 1961",             "Europe",                                                                                                                                                                "OEEC member.[6]",
         "Estonia",                      NA,  "16 May 2007[26]",      "10 May 2010[64]",   "9 December 2010",             "Europe",                                                                                                                                                                               NA,
         "Finland",                      NA,                 NA,                     NA,   "28 January 1969",             "Europe",                                                                                                                                                                               NA,
          "France",                      NA,                 NA,                     NA,     "7 August 1961",             "Europe",                                                                                                                                                                "OEEC member.[6]",
         "Germany",                      NA,                 NA,                     NA, "27 September 1961",             "Europe", "Joined OEEC in 1949 (West Germany).[65] Previously represented by the Trizone.[6] East Germany was a member of the rival Comecon from 1950 until German reunification in 1990.",
          "Greece",                      NA,                 NA,                     NA, "27 September 1961",             "Europe",                                                                                                                                                                "OEEC member.[6]",
         "Hungary",     "December 1993[66]",  "8 June 1994[63]",                     NA,        "7 May 1996",             "Europe",                                                                                                                           "Was a member of the rival Comecon from 1949 to 1991.",
         "Iceland",                      NA,                 NA,                     NA,       "5 June 1961",             "Europe",                                                                                                                                                                "OEEC member.[6]",
         "Ireland",                      NA,                 NA,                     NA,    "17 August 1961",             "Europe",                                                                                                                                                                "OEEC member.[6]",
          "Israel",     "15 March 2004[67]",  "16 May 2007[26]",      "10 May 2010[64]",  "7 September 2010",          "West Asia",                                                                                                                                                                               NA,
           "Italy",                      NA,                 NA,                     NA,     "29 March 1962",             "Europe",                                                                                                                                                                "OEEC member.[6]",
           "Japan",     "November 1962[68]",                 NA,        "July 1963[68]",     "28 April 1964",          "East Asia",                                                                                                                                                                               NA,
     "South Korea",     "29 March 1995[69]",                 NA,  "25 October 1996[70]",  "12 December 1996",          "East Asia",                                                                                                                                               "Officially the Republic of Korea",
          "Latvia",                      NA,  "29 May 2013[71]",      "11 May 2016[72]",   "1 July 2016[73]",             "Europe",                                                                                                                                                                               NA,
       "Lithuania",                      NA, "9 April 2015[74]",          "31 May 2018",   "5 July 2018[75]",             "Europe",                                                                                                                                                                               NA,
      "Luxembourg",                      NA,                 NA,                     NA,   "7 December 1961",             "Europe",                                                                                                                                                                "OEEC member.[6]",
          "Mexico",                      NA,                 NA,    "14 April 1994[76]",       "18 May 1994",      "North America",                                                                                                                                                                               NA,
     "Netherlands",                      NA,                 NA,                     NA,  "13 November 1961",             "Europe",                                                                                                                                                                "OEEC member.[6]",
     "New Zealand",                      NA,                 NA,                     NA,       "29 May 1973",            "Oceania",                                                                                                                                                                               NA,
          "Norway",                      NA,                 NA,                     NA,       "4 July 1961",             "Europe",                                                                                                                                                                "OEEC member.[6]",
          "Poland",   "1 February 1994[77]",  "8 June 1994[63]",     "11 July 1996[78]",  "22 November 1996",             "Europe",                                                                                                                           "Was a member of the rival Comecon from 1949 to 1991.",
        "Portugal",                      NA,                 NA,                     NA,     "4 August 1961",             "Europe",                                                                                                                                                                "OEEC member.[6]",
        "Slovakia",     "February 1994[79]",  "8 June 1994[63]",        "July 2000[79]",  "14 December 2000",             "Europe",                                                                                                 "Was a member of the rival Comecon from 1949 to 1991 as part of Czechoslovakia.",
        "Slovenia",        "March 1996[80]",  "16 May 2007[26]",      "10 May 2010[64]",      "21 July 2010",             "Europe",                                                                                                                                                                               NA,
           "Spain",                      NA,                 NA,                     NA,     "3 August 1961",             "Europe",                                                                                                                                                       "Joined OEEC in 1958.[81]",
          "Sweden",                      NA,                 NA,                     NA, "28 September 1961",             "Europe",                                                                                                                                                                "OEEC member.[6]",
     "Switzerland",                      NA,                 NA,                     NA, "28 September 1961",             "Europe",                                                                                                                                                                "OEEC member.[6]",
          "Turkey",                      NA,                 NA,                     NA,     "2 August 1961",          "West Asia",                                                                                                                                                                "OEEC member.[6]",
  "United Kingdom",                      NA,                 NA,                     NA,        "2 May 1961",             "Europe",                                                                                                                                                                "OEEC member.[6]",
   "United States",                      NA,                 NA,                     NA,     "12 April 1961",      "North America",                                                                                                                                                                               NA
  )

latin_am <- psData::countrycode_data %>% filter(
  region %in% c("Caribbean", "Central America", "South America"))

saveRDS(c(latin_am,OECD$Country), here::here("data", "countries.RDS"))

countries <- readRDS(here::here("data", "countries.RDS"))


data %>%
  filter(year > 2000, year < 2019, country %in% countries) %>% 
  write_csv(here::here("data", "df.csv"))

