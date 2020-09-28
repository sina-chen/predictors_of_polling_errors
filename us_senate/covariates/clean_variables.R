# cleaning of variables ---------------------------------------------------


# change "NULL" to NA for further analysis and trim leading whitespace

df_final %>% 
  mutate(across(where(is.character), trimws, "left")) -> df_final


df_final %>%
  mutate_all(na_if, "NULL") -> df_final




# remove string artefacts by Wikipedia ('[]')
df_final <- as_tibble(lapply(df_final, function(x) {
  gsub("\\[[^][]*]", "", x)
}))


## merging parties with same meaning to fit the scrape data


# democratic party
df_final$dem_candidate <- coalesce(df_final$Democratic, df_final$DFL)

df_final$dem_candidate <- coalesce(df_final$dem_candidate, df_final$`Democratic Farmer-Labor`)

df_final$dem_candidate <- coalesce(df_final$dem_candidate, df_final$`Democratic-Farmer-Labor`)

df_final$dem_candidate <- coalesce(df_final$dem_candidate, df_final$`Democratic-NPL`)


# merge independent candidates (from party names)

df_final$ind_candidate <- coalesce(df_final$Independent, df_final$`Independent American`)

df_final$ind_candidate <- coalesce(df_final$ind_candidate, df_final$Independence)

df_final$ind_candidate <- coalesce(df_final$ind_candidate, df_final$IPD)

df_final$ind_candidate <- coalesce(df_final$ind_candidate, df_final$independent)

df_final$ind_candidate <- coalesce(df_final$ind_candidate, df_final$`American Independent`)

df_final$ind_candidate <- coalesce(df_final$ind_candidate, df_final$`Independent Green`)

df_final$ind_candidate <- coalesce(df_final$ind_candidate, df_final$`Connecticut for Lieberman`)

df_final$ind_candidate <- coalesce(df_final$ind_candidate, df_final$`Alaskan Independence`)

df_final$ind_candidate <- coalesce(df_final$ind_candidate, df_final$IAP)

df_final$ind_candidate <- coalesce(df_final$ind_candidate, df_final$`independent/Write-in`)

df_final$ind_candidate <- coalesce(df_final$ind_candidate, df_final$MIP)


# republican

# Only one case in "Write-in" -> candidate wasn't chosen to be republican nominee
# ran as write-in in election

df_final$rep_candidate <- coalesce(df_final$Republican, df_final$`Write-in`)


# green party does not need merging

df_final$green_candidate <- df_final$Green

# pooling rest of parties in other


other_parties <- list(df_final$`Natural Law`, df_final$Reform, df_final$Marijuana, 
                      df_final$`Marijuana Reform`, df_final$`Marijuana[c]`, df_final$Constitution,
                      df_final$`U.S. Taxpayers`, df_final$`Concerned Citizens`, df_final$Conservative,
                      df_final$`Peace and Freedom`, df_final$Veterans, df_final$politician,
                      df_final$Mountain, df_final$Nebraska, df_final$Tea, df_final$W.F.,
                      df_final$`Constitution Party`, df_final$`UT Justice`, df_final$Country,
                      df_final$`Legal Marijuana Now`, df_final$American) 

df_final$other_candidate <- coalesce(!!!other_parties)

# libertarian party

df_final$lib_candidate <- df_final$Libertarian


# create dummy variable if incumbent runs for re-election

df_final <- df_final %>% 
  rowwise() %>% 
  mutate(incumbency = dem_candidate %in% Senator | rep_candidate %in% Senator |
           lib_candidate %in% Senator | green_candidate %in% Senator |
           ind_candidate %in% Senator | other_candidate %in% Senator) 

# again remove white space

df_final %>% 
  mutate(across(where(is.character), trimws, "left")) -> df_final


# remove elections where more than one candidate from the major parties runs for office

df_final <- df_final[grep("c(", df_final$dem_candidate, invert = TRUE, fixed = TRUE) , ]

df_final <- df_final[grep("c(", df_final$rep_candidate, invert = TRUE, fixed = TRUE) , ]
