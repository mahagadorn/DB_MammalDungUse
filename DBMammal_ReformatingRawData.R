#raw data from DLP
  #it was in a modified format suitable for R import: DungBeetleData_Rready.csv
  #this raw file contains all species and counts, including some spaces with zeros and others empty.

#Goals:
  #import this raw file and make variable names/entries and zeros (not NAs) consistent
  #this version will be ready for converting to long table format
  #long table version will be used for data analysis

library(dplyr)
library(tidyr)

#setwd(XXXXX)

#import the data
data0 <- read.csv("SupplementalFile_1.csv", header=T, sep=",")
data0 <- data0[,1:18]

#replace all NAs with 0s
#Here, a blank space means no individuals were counted; equivalent to zero
data <- data0 %>% 
  mutate(across(5:18, ~replace_na(., 0)))


#see if there are any species with < 25 individuals
species_sums <- data %>%
  select(5:18) %>%  # or select(species count columns)
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "Species", values_to = "Total")

sum(species_sums[,2])

twentyfive <- 25


#keep species with at least 25 individuals collected over the course of the experiment
species_to_keep <- species_sums %>%
  filter(Total >= twentyfive) %>%
  pull(Species)



# Filter if you only want those with less than 25 ind
data_rarefiltered <- data %>%
  select(1:4, all_of(species_to_keep))  #keep the first 4 columns and only at least 25

write.csv2(data_rarefiltered, "DB_totalabunancedata_shortformat.csv")



#Now, I want to reformat the data so it's in table long format
data_longformat <- data_rarefiltered %>%
  pivot_longer(
    cols = -c(1:4),  # whatever your metadata columns are
    names_to = "Species",
    values_to = "Abundance"
  )

#rename columns
colnames(data_longformat)[3] <- "Month"

#Split the first column into two columns
data_refined <- data_longformat %>%
  separate(col = 1, into = c("Location", "Habitat"), sep = c(1, 2), remove = FALSE)


data_refined <- data_refined %>%
  mutate(
    Location = recode(Location,
                   "N" = "Nassawango",
                   "J" = "Johnson"),
    Habitat = recode(Habitat,
                      "O" = "open",
                      "F" = "forest"),
    Type = recode(Type,
                     "Herbivore " = "Herbivore"),
    Animal = recode(Animal,
                  "Lynx " = "Lynx"),
    Animal = recode(Animal,
                    "Lynx" = "Bobcat"),
    Animal = recode(Animal,
                    "Bear" = "Bear/Dog")
  )



write.csv2(data_refined, "SupplementalFile_2.csv")
