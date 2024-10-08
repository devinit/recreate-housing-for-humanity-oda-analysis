list.of.packages <- c("rstudioapi","data.table", "dplyr", "writexl", "scales", "ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

setwd(dirname(getActiveDocumentContext()$path))
setwd("../")


# Load data in
official_donors_housing_pc <- read.csv("data/expanded_donors_housing_pc.csv") %>%
  mutate(Amount = case_when(Amount == ".." ~ 0, TRUE ~ as.numeric(as.character(Amount))))

# Specify the order of donors to appear in the graph
donor_order <- c('Multilateral Agencies, Total', 'Non-DAC Countries, Total', 'DAC Countries, Total', 'Official Donors, Total')
official_donors_housing_pc$Donor.s. <- factor(official_donors_housing_pc$Donor.s., levels = donor_order)

# Plot the data as a faceted stacked barplot
ggplot(official_donors_housing_pc %>%
         filter(Time.Period > 2010 & Donor.s. != "Official Donors, Total"),
       aes(x = as.factor(Time.Period), y = Amount, fill = Donor.s.)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Amount of ODA towards the Housing Purpose Codes", x = "Year", y = "Amount (USD Millions)", fill = "Donor Type") +
  facet_wrap(~Sector.s., scales = "fixed") +
  theme(strip.text = element_text(size = 9), plot.title = element_text(size = 9, hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))

# View the purpose code breakdown of the housing search for 2021
Housing_Search_2021 = read.csv("data/housing_search_2021.csv")
Housing2021PC <- Housing_Search_2021 %>%
  group_by(purpose_name) %>%
  count() %>%
  arrange(desc(n)) %>%
  mutate(Year = 2021)
Housing2021PC

# View the purpose code breakdown of the informal settlement search for 2018
Informal2018PC <- Informal_Set_Search_2018 %>%
  group_by(Purpose_Name) %>%
  count() %>%
  arrange(desc(n))
Informal2018PC

# View the purpose code breakdown of the informal settlement search for 2021
Informal2021PC <- Informal_Set_Search_2021 %>%
  group_by(Purpose_Name) %>%
  count() %>%
  arrange(desc(n))
Informal2021PC

# View the purpose code breakdown of the slum search for 2018
Slum2018PC <- Slum_Search_2018 %>%
  group_by(Purpose_Name) %>%
  count() %>%
  arrange(desc(n))
Slum2018PC

# View the purpose code breakdown of the informal settlement search for 2021
Slum2021PC <- Slum_Search_2021 %>%
  group_by(Purpose_Name) %>%
  count() %>%
  arrange(desc(n))
Slum2021PC

# View the purpose code breakdown of the shelter search for 2018
Shelter2018PC <- Shelter_Search_2018 %>%
  group_by(Purpose_Name) %>%
  count() %>%
  arrange(desc(n))
Shelter2018PC

# View the purpose code breakdown of the shelter search for 2021
Shelter2021PC <- Shelter_Search_2021 %>%
  group_by(Purpose_Name) %>%
  count() %>%
  arrange(desc(n))
Shelter2021PC

# Analyzing the combined search data
# View the purpose code breakdown of the combined searches in 2018
combined_PC18 <- combined_searches %>%
  filter(Year == 2018) %>%
  group_by(Purpose_Name) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
combined_PC18

# View the purpose code breakdown of the combined searches in 2021
combined_PC21 <- combined_searches %>%
  filter(Year == 2021) %>%
  group_by(Purpose_Name) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
combined_PC21

# Create a dataset with the counts for each Purpose_Name
complete_breakdown <- combined_searches %>%
  group_by(Purpose_Name) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Get the top 12 frequent Purpose Codes for each year
top_purposes_2018 <- combined_searches %>%
  filter(Year== 2018)%>%
  count(Purpose_Name, sort = TRUE) %>%
  slice_head(n = 10)
top_purposes_2021 <- combined_searches %>%
  filter(Year== 2021)%>%
  count(Purpose_Name, sort = TRUE) %>%
  slice_head(n = 10)
# Plot bar graphs for 2018 Combined Search Purpose Code Distribution
ggplot(top_purposes_2018, aes(x = reorder(str_wrap(Purpose_Name, 10), -n), y = n, fill = Purpose_Name))
geom_bar(stat = "identity") +
  labs(title = "Most Frequent Purpose Codes in 2018 Combined Search",
       x = "Purpose Code",
       y = "Number of Disbursements") +
  theme_minimal(base_size = 9) + # Adjust the size here
  guides(fill = FALSE) # Remove legend

# Plot bar graphs for 2021
ggplot(top_purposes_2021, aes(x = reorder(str_wrap(Purpose_Name, 10), -n), y = n, fill = Purpose_Name))
geom_bar(stat = "identity") +
  labs(title = "Most Frequent Purpose Codes in 2021 Combined Search",
       x = "Purpose Code",
       y = "Number of Disbursements") +
  theme_minimal(base_size = 9) + # Adjust the size here
  guides(fill = FALSE)

# Count unique combinations
count_unique_combinations <- function(data, dataset_name) {
  selected_columns <- c("year", "donor_code", "donor_name", "crs_id", "project_number", "recipient_code", "recipient_name", "project_title", "short_description")
  selected_data <- data[, selected_columns]
  num_unique_combinations <- nrow(distinct(selected_data))
  cat("Number of individual disbursements from", dataset_name, ":", num_unique_combinations, "\n")
  return(num_unique_combinations)
}

# Loop through datasets and count unique combinations
datasets <- list(
  Housing_Search_2018 = Housing_Search_2018,
  Housing_Search_2021 = Housing_Search_2021,
  Informal_Set_Search_2018 = Informal_Set_Search_2018,
  Informal_Set_Search_2021 = Informal_Set_Search_2021,
  Slum_Search_2018 = Slum_Search_2018,
  Slum_Search_2021 = Slum_Search_2021,
  Shelter_Search_2018 = Shelter_Search_2018,
  Shelter_Search_2021 = Shelter_Search_2021,
  Combined_Searches = combined_searches
)

for (dataset_name in names(datasets)) {
  data <- datasets[[dataset_name]]
  result <- count_unique_combinations(data, dataset_name)
}


# Random sampling
combined_housing_search <- bind_rows(mutate(Housing_Search_2018, Year = 2018), mutate(Housing_Search_2021, Year = 2021))
combined_shelter_search <- bind_rows(mutate(Shelter_Search_2018, Year = 2018), mutate(Shelter_Search_2021, Year = 2021))
combined_slum_search <- bind_rows(mutate(Slum_Search_2018, Year = 2018), mutate(Slum_Search_2021, Year = 2021))
combined_informal_search <- bind_rows(mutate(Informal_Set_Search_2018, Year = 2018), mutate(Informal_Set_Search_2021, Year = 2021))

sampled_rows <- dplyr::sample_n(combined_searches, 100)
writexl::write_xlsx(sampled_rows, path = "sampled_combined_rows.xlsx")

#SHELTER SEARCH RELIABILITY CHECK
#Estimating what percentage of the Shelter Searches would have been captured in some level by a housing
#Housing_focus (1- long term housing, 2-short-term emergency shelter relief, 0-none)
# Upload the sheet with this new categorization
shelter_search_sample <- read_excel("/Users/ameliaheymach/Desktop/Habitat/shelter_search_sample.xlsx")
# Estimating what percentage of the Shelter Searches would have been captured in some level by a housing
set.seed(123)

# Function to calculate sample proportions and bootstrap resampling
calculate_sample_proportion <- function(data, indices) {
  sampled_data <- data[indices, ]
  sample_proportion <- mean(sampled_data$Housing_focus)
  return(sample_proportion)
}

# Perform bootstrap resampling for combined searches
bootstrap_results_housing <- boot(data = combined_search_sample, statistic = function(data, indices) mean(resample_cases(data)$longer_term_housing_focus), R = 1000)
bootstrap_results_shelter <- boot(data = combined_search_sample, statistic = function(data, indices) mean(resample_cases(data)$emergency_shelter_focus), R = 1000)
bootstrap_results_informal_set <- boot(data = combined_search_sample, statistic = function(data, indices) mean(resample_cases(data)$informal_set_focus), R = 1000)
