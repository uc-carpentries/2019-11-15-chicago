# Initial Data Analysis (v1.0 - Arjun B.)
library(tidyverse)

interviews <- read_csv("data/SAFI_clean.csv", na="NULL")

# 1. Selecting subsets of the data
select(interviews, village, no_membrs, years_liv)
filter(interviews, village == "God")

#2. Select + Filter!
final_tbl <- select(filter(interviews, village == "God"), village, no_membrs, years_liv)

final_tbl_v2 <- interviews %>% filter(village == "God") %>% select(no_membrs, years_liv)

# Exercise 2 : subset to members that were members of an irrigation 
# association (memb_assoc) and keep only affect_conflicts, liv_count, and no_meals
interviews %>% filter(memb_assoc=="yes" | village == "God") %>% select(affect_conflicts,liv_count, no_meals)


#3. Mutation
interviews %>% mutate(people_per_room = no_membrs / rooms)

# Filter according to member_assoc
interviews %>% filter(!is.na(memb_assoc)) %>% mutate(people_per_room = no_membrs / rooms)

# Exercise 3 : create a new data frame containing the following:
# only the village column and a new column of the total meals (number of meals * no members)
# only the rows with >20 total meals should be shown

total_meal_frame <- interviews  %>%
  mutate(tot_meal = no_meals * no_membrs) %>%
  filter(tot_meal > 20) %>% 
  select(village, tot_meal)
  
# Group-By Dataset
interviews %>% group_by(village) %>% summarize(mean_no_membrs = mean(no_membrs))

interviews %>% 
  filter(!is.na(memb_assoc)) %>% 
  group_by(village, memb_assoc) %>% 
  summarise(mean_no_membrs = mean(no_membrs), min_no_membrs = min(no_membrs), n=n()) %>% 
  arrange(mean_no_membrs)

interviews %>% arrange(no_membrs)


# Exercise : Find the average, min, and max number of members within each village
# Hint : use group_by + summarise!
interviews %>% 
  group_by(village) %>%
  summarise(mean_no_mem = mean(no_membrs),
            med_no_mem = median(no_membrs),
            min_no_mem = min(no_membrs),
            max_no_mem = max(no_membrs),
            n = n())









