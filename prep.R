
# Load data ----

raw_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-21/starbucks.csv',
                            show_col_types = FALSE)


# Clean up data for my purposes ----

starbucks <- raw_data |>
  mutate(category = case_when(str_detect(product_name, 
                                         "Chai|Tea|tea") ~ "Tea",
                              str_detect(product_name,
                                         "Lemonade|Smoothie|Hot Chocolate|Fibre Powder|Refresher|Apple Spice|Crème Frappuccino Blended") ~ "Other",
                              TRUE ~ "Coffee" ), .before = product_name
  ) |> 
  mutate(product_name = str_to_title(product_name)) |> 
  mutate(size = str_to_title(size)) |>
  mutate(whip = case_when(whip == 0 ~ "No", 
                          whip == 1 ~ "Yes"),
         milk = case_when(milk == 0 ~ "None",
                          milk == 1 ~ "Nonfat",
                          milk == 2 ~ "2%",
                          milk == 3 ~ "Soy",
                          milk == 4 ~ "Coconut",
                          milk == 5 ~ "Whole")
  ) |> 
  select(-c(serv_size_m_l, saturated_fat_g, trans_fat_g, sodium_mg, total_carbs_g, fiber_g)) |>
  mutate(ID = row_number(), .before = category) |> # ensure unique ID for {ggiraph}
  rename(Category = category, 
         Drink = product_name,
         Size = size,
         Calories = calories,
         `Milk Type` = milk,
         `Whipped Topping` = whip,
         `Total Fat (g)` = total_fat_g,
         `Cholesterol (mg)` = cholesterol_mg,
         `Sugar (g)` = sugar_g,
         `Caffeine (mg)` = caffeine_mg)
