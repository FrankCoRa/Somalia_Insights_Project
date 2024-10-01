# Somalia_Insights_Project
Somalia IDP Insights: Data-Driven Visualizations and Analysis

I Led a team to conduct data mining and visualization using the NGO’s consultancy database to analyze internal displacement patterns in Somalia for the period 2022-2023. 

Our goal was to uncover key insights that would inform humanitarian response strategies and policy-making. While we were unable to share the raw data due to confidentiality restrictions, I’m excited to present the most significant findings from our analysis, illustrated through visualizations created in R Studio by my team and me. These insights offer valuable perspectives on the challenges faced by internally displaced persons (IDPs) in Somalia.
## Infant Breastfeed (R Studio)
```r
# Required Libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(scales)
# Cleaned and Filtered Data Structure for children under 2 y.o.
data_breastfeed <- data.frame(
  Question = c("Exclusively breastfed under 2%", 
               "Exclusively breastfed under 2%",
               "Ever breastfed under 2%",
               "Ever breastfed under 2%",
               "Ever breastfed under 2%",
               "Ever breastfed under 2%",
               "% Ate grains yesterday under 2%",
               "% Ate grains yesterday under 2%",
               "% Ate grains yesterday under 2%",
               "% Ate grains yesterday under 2%",
               "% Ate fruits yesterday under 2%",
               "% Ate fruits yesterday under 2%",
               "% Ate fruits yesterday under 2%",
               "% Been fed infant formula under 2%"),
  Indicator = c("Exclusively breastfed", "Not exclusively breastfed", "Don't know", "No", 
                "Prefer not to answer", "Yes", "Don't know", "No", "Prefer not to answer", 
                "Yes", "Don't know", "No", "Yes", "N/A"),
  Average = c(0.0476, 0.9524, 0.0055, 0.0852, 0.0008, 0.9086, 0.0039, 0.5328, 0.0008, 
              0.4625, 0.0031, 0.9250, 0.0719, 0.6102),
  Host_Community = c(0.0518, 0.9482, 0.0079, 0.0735, 0.0013, 0.9173, 0.0066, 0.5394, 0, 
                     0.4541, 0.0039, 0.9226, 0.0735, 0.6076),
  Protracted_IDPs = c(0.0314, 0.9686, 0, 0.0909, 0, 0.9091, 0, 0.5227, 0.0028, 0.4744, 
                      0, 0.9261, 0.0739, 0.5795),
  New_IDPs = c(0.0634, 0.9366, 0.0060, 0.1265, 0, 0.8675, 0, 0.5241, 0, 0.4759, 
               0.0060, 0.9337, 0.0602, 0.6867)
)

# Convert data to long format for easier plotting
data_long <- data_breastfeed %>%
  pivot_longer(cols = c(Average, Host_Community, Protracted_IDPs, New_IDPs),
               names_to = "Group", values_to = "Value")

# Create the grouped bar chart with switched axes
ggplot(data_long, aes(x = Value, y = Indicator, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Question, scales = "free_y", nrow = 3) +
  geom_text(aes(label = scales::percent(Value, accuracy = 0.01)), 
            position = position_dodge(width = 0.9), 
            hjust = -0.1, 
            size = 3) +  # Adjust label size for better visibility
  labs(title = "Somalia Infant Feeding and Nutritional Status of Children Under 2 Years",
       x = "Percentage", y = "Indicator") +
  theme(axis.text.y = element_text(angle = 0), 
        axis.title.x = element_text(vjust = -0.5))  # Adjust x-axis title position
```
![Alt text](https://github.com/FrankCoRa/Somalia_Insights_Project/blob/main/Breastfeed_Plot.png)
- 7% of infants under 2 years consume fruits, likely those closer to the age of 2, while most infants understandably do not eat fruits due to their young age.
- 46% of infants under 2 consume grains, and 61% are fed infant formula, likely to compensate for a lack of breast milk or maternal challenges.
- 8% of infants have never been breastfed, a concerning statistic as maternal milk is crucial for infant survival and development.
- Only 5% of infants are exclusively breastfed, highlighting the need to address breastfeeding practices and improve maternal nutrition to support milk production.

These findings suggest a need to focus on improving maternal health and nutrition to enhance breastfeeding rates and overall infant health.
## Reporting Needs (R Studio)
```r
# Create the data frame
data_needs <- tibble(
  Indicator = c("None - no priority needs", "Food (or cash to buy food)", "Drinking water", 
                "Shelter / housing", "Healthcare", "Hygiene NFIs and sanitation services",
                "Nutrition services", "Livelihoods support", "Seeds or agricultural inputs",
                "Need to repay debt", "Education", "Infrastructure"),
  Average = c(9, 67, 47, 42, 42, 12, 6, 8, 1, 5, 6, 1),
  Host_Community = c(9, 65, 42, 37, 47, 12, 7, 8, 2, 6, 7, 2),
  Protracted_IDPs = c(5, 70, 54, 51, 38, 12, 7, 7, 1, 4, 5, 1),
  New_IDPs = c(12, 68, 52, 45, 34, 11, 6, 6, 0, 2, 2, 0),
  No = c(8, 66, 47, 41, 45, 13, 7, 8, 2, 4, 6, 1)
)

# Reshape the data for ggplot
data_long <- data_needs %>%
  pivot_longer(cols = -Indicator, names_to = "Category", values_to = "Percentage")

# Create the plot
ggplot(data_long, aes(x = Percentage, y = reorder(Indicator, Percentage), fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(labels = scales::percent_format(scale = 1), 
                     limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(title = "% of Households Reporting Top 3 Priority Needs by Type of Need",
       x = "Percentage",
       y = NULL,
       fill = "Category") +
  theme_minimal() +
  theme(legend.position = "right",
        axis.text.y = element_text(hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  guides(fill = guide_legend(reverse = TRUE))
```
![Alt text](https://github.com/FrankCoRa/Somalia_Insights_Project/blob/main/Priority_Needs.png)
Top Needs in IDP Communities:

- Food Source: Identified by 70% of the population as the most urgent need.
- Water Source: Cited by 48% of respondents as a critical requirement.
- Shelter: 42% reported lacking adequate places to stay.
- Healthcare: 42% identified healthcare access as a major concern.

Focus Areas: These are likely the primary focus areas for the NGO, but challenges remain unresolved, highlighting ongoing humanitarian gaps.
## Education Barriers (R Studio)
```r
# Create the data frame
data_education <- data.frame(
  Indicator = c(
    "Security concerns", "Risk of recruitment", "Risk of violence",
    "Verbal bullying", "Physical bullying", "Physical punishment",
    "Lack of staff", "Discrimination", "Unsafe infrastructure",
    "Lack of hygiene", "Other", "Don't know", "Prefer not to answer"
  ),
  Average = c(18, 11, 7, 6, 10, 4, 22, 4, 14, 7, 0, 13, 4),
  Host_Community = c(16, 10, 2, 2, 8, 6, 31, 4, 12, 10, 0, 16, 2),
  Protracted_IDPs = c(20, 12, 20, 12, 16, 0, 4, 4, 24, 0, 0, 8, 8),
  New_IDPs = c(29, 14, 0, 14, 0, 0, 14, 0, 0, 14, 0, 14, 0)
)

# Reshape the data from wide to long format
data_long <- data_education %>%
  pivot_longer(cols = -Indicator,
               names_to = "Group",
               values_to = "Count") %>%
  group_by(Group) %>%
  mutate(Percentage = Count / sum(Count))

# Create the plot
ggplot(data_long, aes(x = Indicator, y = Percentage, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 0.35)) +
  scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  labs(title = "Barriers faced by school-aged children in safe learning conditions",
       x = NULL,
       y = "Percentage",
       fill = "Group") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.y = element_text(size = 8),
    legend.position = "bottom",
    panel.grid.major.y = element_blank()
  )
```
![Alt text](https://github.com/FrankCoRa/Somalia_Insights_Project/blob/main/Education_barriers.png)
Key Barriers to Education for Somalia IDPs:

- Lack of Staff (19%): The most significant barrier impacting access to education.
- Security Concerns (15%): Safety issues pose a major challenge to educational access.
- Unsafe Infrastructure (12%): Poor learning environments hinder education for IDPs.
- Critical Implications:

These barriers are crucial factors influencing the future development of Somalia’s internally displaced population.
Addressing these challenges is essential for improving educational outcomes and long-term societal stability for Somalia’s IDPs.
## Health Barriers (R Studio)
```r
# Create the data frame
data_health <- data.frame(
  Answer = c(
    "No functional health facility nearby", 
    "No information about health facilities", 
    "Specific medicine unavailable", 
    "Long waiting time", 
    "Could not afford consultation", 
    "Could not afford treatment/medicines", 
    "Could not afford transportation", 
    "Health facility too far", 
    "No means of transport", 
    "Disability prevents access", 
    "Insecurity at health facility", 
    "Did not receive correct medications", 
    "Not enough qualified staff", 
    "Lack of female staff", 
    "Wait and see if problem improves", 
    "Minority clan affiliation issues", 
    "Family discouragement"
  ),
  Average = c(31, 4, 10, 2, 6, 32, 3, 8, 1, 0, 0, 1, 1, 0, 0, 0, 0)
)

# Filter out zero values for better visualization
data_filtered <- data_health %>% filter(Average > 0)

# Calculate percentages
data_filtered <- data_filtered %>%
  mutate(Percentage = round(Average / sum(Average) * 100, 1))  # Calculate percentages

# Create the pie chart with labels
ggplot(data_filtered, aes(x = "", y = Average, fill = Answer)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Most Significant Barriers to Accessing Health Care",
       fill = "Barrier") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  ) +
  geom_text(aes(label = paste0(Percentage, "%")), 
            position = position_stack(vjust = 0.5), 
            size = 4, color = "darkgreen") +  # Add percentage labels
  scale_fill_brewer(palette = "Set3")
```
![Alt text](https://github.com/FrankCoRa/Somalia_Insights_Project/blob/main/Health_Barriers.png)
Impactful points for the health-related findings
- 32.3% of respondents reported concern over insufficient qualified staff, highlighting a critical gap in healthcare provision.
- 31.3% of respondents noted the absence of functional health facilities nearby, significantly limiting access to care.
- 10.1% of respondents cited unavailability of specific medicines, further complicating health outcomes.
- 6.1% of respondents indicated they could not afford consultations, despite the NGO’s healthcare services being advertised as free, signaling potential barriers in accessibility.

The findings underscore urgent health concerns among the IDP population, revealing significant challenges in accessing affordable and adequate healthcare, which is crucial for the well-being of displaced individuals.
## Water Access (R Studio)
```r
# Create the data frame
data_water <- data.frame(
  Question = c(
    "No problems related to access to water",
    "Waterpoints are too far",
    "People with disabilities cannot reach/access waterpoints",
    "Safety concerns at main water points",
    "Safety concerns traveling to main water points",
    "Some groups do not have access to the waterpoints",
    "Insufficient number of water points / long waiting time",
    "Water points are not functioning or closed",
    "Water is not available at the market",
    "Water is too expensive",
    "Not enough containers to store the water",
    "Don’t like taste / quality of water"
  ),
  Average = c(44, 26, 12, 4, 2, 3, 11, 3, 3, 14, 14, 5),
  Host_Community = c(51, 21, 10, 2, 1, 2, 7, 2, 3, 12, 11, 6),
  Protracted_IDPs = c(32, 33, 16, 6, 3, 4, 16, 4, 4, 19, 19, 4),
  New_IDPs = c(38, 31, 16, 6, 3, 4, 15,5 ,4 ,14 ,15 ,3)
)

# Reshape the data for plotting
data_long <- data_water %>%
  pivot_longer(cols = -Question,
               names_to = "Group",
               values_to = "Percentage")

# Create the bubble chart
ggplot(data_long, aes(x = Group, y = Question)) +
  geom_point(aes(size = Percentage, color = Percentage), alpha = .7) +
  scale_size(range = c(5, 20), name = "Percentage") +  # Smaller to larger size range
  scale_color_gradient(low = "lightblue", high = "darkblue", name = "Percentage") +  # Color intensity mapped to percentage
  labs(title = "Problems Related to Accessing Water by Group",
       x = "Group", y = "Problem") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```
![Alt text](https://github.com/FrankCoRa/Somalia_Insights_Project/blob/main/Water_access.png)
Impactful points for the water access-related findings
- 50% of the population reported no issues with accessing water, indicating a positive overall trend.
- 30% of respondents highlighted that waterpoints are too far, presenting a significant accessibility concern.
- 15% of the population expressed that water is too expensive, a worrisome finding given that water is expected to be a free resource for IDPs.

This issue of water cost should be further investigated to ensure equitable access for all internally displaced persons (IDPs).
Overall, these results show improvement compared to previous findings, but specific areas still require attention.

## Thank you!
We hope that this Non-Profit Insight-Visualization Project proves valuable to you. The findings align with previous insights, offering a comprehensive look at the ongoing challenges faced by internally displaced persons (IDPs) in Somalia.
Your feedback is highly appreciated, and if you’re interested in learning more about this NGO's cause or wish to support their efforts, please visit Somalia Humanitarian Aid at https://civil-protection-humanitarian-aid.ec.europa.eu/where/africa/somalia_en.

Thank you for taking the time to review this project, and we look forward to any thoughts or suggestions you may have. Together, we can make a meaningful difference in humanitarian efforts.
