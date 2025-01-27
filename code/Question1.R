library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(maps)
library(jsonlite)
library(countrycode)
library(rworldmap)
library(RColorBrewer)
library(writexl)
library(readxl)
library(viridis)
library(text2vec)

data <- fromJSON("vcdb_1-of-1.json")
country_data <- unlist(data$victim$country)
country_data[country_data == "Unknown"] <- NA
country_data <- na.omit(country_data)
country_summary <- as.data.frame(table(country_data))
colnames(country_summary) <- c("Country", "Count")
country_summary <- country_summary %>% arrange(desc(Count))
unique_counts <- unique(country_summary$Count)
num_unique_counts <- length(unique_counts)
country_summary$Country_Full <- countrycode(country_summary$Country,
                                            origin = "iso2c", 
                                            destination = "country.name")

country_summary <- country_summary %>%
  select(-Color) 
unique_counts <- sort(unique(country_summary$Count), decreasing = TRUE)

log_counts <- log1p(unique_counts) 
colors <- colorRampPalette(brewer.pal(9, "YlOrRd"))(length(log_counts)) 
count_to_color <- setNames(colors, log_counts)
country_summary$Color <- count_to_color[as.character(log1p(country_summary$Count))]
map_data <- joinCountryData2Map(country_summary,
                                joinCode = "ISO2",
                                nameJoinColumn = "Country")

mapCountryData(map_data,
               nameColumnToPlot = "Count",  
               mapTitle = "Heat map of global cybercrime distribution",
               colourPalette = colors,  
               oceanCol = "lightblue", 
               missingCountryCol = "white", 
               catMethod = 'categorical',  
               addLegend = FALSE) 
#############################################################################
#变量分析——security_incident
table(data$security_incident)
#参与者——外部参与者data$actor$external$country
filtered_country <- subset(data, !sapply(actor$external$country, is.null) & actor$external$country != "Unknown")$actor$external$country
cleaned_countries <- unlist(lapply(filtered_country, function(x) x[x != "Unknown"]))
country_freq <- table(cleaned_countries)
country_freq_df <- as.data.frame(country_freq)
colnames(country_freq_df) <- c("Country", "Crime_Count")
country_freq_df <- country_freq_df %>% arrange(desc(Crime_Count))
unique_counts <- unique(country_freq_df$Crime_Count)
country_freq_df$Crime_Count_log <- log(country_freq_df$Crime_Count+1)
country_freq_df$country_Full<-countrycode(country_freq_df$Country,
            origin = "iso2c", 
            destination = "country.name")
country_freq_df$Crime_Count <- as.numeric(country_freq_df$Crime_Count)

colors <- colorRampPalette(brewer.pal(9, "YlOrRd"))(length(unique_counts))
count_to_color <- setNames(colors, unique_counts)

map_data <- joinCountryData2Map(country_freq_df,
                                joinCode = "ISO2",
                                nameJoinColumn = "Country")

mapCountryData(map_data,
               nameColumnToPlot = "Crime_Count", 
               mapTitle = "Heat map of actor_external",
               colourPalette = colors, 
               oceanCol = "lightblue",  
               missingCountryCol = "white",  
               catMethod = 'categorical', 
               addLegend = FALSE) 
###前员工
result <- data %>%
  filter(actor$external$variety == "Former employee") %>%  
  group_by(actor$external$country) %>% 
  summarise(count = n())  

#参与者——内部参与者data$actor$internal$country
result_1 <- subset(data, !sapply(actor$internal$motive, is.null) & actor$internal$motive != "Unknown" & !is.na(actor$internal$motive))$victim$country

result_1 <- unlist(lapply(result_1, function(x) x[x != "Unknown"]))
country_freq <- table(result_1)

###合作者actor$partner$country
partner_country <- subset(data, !sapply(actor$external$country, is.null) & actor$external$country != "Unknown" )$actor$external$country
partner_country <- unlist(lapply(partner_country, function(x) x[x != "Unknown" & x != "Other"]))
country_freq <- table(partner_country)

partner_country_df <- as.data.frame(country_freq)
colnames(partner_country_df) <- c("Country", "Crime_Count")

partner_country_df <- partner_country_df %>% arrange(desc(Crime_Count))
unique_counts <- unique(partner_country_df$Crime_Count)
partner_country_df$Crime_Count_log <- log(partner_country_df$Crime_Count+1)
partner_country_df$country_Full<-countrycode(partner_country_df$Country,
                                          origin = "iso2c", 
                                          destination = "country.name")

partner_country_df$Crime_Count <- as.numeric(partner_country_df$Crime_Count)

colors <- colorRampPalette(brewer.pal(9, "YlOrRd"))(length(unique_counts))  # 生成与唯一犯罪数一样多的颜色

count_to_color <- setNames(colors, unique_counts)
map_data <- joinCountryData2Map(partner_country_df,
                                joinCode = "ISO2",
                                nameJoinColumn = "Country")

mapCountryData(map_data,
               nameColumnToPlot = "Crime_Count", 
               mapTitle = "Heat map of partner_country",
               colourPalette = colors,  
               oceanCol = "lightblue",  
               missingCountryCol = "white",  
               catMethod = 'categorical', 
               addLegend = FALSE) 

check_event_occurrence <- function(threat_data) {
  result <- sapply(threat_data, function(x) !is.null(x) & !is.na(x) & x != "Unknown")
  if (any(unlist(result))) { 
    return(1) 
  } else {
    return(0) 
  }
}

process_threat_data <- function(threat_data) {
  result <- apply(threat_data, 1, check_event_occurrence)
  return(result)
}

malware_result <- process_threat_data(data$action$malware)
hacking_result <- process_threat_data(data$action$hacking)
social_result <- process_threat_data(data$action$social)
misuse_result <- process_threat_data(data$action$misuse)
physical_result <- process_threat_data(data$action$physical)
environmental_result <- process_threat_data(data$action$environmental)

all_results <- data.frame(
  threat_type = rep(c("malware", "hacking", "social", "misuse", "physical", "environmental"), 
                    times = sapply(list(malware_result, hacking_result, social_result, misuse_result, physical_result, environmental_result), length)),
  occurrence = c(malware_result, hacking_result, social_result, misuse_result, physical_result, environmental_result)
)

summary_results <- all_results %>%
  group_by(threat_type) %>%
  summarise(count = sum(occurrence))
ggplot(summary_results, aes(x = threat_type, y = count, fill = threat_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Occurrence of Different Threat Actions", x = "Threat Type", y = "Occurrence Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

data <- data %>%
  filter(!is.na(victim$country) & victim$country != "UNKNOWN" & victim$country != "NULL")
data$victim_country <- sapply(data$victim$country, function(x) first(x))
str(victim_country)
crime_by_country <- data %>%
  mutate(year = timeline$incident$year) %>%  # 使用 'timeline$incident$year' 作为年份
  group_by(victim_country, year) %>%  # 'victim_country' 是国家列
  summarise(crime_count = n(), .groups = 'drop')
top_countries <- crime_by_country %>%
  group_by(victim_country) %>%
  summarise(total_crimes = sum(crime_count), .groups = 'drop') %>%
  arrange(desc(total_crimes)) %>%
  head(5) %>%
  pull(victim_country)
crime_by_country_filtered <- crime_by_country %>%
  filter(victim_country %in% top_countries)
countries <- unique(crime_by_country_filtered$victim_country)
for (country in countries) {
  country_data <- subset(crime_by_country_filtered, victim_country == country)
  p<-ggplot(country_data, aes(x = year, y = crime_count)) +
    geom_line() +
    geom_point() +
    labs(title = paste("Crime Trend for", country),
         x = "Year",
         y = "Crime Count") +
    theme_minimal()
  print(p)
}

gci_data <- read_xlsx("GCIv5.xlsx")  
gci_data$Country <- as.character(gci_data$country)
gci_data$GCI_level<-gci_data$INDEX
gci_data$ISO_A3 <- countrycode(gci_data$country, "country.name", "iso3c")
gci_data <- gci_data %>%
  select(-country, -INDEX) %>%  
  rename(country = ISO_A3)  
gci_data <- na.omit(gci_data)
colors <- brewer.pal(5, "RdYlBu")  
map_data2 <- joinCountryData2Map(gci_data, 
                                joinCode = "ISO3", 
                                nameJoinColumn = "ISO_A3")  
head(map_data2)
mapCountryData(map_data2,
               nameColumnToPlot = "GCI_level", 
               mapTitle = "Global Cybersecurity Index Levels",
               colourPalette = colors, 
               oceanCol = "lightblue", 
               missingCountryCol = "white", 
               addLegend = TRUE,  
               catMethod = "categorical")  

#起诉得分
data_GCI <- read_xlsx("C:/Users/Administrator/Desktop/2024(2).xlsx") 
prosecution_data <- read.csv("C:/Users/Administrator/Desktop/prosecution.csv")
income_data <- read.csv("C:/Users/Administrator/Desktop/GLM数据集/经济条件/收入指数.csv")
# 合并数据框，按 'ISO_Code' 列匹配
prosecution_data <- merge(prosecution_data, income_data[, c("ISO_Code", "income")], by = "ISO_Code", all.x = TRUE)
prosecution_data <- merge(prosecution_data, data_GCI[, c("ISO_Code", "Legal")], by = "ISO_Code", all.x = TRUE)
#缺失值处理
prosecution_data$poverty[is.na(prosecution_data$poverty)] <- 0
prosecution_data$unem[is.na(prosecution_data$unem)] <- 0
prosecution_data$GINI[is.na(prosecution_data$GINI)] <- mean(prosecution_data$GINI, na.rm = TRUE)
prosecution_data$Legal[is.na(prosecution_data$Legal)] <- mean(prosecution_data$Legal, na.rm = TRUE)
prosecution_data$income[is.na(prosecution_data$income)] <- mean(prosecution_data$income, na.rm = TRUE)
prosecution_data$WGI_Legal[is.na(prosecution_data$WGI_Legal)] <- mean(prosecution_data$WGI_Legal, na.rm = TRUE)
prosecution_data <- na.omit(prosecution_data)
prosecution_data_num <- prosecution_data[, -which(names(prosecution_data) == "ISO_Code")]
prosecution_data_clean <- prosecution_data_num[, -which(names(prosecution_data_num) == "poverty")]
prosecution_data_scaled <- as.data.frame(lapply(prosecution_data_clean, function(x) (x - min(x)) / (max(x) - min(x))))
custom_weights <- c(0.175, 0.1, 0.1, 0.325, 0.1, 0.3)  
weights_df <- data.frame(Variable = names(prosecution_data_scaled), Weight = custom_weights)
print(weights_df)
weights_vector <- as.numeric(weights_df$Weight)
prosecution_data_scaled$composite_score <- apply(prosecution_data_scaled, 1, function(x) {
  sum(x * weights_vector, na.rm = TRUE) 
})

summary(prosecution_data_scaled$composite_score)
merged_data <- merge(prosecution_data, data_GCI, by = "ISO_Code")
top_10_countries <- prosecution_data_scaled[order(-prosecution_data_scaled$composite_score), ][1:10, ]
world <- ne_countries(scale = "medium", returnclass = "sf")

world_data_0 <- world %>%
  left_join(merged_data, by = c("iso_a3" = "ISO_Code"))
ggplot(data = world_data_0) +
  geom_sf(aes(fill = composite_score)) +  
  scale_fill_gradient(low = "lightpink", high = "darkred") +
  theme_minimal() +
  labs(title = "Top 10 Countries by Composite Score",
       fill = "Composite Score") 
#TOPSIS模型

prosecution_data_num <- prosecution_data[, -which(names(prosecution_data) == "ISO_Code")]
prosecution_data_clean <- prosecution_data_num[, -which(names(prosecution_data_num) == "poverty")]
prosecution_data_scaled <- as.data.frame(lapply(prosecution_data_clean, function(x) (x - min(x)) / (max(x) - min(x))))
prosecution_data_weighted <- prosecution_data_scaled * weights_vector
PIS <- apply(prosecution_data_weighted, 2, max)
NIS <- apply(prosecution_data_weighted, 2, min)
distance_to_PIS <- apply(prosecution_data_weighted, 1, function(x) sqrt(sum((x - PIS)^2)))
distance_to_NIS <- apply(prosecution_data_weighted, 1, function(x) sqrt(sum((x - NIS)^2)))
relative_closeness <- distance_to_NIS / (distance_to_PIS + distance_to_NIS)
prosecution_data$composite_score_topsis <- relative_closeness
top_10_countries <- prosecution_data[order(-prosecution_data$composite_score_topsis), ][1:20, ]
merged_data <- merge(prosecution_data, data_GCI, by = "ISO_Code")
world <- ne_countries(scale = "medium", returnclass = "sf")
world_data_0 <- world %>%
  left_join(merged_data, by = c("iso_a3" = "ISO_Code"))
ggplot(data = world_data_0) +
  geom_sf(aes(fill = composite_score_topsis)) +
  scale_fill_gradient(low = "lightpink", high = "darkred") + 
  theme_minimal() +
  labs(title = "Countries by Composite Score (TOPSIS)",
       fill = "Composite Score")
#############################################################################、
#报告率得分——TOPSIS模型
report_data <- read.csv("report.csv")
edu_data <- read.csv("教育指数.csv")
report_data <- merge(report_data, edu_data[, c("ISO_Code", "ED")], by = "ISO_Code", all.x = TRUE)
get_median <- function(x) {
  median(x, na.rm = TRUE)  
}

report_data_filled <- report_data
for (col in colnames(report_data_filled)) {
  if (any(is.na(report_data_filled[[col]]))) {  
    median_value <- get_median(report_data_filled[[col]]) 
    report_data_filled[[col]][is.na(report_data_filled[[col]])] <- median_value  # 填补缺失值
  }
}

report_data_filled <- report_data_filled[, -which(names(report_data_filled) == "ISO_Code")]
report_data_filled_scaled <- as.data.frame(lapply(report_data_filled, function(x) (x - min(x)) / (max(x) - min(x))))
n <- nrow(report_data_filled_scaled )
entropy_values <- apply(report_data_filled_scaled , 2, function(x) {
  p_j <- x / sum(x) 
  -sum(p_j * log(p_j + 1e-10)) / log(n)  
})

weights <- (1 - entropy_values) / sum(1 - entropy_values)
print(weights_df)
weights_vector <- as.numeric(weights_df$Weight)
report_data_filled_weighted <- report_data_filled_scaled * weights_vector
PIS <- apply(report_data_filled_weighted, 2, max)
NIS <- apply(report_data_filled_weighted, 2, min)
distance_to_PIS <- apply(report_data_filled_weighted, 1, function(x) sqrt(sum((x - PIS)^2)))
distance_to_NIS <- apply(report_data_filled_weighted, 1, function(x) sqrt(sum((x - NIS)^2)))
relative_closeness <- distance_to_NIS / (distance_to_PIS + distance_to_NIS)
report_data_filled$report_score_topsis <- relative_closeness
report_data_filled$ISO_Code <- report_data$ISO_Code

merged_data <- merge(report_data_filled, data_GCI, by = "ISO_Code")
prosecution_data_scaled$ISO_Code <- prosecution_data$ISO_Code
merged_data <- merge(prosecution_data_scaled, merged_data, by = "ISO_Code")
top_10_countries <- report_data_filled[order(-report_data_filled$report_score_topsis), ][1:10, ]
world <- ne_countries(scale = "medium", returnclass = "sf")
world_data_0 <- world %>%
  left_join(merged_data, by = c("iso_a3" = "ISO_Code"))
ggplot(data = world_data_0) +
  geom_sf(aes(fill = report_score_topsis)) +  
  scale_fill_gradient(low = "lightpink", high = "darkred") +
  theme_minimal() +
  labs(title = "Countries by Composite Score (TOPSIS)",
       fill = "Composite Score")
#综合国家安全得分
data_secure <- read_xlsx("merged_data.xlsx") 
data_for_entropy <- data_secure[, c("prosecution_score", "report_score", "GCI_score")]
data_scaled <- as.data.frame(lapply(data_for_entropy, function(x) (x - min(x)) / (max(x) - min(x))))

n <- nrow(data_scaled)
p_j <- data_scaled / rowSums(data_scaled)  
entropy_values <- apply(p_j, 2, function(x) {
  -sum(x * log(x + 1e-10)) / log(n)  
})

weights <- (1 - entropy_values) / sum(1 - entropy_values)
weights_df <- data.frame(Variable = names(data_scaled), Weight = weights)
print(weights_df)
norm_matrix <- as.data.frame(lapply(data_scaled, function(x) x / sqrt(sum(x^2))))
weighted_matrix <- norm_matrix * weights
ideal_positive <- apply(weighted_matrix, 2, max)
ideal_negative <- apply(weighted_matrix, 2, min)
distance_to_positive <- sqrt(rowSums((weighted_matrix - ideal_positive)^2))
distance_to_negative <- sqrt(rowSums((weighted_matrix - ideal_negative)^2))
topsis_score <- distance_to_negative / (distance_to_positive + distance_to_negative)
data_secure$topsis_score <- topsis_score
data_secure$security_level <- cut(data_secure$topsis_score,
                                  breaks = c(-Inf, quantile(data_secure$topsis_score, probs = 0.33),
                                             quantile(data_secure$topsis_score, probs = 0.66), Inf),
                                  labels = c("Low", "Medium", "High"))

# 聚类分析
data_secure$log_IP<- log(data_secure$IP_COUNT)
cluster_data <- data_secure[, c("log_IP", "topsis_score")]
cluster_data_scaled <- scale(cluster_data)
set.seed(123)
kmeans_result <- kmeans(cluster_data_scaled, centers = 4, nstart = 25)
data_secure$cluster <- as.factor(kmeans_result$cluster)

ggplot(data_secure, aes(x = log_IP, y = topsis_score, color = cluster)) +
  geom_point(size = 3) +
  labs(title = "Clustering of Countries Based on Crime and Security Scores",
       x = "IP Count (Crime Count)",
       y = "TOPSIS Composite Score",
       color = "Cluster") +
  theme_minimal()