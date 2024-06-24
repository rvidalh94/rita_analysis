library("ggplot2") # Grapgh Library
library("plotly")  # Grapgh Library
library("dplyr")   # Data sorting

# Read database from the dir where the file is
datos <- read.csv("BD_RITA_CONSOLIDATED_updated.csv", sep = ",", header = TRUE, encoding = "UTF-8")
# Filter the dataset for the desired year -> 2022
desired_year <- 2022


# Number of publications per year

paleta <- "Greens" #"Blues" "Zissou" "GrandBudapest"

datos_tmp <- datos

# Grouping docs by status and removing duplicates because the authors separation logic
datos_tmp <- datos_tmp %>%
  distinct(doc, .keep_all = TRUE) %>%
  group_by(status_article, year) %>% 
  summarize(pubs = n(), .groups = 'drop')

# Creating bar graph
g <- ggplot(data = datos_tmp, aes(x = year, y = pubs, fill = status_article)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = seq(2008, 2023, by = 1)) +
  theme_classic() +
  xlab("Years") +
  ylab("# Published Manuscripts") +
  ylim(0, 50) +
  scale_fill_brewer(palette = paleta) +
  ggtitle("Publications by year")

ggplotly(g)

datos_tmp <- datos

datos_year <- datos_tmp %>%
  filter(year == desired_year)

# Counting the number of publications per author
publications_per_author <- datos_year %>%
  group_by(authors) %>%
  summarize(publications = n())

publications_per_author

# Filter the data for the desired year
data_year <- datos %>%
  filter(year == desired_year)

# Grouping data by year and calculating the number of publications and authors
publications_authors_for_year <- data_year %>%
  group_by(year) %>%
  summarize(
    total_publicaciones = n(), # Counting the total number of publications
    total_autores = n_distinct(authors) # Counting the number of unique authors
  )

publications_authors_for_year

# Calculating the avg of total publications to total authors
ratio_publications_to_authors <- publications_authors_for_year$total_publicaciones / publications_authors_for_year$total_autores

result = paste("Promedio publicaciones por autor:", round(ratio_publications_to_authors, 2))
print(result)

datos_tmp <- datos

datos_year <- datos_tmp %>%
  filter(year == desired_year)

# Counting the number of publications per author
publications_per_institution <- datos_year %>%
  group_by(institution) %>%
  distinct(doc, .keep_all = TRUE) %>%
  summarize(publications = n())

publications_per_institution

# Calculating the average number of publications per author
average_publications_per_institution <- mean(publications_per_institution$publications)

datos_tmp <- datos

datos_year <- datos_tmp %>%
  filter(year == desired_year)

# Counting the number of publications per author
publications_per_country <- datos_year %>%
  group_by(country) %>%
  distinct(doc, .keep_all = TRUE) %>%
  summarize(publications = n())

publications_per_country

# Calculating the average number of publications per author
average_publications_per_country <- mean(publications_per_country$publications)

datos_tmp <- datos

datos_year <- datos_tmp %>%
  filter(year == desired_year)

# Group the data by publication ID and count the number of rows for each publication
publication_counts <- datos_year %>%
  group_by(doc) %>%
  summarize(num_authors = n())

# Count the number of publications with more than one author
total_coauthored_publications <- sum(publication_counts$num_authors > 1)

# Total publicataions
total_pubs <- datos_year %>%
  summarize(total_publications = n_distinct(doc))
print(paste("Total de publicaciones:", total_pubs))

# Total number of co-authored publication
print(paste("Total de publicaciones con coautoría:", total_coauthored_publications))

# Average of co-authored publications
avg_coauthored = (total_coauthored_publications/total_pubs) * 100
print(paste("Promedio de publicaciones con coautoría:", round(avg_coauthored, 2)))

datos_tmp <- datos

datos_year <- datos_tmp %>%
  filter(year == desired_year)

# Group the data by publication ID and count the number of rows for each publication
publication_counts <- datos_year %>%
  group_by(doc) %>%
  summarize(num_institution = n_distinct(institution), .groups = 'drop')

# Count the number of publications with more than one author
total_co_institution_publications <- sum(publication_counts$num_institution > 1)

# Total publicataions
total_pubs <- datos_year %>%
  summarize(total_publications = n_distinct(doc))
print(paste("Total de publicaciones:", total_pubs))

# Total number of co-institution publication
print(paste("Total de publicaciones con coautoría institucional:", total_co_institution_publications))

# Average of co-institution publications
avg_coauthored = (total_co_institution_publications/total_pubs) * 100
print(paste("Promedio de publicaciones con coautoría institucional:", round(avg_coauthored, 2)))
