library("ggplot2") # Grapgh Library
library("plotly")  # Grapgh Library
library("dplyr")   # Data sorting

# Read database from the dir where the file is
datos <- read.csv("BD_RITA05 - BD_RITA05.csv", sep = ",", header = TRUE, encoding = "UTF-8")
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

# Splitting authors column and unnesting to create one row per author
authors_split <- datos_year %>%
  mutate(authors = strsplit(as.character(authors), ", ")) %>%
  tidyr::unnest(authors) %>%
  mutate(authors = trimws(authors))  # Remove extra whitespaces

# Counting the number of publications per author
publications_per_author <- authors_split %>%
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
    total_publicaciones = n(),              # Counting the total number of publications
    total_autores = n_distinct(authors)  # Counting the number of unique authors
  )

publications_authors_for_year

# Calculating the ratio of total publications to total authors
ratio_publications_to_authors <- publications_authors_for_year$total_publicaciones / publications_authors_for_year$total_autores

result = paste("Promedio publicaciones por autor:", round(ratio_publications_to_authors, 2))
print(result)

datos_tmp <- datos

datos_year <- datos_tmp %>%
  filter(year == desired_year)

# Splitting authors column and unnesting to create one row per author
institution_split <- datos_year %>%
  mutate(institution = strsplit(as.character(institution), ", ")) %>%
  tidyr::unnest(institution)

# Counting the number of publications per author
publications_per_institution <- institution_split %>%
  distinct(doc, .keep_all = TRUE) %>%
  group_by(institution) %>%
  summarize(publications = n())

publications_per_institution

# Calculating the average number of publications per author
average_publications_per_institution <- mean(publications_per_institution$publications)


