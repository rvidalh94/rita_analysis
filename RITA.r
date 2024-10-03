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

data <- datos_tmp %>%
  filter(year == desired_year) %>%
  filter(status_article == "Published")

# Counting the number of publications per author
publications_per_author <- data %>%
  group_by(authors) %>%
  summarize(publications = n())

publications_per_author



# Filter the data for the desired year
data <- datos_tmp %>%
  filter(year == desired_year) %>%
  filter(status_article == "Published")

# Grouping data by year and calculating the number of publications and authors
publications_authors_for_year <- data %>%
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

data <- datos_tmp %>%
  filter(year == desired_year) %>%
  filter(status_article == "Published")

# Counting the number of publications per author
publications_per_institution <- data %>%
  group_by(institution) %>%
  distinct(doc, .keep_all = TRUE) %>%
  summarize(publications = n())

publications_per_institution

# Calculating the average number of publications per author
average_publications_per_institution <- mean(publications_per_institution$publications)


datos_tmp <- datos

data <- datos_tmp %>%
  filter(year == desired_year) %>%
  filter(status_article == "Published")

# Counting the number of publications per author
publications_per_country <- data %>%
  group_by(country) %>%
  distinct(doc, .keep_all = TRUE) %>%
  summarize(publications = n())

publications_per_country

# Calculating the average number of publications per author
average_publications_per_country <- mean(publications_per_country$publications)


datos_tmp <- datos

data <- datos_tmp %>%
  filter(year == desired_year) %>%
  filter(status_article == "Published")

# Group the data by publication ID and count the number of rows for each publication
publication_counts <- data %>%
  group_by(doc) %>%
  summarize(num_authors = n())

# Count the number of publications with more than one author
total_coauthored_publications <- sum(publication_counts$num_authors > 1)

# Total publicataions
total_pubs <- data %>%
  summarize(total_publications = n_distinct(doc))
print(paste("Total de publicaciones:", total_pubs))

# Total number of co-authored publication
print(paste("Total de publicaciones con coautoría:", total_coauthored_publications))

# Average of co-authored publications
avg_coauthored = (total_coauthored_publications/total_pubs) * 100
print(paste("Promedio de publicaciones con coautoría:", round(avg_coauthored, 2)))


datos_tmp <- datos

data <- datos_tmp %>%
  filter(year == desired_year) %>%
  filter(status_article == "Published")

# Group the data by publication ID and count the number of rows for each publication
publication_counts <- data %>%
  group_by(doc) %>%
  summarize(num_institution = n_distinct(institution), .groups = 'drop')

# Count the number of publications with more than one author
total_co_institution_publications <- sum(publication_counts$num_institution > 1)

# Total publicataions
total_pubs <- data %>%
  summarize(total_publications = n_distinct(doc))
print(paste("Total de publicaciones:", total_pubs))

# Total number of co-institution publication
print(paste("Total de publicaciones con coautoría institucional:", total_co_institution_publications))

# Average of co-institution publications
avg_coauthored = (total_co_institution_publications/total_pubs) * 100
print(paste("Promedio de publicaciones con coautoría institucional:", round(avg_coauthored, 2)))

# Load necessary libraries
library(dplyr)
library(igraph)
library(networkD3)

# Sample dataset
# Replace this with your actual dataset loading step
# Assuming your data is loaded into a dataframe called 'datos'
# datos <- read.csv("path/to/your/dataset.csv")

# Filter dataset for the desired year and status
desired_year <- 2022
datos <- datos %>%
  filter(year == desired_year, status_article == "Published")

# Get unique documents and authors
documentos <- unique(datos$doc)
autores <- sort(unique(datos$authors))

# Create an adjacency matrix for authors
matriz_autores <- matrix(0, ncol = length(autores), nrow = length(autores))
rownames(matriz_autores) <- colnames(matriz_autores) <- autores

# Fill the adjacency matrix
for (doc in documentos) {
  idx <- which(datos$doc == doc)
  authors_in_doc <- datos$authors[idx]
  for (i in seq_along(authors_in_doc)) {
    for (j in seq_along(authors_in_doc)) {
      if (i != j) {
        matriz_autores[authors_in_doc[i], authors_in_doc[j]] <- matriz_autores[authors_in_doc[i], authors_in_doc[j]] + 1
      }
    }
  }
}

# Convert the adjacency matrix to an igraph object
g1 <- graph_from_adjacency_matrix(matriz_autores, weighted = TRUE, mode = "undirected", diag = FALSE)

# Get edges and nodes from the igraph object
edges <- get.data.frame(g1, what = "edges")
nodes <- data.frame(name = V(g1)$name, stringsAsFactors = FALSE)

# Create an index for the nodes
nodes$id <- 0:(nrow(nodes) - 1)

# Map the node names to their indices in the edges data frame
edges$source <- match(edges$from, nodes$name) - 1
edges$target <- match(edges$to, nodes$name) - 1

# Create a 'group' column in nodes for visualization (optional, can be removed or customized)
nodes$group <- 1  # All nodes in the same group for simplicity

# Create the network graph using networkD3
forceNetwork(
  Links = edges,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  NodeID = "name",
  Group = "group",  # Use the 'group' column for node color (optional)
  linkWidth = JS("function(d) { return Math.sqrt(d.value); }"),  # Adjust link width based on weight
  fontSize = 12,
  opacity = 0.9,
  zoom = TRUE,
  linkDistance = 50  # Adjust link distance
)


library(wordcloud2)

# Create a term-frequency table
word_freqs <- table(datos$keywords)

# Convert to data frame
word_freq_df <- data.frame(word = names(word_freqs), freq = as.numeric(word_freqs))

# Plot the word cloud using wordcloud2
wordcloud2(data = word_freq_df)