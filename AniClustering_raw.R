library(tidyverse)
library(cluster) #clustering
library(mclust) # gaussian mixture modeling
library(FactoMineR) # PCA
library(factoextra) # extracting clustering data
library(circlize) # sicc plot
library(kableExtra) # tables
library(viridis) # for color-blind palette
library(gtools) # for combinations
library(GGally) # for ggpairs
library(catspec) # crosstabulation
library(scatterplot3d) # 3d scatters
library(wordcloud) # word clouds

blank <- theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  # removes most formatting 
blank_w_border <- theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 
centered <- element_text(hjust = 0.5)
no_legend <- theme(legend.position="none")

raw_shows <- read_csv("~/github/aniclustering/data/anime.csv", progress=FALSE)
raw_ratings <- read_csv("~/github/aniclustering/data/rating.csv", progress=FALSE)

ratings <- raw_ratings %>% mutate(
  user_id = as.integer(user_id),
  anime_id = as.integer(anime_id)
) 
# filter(rating != -1) # eliminate negative ratings bc we're focusing on positive?

shows <- raw_shows %>% mutate(
  anime_id = as.integer(anime_id),
  episodes = parse_integer(episodes), # not all episode durations are known
  type = factor(type),
  genre = strsplit(genre, ", ")
) %>% 
  filter(!is.na(rating)) %>%
  rename(meanRating = rating)


# manually fill in the total episode number for 5 most popular shows w/missing episode num
unknown_ep_num <- problems(shows$episodes)$row
most_pop_ongoing <- shows[unknown_ep_num,] %>% add_column(row=unknown_ep_num) %>% top_n(5, members)
shows[most_pop_ongoing$row,]$episodes <- c(869, 930, 500, 131, 6)

# remove all the missing episode numbers
shows <- shows %>% filter(!is.na(shows$episodes))

raw_shows %>% mutate(episodes = parse_integer(episodes)) %>%
  select(-c(anime_id, genre, name, type)) %>% summary() %>% 
  as.data.frame %>% as_tibble %>% select(-Var1) %>% group_by(Var2) %>%
  mutate(Stat = seq_along(Var2)) %>% spread(key = Stat, value = Freq) %>% t %>% as_tibble %>%
  `colnames<-`(.[1,]) %>% .[-1,] %>% mutate_all(function(x) gsub("^.*?:", "", x, perl = T)) %>%
  `rownames<-`(c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max", "Missing Vals")) %>%
  t %>%
  kable(format = "latex", escape = F, booktabs = T, align='c', linesep = "",
        caption = "Summary of Show Data and Missing Values", longtable=T) %>% gsub("NA", "0", .)


### exploratory
# genre distribution (pi/bar chart)
shows$genre %>% unlist %>% plyr::count() %>% 
  .[-c(8,12),] %>% # remove unsavory stuff - we're keeping this appropriate
  top_n(20, freq) %>% 
  rename(Genre = x, Count = freq) %>%
  ggplot() + geom_histogram(mapping = aes(Genre, Count, fill=Genre), stat="identity") +
  blank +
  guides(fill=FALSE) + # remove legend for fill
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        plot.title = centered) + 
  scale_fill_viridis_d() +
  ggtitle(paste0("Top 20 Most Popular Genres (from ", dim(shows)[1]," shows)"))

# genre association via circlize -- canonical-correlation analysis?
combo <- function(v) { v <- unique(v); if(length(v) > 1) combinations(length(v), 2, v) else NA }
links <- shows$genre %>% sapply(combo) %>%  # generate all pairs per genre list
  do.call(rbind, .) %>% as_tibble() %>%
  transmute(
    from = V1 %>% unlist,
    to   = V2 %>% unlist
  ) %>% 
  filter(!is.na(to) & !is.na(from)) %>% # remove NAs
  plyr::count() %>% 
  top_n(16, freq) # --select top 10 most common pairings-- 16 looks nice tbh

# coloring
set.seed(1966) # for both colors and shuffling the linkages (when they're ordered)
grid.names <- unique(c(links$from, links$to))
grid.colors <- length(grid.names) %>% viridis %>% `names<-`(grid.names)
colors <- dim(links)[1] %>% viridis()
links[sample(nrow(links)),] %>% 
  chordDiagram(scale=T, 
               transparency=0.1, 
               grid.col=grid.colors, col=colors,
               annotationTrack = c("name","grid")) 
title(main="Most Frequent Genre Pairings")

links %>% rename(Genre1 = from, Genre2 = to, Count = freq) %>% arrange(desc(Count)) %>%
  kable(format = "latex", escape = F, booktabs = T, align='c', linesep = "")

paired <- shows %>% 
  mutate(`log10(Num. of Ratings)` = log10(members), `Average Rating` = meanRating) %>% 
  filter(type %in% c("OVA", "Special", "TV", "Movie")) %>%
  select(`Average Rating`, `log10(Num. of Ratings)`, type) %>%
  ggpairs(aes(fill = type), title="Relationship and Distributions between Show Variables") + 
  blank_w_border +
  theme(plot.title = centered) 
# adjust colors
for(i in 1:paired$nrow){ for(j in 1:paired$ncol){paired[i,j] <- paired[i,j] + scale_fill_viridis_d() }}
paired

# binarize user enjoyment data by comparing their average enjoyment to an
pre.pca <- inner_join(shows, ratings, by="anime_id") %>%
  rename(
    user_rating = rating,
    mean_rating = meanRating
  )
user_averages <- pre.pca %>% group_by(user_id) %>% summarize(user_average = mean(user_rating))
pre.pca <- inner_join(pre.pca, user_averages, by="user_id") %>%
  filter(user_rating <= user_average) %>%
  filter(members > 250000)
# minify data
subset <- which(pre.pca$user_id <= 50000)
df <- pre.pca[subset,] # mini dataset for now

# cross tabulate to exchange info btwn user and show
enjoyed_anime <- ctab(factor(df$user_id), factor(df$anime_id)) %>% .$table %>% as.data.frame.matrix %>%
  as_tibble %>% rownames_to_column("user_id") %>% mutate(user_id = as.integer(user_id))
pca_data <- df %>% mutate(
  num_genres = sapply(genre, length),
  type = as.numeric(type)
) %>% 
  inner_join(enjoyed_anime, by="user_id") %>% {. ->> named_data} %>% 
  select(-c(anime_id, name, user_id, genre, user_rating, user_average))

# store names for recovery later
named_data <- named_data %>% select(anime_id, name, genre)

# do PCA and show top eigen values
pca_data <- pca_data[, colSums(abs(pca_data)) != 0] # eliminate empty columns
pca <- prcomp(pca_data, scale=T)
comp <- data.frame(pca$x[,1:3])
res.pca <- pca_data %>% PCA(ncp=3, scale.unit = TRUE, graph=F) # 20% with 3 components
head(res.pca$eig, 5) %>%
  kable(format = "latex", escape = F, booktabs = T, align='c', linesep = "",
        caption = "Principal Component Analysis: Top Eigenvalues", longtable=T)


pca.df <- pca$x[,c(1,3,2)] #%>% as_tibble %>% filter(PC1 > -1 & PC2 > -1 & PC3 > -1)
scatterplot3d(pca.df, color=viridis(1),
              main = "Post-PCA: Three Principal Components, Unclustered",
              xlab = "Principal Component 1",
              ylab = "Principal Component 2",
              zlab = "Principal Component 3")

fviz_nbclust(pca.df, kmeans, method = "silhouette")

k_val = 3
k <- kmeans(pca.df, k_val, nstart=25, iter.max=1000)
# determine optimal cluster number
colors <- viridis(k_val)
scatterplot3d(pca.df, color=colors[k$cluster],
              main = paste0("Post-PCA, Clustered (k = ",k_val,")"),
              xlab = "Principal Component 1",
              ylab = "Principal Component 2",
              zlab = "Principal Component 3")

clustered_shows <- lapply(1:k_val, function(x) named_data[which(k$cluster == x),] %>% unique)
cluster_word_counts <- clustered_shows %>% lapply(function(c) c$genre %>% unlist %>%
                                                    plyr::count() %>% arrange(desc(freq)))
set.seed(1966)
par(mfrow=c(1,2))
invisible(
  cluster_word_counts[c(1,2)] %>% lapply(function(d) 
    wordcloud(words = d$x, freq = d$freq, min.freq = 2,
              max.words=30, random.order=FALSE, rot.per=0.2,
              scale=c(2,0.5), color=viridis(7) %>% rev))
)


set.seed(1996)
show_recs <- clustered_shows[1:2] %>% 
  lapply(function(x) select(x, "anime_id") %>% inner_join(raw_shows) %>% 
           sample_n(3) %>% transmute(Name=name, Genres=genre)) # pls fit
show_recs[[1]] %>% kable(format = "latex", escape = F, booktabs = T, align='l',
                         linesep = "", caption = "Cluster 1 Recommendations", longtable=T)
# show_recs[[2]] %>% kable(format = "latex", escape = F, booktabs = T, align='l',
#                          linesep = "", caption = "Cluster 2 Recommendations")
