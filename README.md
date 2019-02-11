# AniClustering
## Project Description
Many streaming companies today use data analytics to improve their user experience. One way in which they do this is through show recommendations. These recommendations are based off of your previously viewed shows, with special consideration given to those that you've rated positively. Using Principal Component Analysis (PCA) and clustering, this project explores a dataset of TV shows and their respective ratings from users.  

## Process
It begins by exploring one of the more useful facets of a show: genre. Most shows have multiple genres, and some are more common than others. As well, some are more commonly paired together than others. This tendency for genres to be associated makes the idea of clustering seem more promising. If a user tends to watch shows of the same genres, perhaps we can use their preferences to make recommendations? We'll assign show preference by checking if a user rated a show higher than their average rating. Filtering out "disliked" shows, we can then create a contingency table of users and the shows they liked! A problem with this idea is that there are many unique shows in the dataset (around 12,000), so we'll use PCA to reduce the dimensionality of our data. After PCA, we'll select the top three components (for visualization purpose), and conduct clustering on those data. Done! From there, we can see where (which cluster) new users would fall, and make recommendations accordingly. Included are visuals attesting to the quality of clustering and genre distribution of some clusters.  

## Project Slides 
![](slides/slide1.png?raw=true)
![](slides/slide2.png?raw=true)
![](slides/slide3.png?raw=true)
