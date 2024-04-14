Social Media
================
Satya Shiva Sai Ram Kamma
2024-03-25

``` r
library(readxl)
```

    ## Warning: package 'readxl' was built under R version 4.3.2

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(FactoMineR)
```

    ## Warning: package 'FactoMineR' was built under R version 4.3.3

``` r
library(factoextra)
```

    ## Warning: package 'factoextra' was built under R version 4.3.2

    ## Loading required package: ggplot2

    ## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa

``` r
social_media <- read_excel("social_media_cleaned.xlsx")

social_media_numeric <- select_if(social_media, is.numeric)

# Perform PCA
pca_result <- prcomp(social_media_numeric, scale = TRUE)

# Scree plot
plot(pca_result$sdev^2, type = "b", xlab = "Principal Component", ylab = "Variance Explained")
```

![](Social-Media_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
#From PCA variate representation of each PC, It’s evident that PC1 and PC2 add arround 50% of the to total variance

plot(pca_result$sdev^2, xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree diagram")
```

![](Social-Media_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

``` r
# Loadings
loadings <- pca_result$rotation
print(loadings)
```

    ##                                       PC1          PC2         PC3        PC4
    ## Instagram                      0.35997253 -0.235305731 -0.48074695  0.1123396
    ## LinkedIn                       0.39149535 -0.188256641  0.42154260 -0.2431677
    ## SnapChat                       0.14434470 -0.001375426 -0.53148137 -0.5851768
    ## Twitter                       -0.38928919 -0.485292758  0.14824132 -0.2290724
    ## Whatsapp/Wechat                0.49712209  0.001226222 -0.14994837  0.2121451
    ## youtube                        0.43321201 -0.232497668  0.25452061  0.3341619
    ## OTT                            0.02991209 -0.694285079 -0.10777759 -0.1630966
    ## Reddit                        -0.14824451 -0.346301530  0.09853811  0.2831951
    ## How you felt the entire week?  0.29511949  0.132915416  0.42207257 -0.5194936
    ##                                       PC5         PC6        PC7         PC8
    ## Instagram                      0.13976700 -0.31408369  0.4516641 -0.49586684
    ## LinkedIn                      -0.11932387 -0.28841101 -0.5352279 -0.41219103
    ## SnapChat                      -0.34293942 -0.16571370 -0.1349190  0.39502630
    ## Twitter                        0.31769932  0.03070392  0.1596447  0.04109432
    ## Whatsapp/Wechat                0.03955265  0.70875864 -0.1526895  0.02249668
    ## youtube                        0.10599868 -0.36788501  0.1356451  0.64454084
    ## OTT                            0.13238183  0.29590419 -0.1450540  0.06870679
    ## Reddit                        -0.84276046  0.07699596  0.1923146 -0.07462750
    ## How you felt the entire week? -0.08435896  0.24207624  0.6050635 -0.01585044
    ##                                       PC9
    ## Instagram                     -0.05696856
    ## LinkedIn                      -0.14378008
    ## SnapChat                      -0.18692529
    ## Twitter                       -0.63987847
    ## Whatsapp/Wechat               -0.39705749
    ## youtube                       -0.03767887
    ## OTT                            0.58992497
    ## Reddit                        -0.09727197
    ## How you felt the entire week?  0.12308003

``` r
# Data projection onto all PCs
data_projection_all <- as.data.frame(pca_result$x)

# Matrix scatterplot for pairs of principal components
pairs(data_projection_all, col = "blue", pch = 19,
      main = "Data Visualization using All PCs")
```

![](Social-Media_files/figure-gfm/unnamed-chunk-1-3.png)<!-- -->

``` r
# Visualize Eigenvalues
fviz_eig(pca_result, addlabels = TRUE)
```

![](Social-Media_files/figure-gfm/unnamed-chunk-1-4.png)<!-- -->

``` r
# Visualize Variable Quality
fviz_pca_var(pca_result, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE)
```

![](Social-Media_files/figure-gfm/unnamed-chunk-1-5.png)<!-- -->

``` r
# Visualize Individual Contributions
fviz_pca_ind(pca_result,
             geom.ind = "point", # Show points only
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
             )
```

![](Social-Media_files/figure-gfm/unnamed-chunk-1-6.png)<!-- -->

``` r
# Biplot
biplot(pca_result)
```

![](Social-Media_files/figure-gfm/unnamed-chunk-1-7.png)<!-- -->

``` r
# Variable correlation plot (Correlation Circle)
fviz_pca_var(pca_result, col.var = "black")
```

![](Social-Media_files/figure-gfm/unnamed-chunk-1-8.png)<!-- -->

``` r
# Quality of representation of variables on dimensions 1 and 2
fviz_cos2(pca_result, choice = "var", axes = 1:2)
```

![](Social-Media_files/figure-gfm/unnamed-chunk-1-9.png)<!-- -->

``` r
# Contributions of variables to principal components
fviz_contrib(pca_result, choice = "var", axes = 1, top = 10)
```

![](Social-Media_files/figure-gfm/unnamed-chunk-1-10.png)<!-- -->

``` r
fviz_contrib(pca_result, choice = "var", axes = 2, top = 10)
```

![](Social-Media_files/figure-gfm/unnamed-chunk-1-11.png)<!-- -->

``` r
# Visualize individual contributions
fviz_pca_ind(pca_result,
             geom.ind = "point", # Show points only
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
             )
```

![](Social-Media_files/figure-gfm/unnamed-chunk-1-12.png)<!-- -->

``` r
library(scatterplot3d)
scatterplot3d(pca_result$x[,1:3], color = social_media$Instagram)
```

![](Social-Media_files/figure-gfm/unnamed-chunk-1-13.png)<!-- -->

### Cluster Analysis

``` r
library(readxl)
library(factoextra)
library(ggplot2)
library(factoextra)
library(ggfortify)
```

    ## Warning: package 'ggfortify' was built under R version 4.3.3

``` r
library(MASS)
```

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

``` r
library(ggrepel)
```

    ## Warning: package 'ggrepel' was built under R version 4.3.2

``` r
library(stats)


social_media_cluster <- read_excel("social_media_cleaned.xlsx")
data.scaled <- scale(x = social_media_cluster[, -1], center = TRUE, scale = TRUE)
data <- data.scaled
head(data)
```

    ##       Instagram   LinkedIn   SnapChat     Twitter Whatsapp/Wechat    youtube
    ## [1,] -0.6888867  0.1519097 -0.2810325  3.42451853      -1.3480276 -0.2719612
    ## [2,]  0.5146906  0.6366870  0.4933528 -0.44951033      -0.5576781  0.7352180
    ## [3,] -0.6130708  1.3638528 -0.4157082 -0.44951033       0.8450887 -0.6460563
    ## [4,] -0.1534369  0.6838181 -0.1944552 -0.44951033      -0.2804351 -0.5597267
    ## [5,]  1.7277449 -1.2283589 -0.5696232  0.06702685      -0.8514729  0.3035697
    ## [6,] -1.0205812  1.3638528 -0.4349475 -0.44951033       1.3830228  2.3179281
    ##             OTT     Reddit How you felt the entire week?
    ## [1,]  3.4382238  1.2405563                    -0.5741693
    ## [2,] -0.6686547 -0.3292004                    -0.5741693
    ## [3,] -0.1021887 -0.3292004                     0.7655590
    ## [4,] -0.1021887 -0.3292004                     0.7655590
    ## [5,] -0.1021887  0.2987023                    -0.5741693
    ## [6,]  0.1810443 -0.3292004                     2.1052873

``` r
# Perform PCA
pc <- prcomp(data.scaled)
pc_first_three <- pc$x[, 1:3]
# Perform K-means clustering on the first three principal components
set.seed(123)  # For reproducibility
k <- 3  # Number of clusters
km_clusters <- kmeans(pc_first_three, centers = k)

# Define colors for each cluster
cluster_colors <- c("red", "blue", "green")

# Plot the first three principal components with cluster assignments
plot(pc_first_three, col = cluster_colors[km_clusters$cluster], 
     main = "First Three Principal Components with Cluster Assignments", 
     xlab = "", ylab = "", pch = 20)
```

![](Social-Media_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->
<p>
It is first performs Principal Component Analysis (PCA) on scaled data,
reducing its dimensionality.
</p>
<p>
Then, it extracts the first three principal components. Next, it applies
K-means clustering to these components, dividing data into three
clusters.
</p>
<p>
Finally, it plots the first three principal components with color-coded
cluster assignments for visualization and analysis
</p>

``` r
# Take a subset of 20 rows
data_subset <- data[1:20, ]

# Perform PCA
pca_result <- prcomp(data_subset)

# Extract the first three principal components
pc_first_three <- pca_result$x[, 1:3]

# Perform hierarchical clustering on the first three principal components
hc <- hclust(dist(pc_first_three))

# Plot the dendrogram
plot(hc, main = "Dendrogram of Hierarchical Clustering (Subset of 20 Rows)",
     xlab = "Sample Index", ylab = "Distance", sub = NULL)
```

![](Social-Media_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

<p>
The plot shows the first three principal components, performs
hierarchical clustering on them, and plots a dendrogram showing the
relationships between the samples based on their distances in the
reduced-dimensional space.
</p>

``` r
# Visualize cluster and membership using first two Principal Components
fviz_cluster(list(data = pc$x[, 1:2], cluster = km_clusters$cluster))
```

![](Social-Media_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

<p>
This plot visualizes clustering results by plotting data points in a
two-dimensional space using the first two Principal Components. Each
point is colored according to its assigned cluster, showing the grouping
pattern identified by the clustering algorithm. It helps understand how
data points are grouped based on their features.
</p>

``` r
# Non-hierarchical clustering (k-means)
num_clusters <- 2  
kmeans_model <- kmeans(data, centers = num_clusters)

# Membership for each cluster
table(kmeans_model$cluster)
```

    ## 
    ##  1  2 
    ## 11 10

<p>
This represents clustering using the k-means algorithm, dividing data
into two clusters. It initializes cluster centers randomly, assigning
each data point to the nearest cluster. The table function counts the
number of data points assigned to each cluster, providing insight into
cluster membership and distribution.
</p>
<b>
<p>
This represents clustering using the k-means algorithm, dividing data
into two clusters. It initializes cluster centers randomly, assigning
each data point to the nearest cluster. The table function counts the
number of data points assigned to each cluster, providing insight into
cluster membership and distribution.
</p>

</b>

``` r
# Visualize cluster and membership using first two Principal Components
fviz_cluster(list(data = pc$x[, 1:2], cluster = kmeans_model$cluster))
```

![](Social-Media_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
<p>
This plot visualizes clusters and their memberships using the first two
principal components. It extracts these components from the data, then
assigns each data point to a cluster using k-means clustering. Finally,
it creates a visual representation showing how the data points are
grouped based on their similarities in the first two principal
components.
</p>
<b>
<p>
What is the relationship between the clustering results obtained through
k-means algorithm and the underlying structure of the data as revealed
by Principal Component Analysis (PCA)?
</p>

</b>

``` r
# Visualize cluster and membership using first two Principal Components for k-means
pca_result <- prcomp(data, scale = TRUE)
fviz_cluster(kmeans_model, data = pca_result$x[, 1:2], geom = "point", 
             pointsize = 2, fill = "white", main = "K-means Clustering Result (PCA)")
```

![](Social-Media_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->
<p>
This shows visualization of the clusters and their memberships using the
first two Principal Components (PCs) obtained from the PCA (Principal
Component Analysis) of the numerical data. First, it computes the PCA
result for the numerical data and scales it. Then, it uses the
fviz_cluster function to plot the clusters obtained from the k-means
algorithm (kmeans_model). It represents each data point as a point on
the plot, with the size set to 2 and colored white. The plot is titled
“K-means Clustering Result (PCA)”. This visualization helps to
understand how the data points are grouped into clusters based on their
similarities, as revealed by the PCA analysis.
</p>
<p>
What is the relationship between the number of clusters (k) and the
average silhouette width in k-means clustering, and how does this
relationship inform the determination of the optimal number of clusters
for a given dataset?
</p>

``` r
library(factoextra)
library(cluster)

# Calculate silhouette information for k-means clustering
sil <- silhouette(kmeans_model$cluster, dist(data))

# Visualize the silhouette plot for k-means clustering
fviz_silhouette(sil, main = "Silhouette Plot for K-means Clustering")
```

    ##   cluster size ave.sil.width
    ## 1       1   11          0.26
    ## 2       2   10          0.04

![](Social-Media_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

<p>
This plot calculates and visualizes the silhouette information for
k-means clustering. Silhouette analysis helps evaluate the quality of
clustering by measuring how similar an object is to its own cluster
compared to other clusters. A higher silhouette width indicates better
separation of clusters, while negative values suggest that points might
be assigned to the wrong clusters. This plot helps in determining the
optimal number of clusters for k-means clustering and assessing the
overall clustering performance.
</p>

``` r
# Create a data frame with cluster membership
data_clustered <- data.frame(data, Cluster = kmeans_model$cluster)  # Ensure conversion to data frame

# Scatter plot of data points colored by cluster membership
plot(data_clustered$Whatsapp.Wechat, data_clustered$youtube, 
     col = data_clustered$Cluster, pch = 17, 
     xlab = "Whatsapp", ylab = "Youtube",  
     main = "Scatter Plot of Clustering")
legend("topright", legend = unique(data_clustered$Cluster), 
       col = 1:max(data_clustered$Cluster), pch = 17, title = "Cluster")
```

![](Social-Media_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->
<p>
Overall, this plot visualizes clusters in the data, helping us
understand how data points group together based on the Whatsapp and
Youtube, with each group represented by a different color on the plot.
</p>

### Factor Analysis

``` r
library(ggplot2)
library(psych)
```

    ## Warning: package 'psych' was built under R version 4.3.3

    ## 
    ## Attaching package: 'psych'

    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     %+%, alpha

``` r
fa.parallel(social_media_numeric)
```

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect.  Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected.  Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect.  Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected.  Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect.  Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected.  Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect.  Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected.  Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect.  Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected.  Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect.  Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected.  Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect.  Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect.  Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect.  Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected.  Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect.  Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected.  Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect.  Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected.  Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect.  Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected.  Examine the results carefully

![](Social-Media_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  0  and the number of components =  0

<p>
Parallel analysis suggests that the number of factors = 0 and the number
of components = 0
</p>

``` r
fit.pc <- principal(social_media_numeric, nfactors=2, rotate="varimax")
fit.pc
```

    ## Principal Components Analysis
    ## Call: principal(r = social_media_numeric, nfactors = 2, rotate = "varimax")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##                                 RC1   RC2    h2   u2 com
    ## Instagram                      0.63  0.09 0.400 0.60 1.0
    ## LinkedIn                       0.65  0.01 0.421 0.58 1.0
    ## SnapChat                       0.21 -0.08 0.049 0.95 1.3
    ## Twitter                       -0.32  0.81 0.761 0.24 1.3
    ## Whatsapp/Wechat                0.71 -0.28 0.581 0.42 1.3
    ## youtube                        0.73  0.04 0.534 0.47 1.0
    ## OTT                            0.37  0.83 0.832 0.17 1.4
    ## Reddit                        -0.05  0.51 0.258 0.74 1.0
    ## How you felt the entire week?  0.36 -0.33 0.235 0.77 2.0
    ## 
    ##                        RC1  RC2
    ## SS loadings           2.27 1.80
    ## Proportion Var        0.25 0.20
    ## Cumulative Var        0.25 0.45
    ## Proportion Explained  0.56 0.44
    ## Cumulative Proportion 0.56 1.00
    ## 
    ## Mean item complexity =  1.3
    ## Test of the hypothesis that 2 components are sufficient.
    ## 
    ## The root mean square of the residuals (RMSR) is  0.14 
    ##  with the empirical chi square  29.01  with prob <  0.066 
    ## 
    ## Fit based upon off diagonal values = 0.66

<p>
High absolute values (close to 1) indicate a strong relationship between
the variable and the factor. \#h2 explains how much variance of the
variables are explained by the factors. \#u2 indicates the amount of
variance not explained by the factors Principal Components Analysis
Call: principal(r = social_media_numeric, nfactors = 2, rotate =
“varimax”) Standardized loadings (pattern matrix) based upon correlation
matrix
</p>
<p>
SS loadings 2.27 1.80 Proportion Var 0.25 0.20 Cumulative Var 0.25 0.45
Proportion Explained 0.56 0.44 Cumulative Proportion 0.56 1.00
</p>
<p>
Mean item complexity = 1.3 Test of the hypothesis that 2 components are
sufficient.
</p>
<p>

The root mean square of the residuals (RMSR) is 0.14 with the empirical
chi square 29.01 with prob \< 0.066

</p>

``` r
round(fit.pc$values, 3)
```

    ## [1] 2.349 1.721 1.370 1.093 0.867 0.611 0.497 0.369 0.122

``` r
fit.pc$loadings
```

    ## 
    ## Loadings:
    ##                               RC1    RC2   
    ## Instagram                      0.626       
    ## LinkedIn                       0.649       
    ## SnapChat                       0.207       
    ## Twitter                       -0.324  0.810
    ## Whatsapp/Wechat                0.709 -0.278
    ## youtube                        0.729       
    ## OTT                            0.374  0.832
    ## Reddit                                0.506
    ## How you felt the entire week?  0.358 -0.327
    ## 
    ##                  RC1   RC2
    ## SS loadings    2.266 1.804
    ## Proportion Var 0.252 0.200
    ## Cumulative Var 0.252 0.452

``` r
fit.pc$communality
```

    ##                     Instagram                      LinkedIn 
    ##                    0.39967455                    0.42102084 
    ##                      SnapChat                       Twitter 
    ##                    0.04894532                    0.76130198 
    ##               Whatsapp/Wechat                       youtube 
    ##                    0.58050860                    0.53387209 
    ##                           OTT                        Reddit 
    ##                    0.83170119                    0.25801853 
    ## How you felt the entire week? 
    ##                    0.23499111

``` r
# Rotated factor scores, Notice the columns ordering: RC1, RC2
fit.pc
```

    ## Principal Components Analysis
    ## Call: principal(r = social_media_numeric, nfactors = 2, rotate = "varimax")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##                                 RC1   RC2    h2   u2 com
    ## Instagram                      0.63  0.09 0.400 0.60 1.0
    ## LinkedIn                       0.65  0.01 0.421 0.58 1.0
    ## SnapChat                       0.21 -0.08 0.049 0.95 1.3
    ## Twitter                       -0.32  0.81 0.761 0.24 1.3
    ## Whatsapp/Wechat                0.71 -0.28 0.581 0.42 1.3
    ## youtube                        0.73  0.04 0.534 0.47 1.0
    ## OTT                            0.37  0.83 0.832 0.17 1.4
    ## Reddit                        -0.05  0.51 0.258 0.74 1.0
    ## How you felt the entire week?  0.36 -0.33 0.235 0.77 2.0
    ## 
    ##                        RC1  RC2
    ## SS loadings           2.27 1.80
    ## Proportion Var        0.25 0.20
    ## Cumulative Var        0.25 0.45
    ## Proportion Explained  0.56 0.44
    ## Cumulative Proportion 0.56 1.00
    ## 
    ## Mean item complexity =  1.3
    ## Test of the hypothesis that 2 components are sufficient.
    ## 
    ## The root mean square of the residuals (RMSR) is  0.14 
    ##  with the empirical chi square  29.01  with prob <  0.066 
    ## 
    ## Fit based upon off diagonal values = 0.66

``` r
fit.pc$scores
```

    ##                RC1         RC2
    ##  [1,] -0.372676698  3.71255215
    ##  [2,]  0.268347117 -0.35575841
    ##  [3,]  0.359865328 -0.58570042
    ##  [4,]  0.001187453 -0.44418221
    ##  [5,] -0.181847349  0.38801798
    ##  [6,]  1.626846855 -0.58008812
    ##  [7,] -0.065741826 -0.71382608
    ##  [8,]  0.432476700 -0.44564386
    ##  [9,]  1.187916225  0.08916243
    ## [10,] -1.711078905 -0.31265266
    ## [11,] -1.280365844  0.12706476
    ## [12,] -0.992156374  0.64778326
    ## [13,] -0.565682913 -0.85688685
    ## [14,] -0.604748082  0.02945858
    ## [15,] -0.105177713  0.96127376
    ## [16,]  0.748723180 -0.55650547
    ## [17,]  0.456870010 -0.31905559
    ## [18,] -0.534366898 -0.28003587
    ## [19,]  0.381851729 -0.49687951
    ## [20,]  2.408453160  0.83778421
    ## [21,] -1.458695155 -0.84588208

``` r
fa.plot(fit.pc)
```

![](Social-Media_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
fa.diagram(fit.pc) # Visualize the relationship
```

![](Social-Media_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

``` r
vss(social_media_numeric)
```

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected.  Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect.  Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected.  Examine the results carefully

![](Social-Media_files/figure-gfm/unnamed-chunk-12-3.png)<!-- -->

    ## 
    ## Very Simple Structure
    ## Call: vss(x = social_media_numeric)
    ## VSS complexity 1 achieves a maximimum of 0.61  with  6  factors
    ## VSS complexity 2 achieves a maximimum of 0.78  with  7  factors
    ## 
    ## The Velicer MAP achieves a minimum of 0.06  with  1  factors 
    ## BIC achieves a minimum of  -53.17  with  1  factors
    ## Sample Size adjusted BIC achieves a minimum of  1.47  with  5  factors
    ## 
    ## Statistics by number of factors 
    ##   vss1 vss2   map dof   chisq prob sqresid  fit RMSEA   BIC SABIC complex
    ## 1 0.38 0.00 0.062  27 2.9e+01 0.36     8.1 0.38 0.035 -53.2  30.2     1.0
    ## 2 0.49 0.60 0.080  19 1.4e+01 0.81     5.3 0.60 0.000 -44.2  14.4     1.2
    ## 3 0.55 0.69 0.101  12 7.9e+00 0.79     3.6 0.72 0.000 -28.6   8.4     1.3
    ## 4 0.57 0.75 0.133   6 5.0e+00 0.55     2.5 0.81 0.000 -13.3   5.2     1.6
    ## 5 0.59 0.77 0.199   1 1.4e+00 0.23     1.9 0.86 0.134  -1.6   1.5     1.6
    ## 6 0.61 0.76 0.282  -3 4.5e-08   NA     1.5 0.88    NA    NA    NA     1.7
    ## 7 0.61 0.78 0.439  -6 2.5e-10   NA     1.3 0.90    NA    NA    NA     1.6
    ## 8 0.61 0.78 1.000  -8 0.0e+00   NA     1.3 0.90    NA    NA    NA     1.6
    ##    eChisq    SRMR eCRMS  eBIC
    ## 1 3.9e+01 1.6e-01 0.186 -43.2
    ## 2 1.6e+01 1.0e-01 0.140 -42.2
    ## 3 6.1e+00 6.3e-02 0.110 -30.5
    ## 4 1.9e+00 3.6e-02 0.087 -16.3
    ## 5 5.7e-01 1.9e-02 0.116  -2.5
    ## 6 1.2e-08 2.9e-06    NA    NA
    ## 7 3.0e-11 1.4e-07    NA    NA
    ## 8 3.2e-16 4.6e-10    NA    NA

<p>

Very Simple Structure Call: vss(x = social_media_numeric) VSS complexity
1 achieves a maximimum of 0.61 with 6 factors VSS complexity 2 achieves
a maximimum of 0.78 with 7 factors

The Velicer MAP achieves a minimum of 0.06 with 1 factors BIC achieves a
minimum of -53.17 with 1 factors Sample Size adjusted BIC achieves a
minimum of 1.47 with 5 factors

Statistics by number of factors

</p>

``` r
# Computing Correlation Matrix
corrm.social <- cor(social_media_numeric)
corrm.social
```

    ##                                 Instagram     LinkedIn    SnapChat    Twitter
    ## Instagram                      1.00000000  0.097056399  0.28968877 -0.1930565
    ## LinkedIn                       0.09705640  1.000000000  0.02552545 -0.1300685
    ## SnapChat                       0.28968877  0.025525452  1.00000000 -0.1799569
    ## Twitter                       -0.19305653 -0.130068464 -0.17995686  1.0000000
    ## Whatsapp/Wechat                0.37769615  0.228835623  0.08099980 -0.4958329
    ## youtube                        0.33000187  0.452197669 -0.16006877 -0.1881777
    ## OTT                            0.26738122  0.185492527  0.13158590  0.5570740
    ## Reddit                        -0.07461553 -0.006992884 -0.08116237  0.1649030
    ## How you felt the entire week? -0.06488863  0.415506031  0.07962803 -0.1455712
    ##                               Whatsapp/Wechat     youtube        OTT
    ## Instagram                           0.3776962  0.33000187  0.2673812
    ## LinkedIn                            0.2288356  0.45219767  0.1854925
    ## SnapChat                            0.0809998 -0.16006877  0.1315859
    ## Twitter                            -0.4958329 -0.18817769  0.5570740
    ## Whatsapp/Wechat                     1.0000000  0.37168516  0.1336204
    ## youtube                             0.3716852  1.00000000  0.1605652
    ## OTT                                 0.1336204  0.16056523  1.0000000
    ## Reddit                             -0.1344497  0.02639991  0.2327911
    ## How you felt the entire week?       0.1877267  0.17877463 -0.1088494
    ##                                     Reddit How you felt the entire week?
    ## Instagram                     -0.074615529                   -0.06488863
    ## LinkedIn                      -0.006992884                    0.41550603
    ## SnapChat                      -0.081162369                    0.07962803
    ## Twitter                        0.164902964                   -0.14557124
    ## Whatsapp/Wechat               -0.134449660                    0.18772672
    ## youtube                        0.026399913                    0.17877463
    ## OTT                            0.232791099                   -0.10884945
    ## Reddit                         1.000000000                   -0.15598604
    ## How you felt the entire week? -0.155986035                    1.00000000

``` r
plot(corrm.social)
```

![](Social-Media_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
social_pca <- prcomp(social_media_numeric, scale=TRUE)
summary(social_pca)
```

    ## Importance of components:
    ##                          PC1    PC2    PC3    PC4     PC5     PC6     PC7
    ## Standard deviation     1.533 1.3119 1.1705 1.0456 0.93113 0.78196 0.70503
    ## Proportion of Variance 0.261 0.1912 0.1522 0.1215 0.09633 0.06794 0.05523
    ## Cumulative Proportion  0.261 0.4522 0.6045 0.7259 0.82225 0.89019 0.94542
    ##                            PC8     PC9
    ## Standard deviation     0.60784 0.34892
    ## Proportion of Variance 0.04105 0.01353
    ## Cumulative Proportion  0.98647 1.00000

``` r
plot(social_pca)
```

![](Social-Media_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->

``` r
biplot(fit.pc)
```

![](Social-Media_files/figure-gfm/unnamed-chunk-13-3.png)<!-- -->

<p>
Overall, these techniques complement each other and can be used together
to gain a comprehensive understanding of the data, uncover hidden
patterns and structures, and derive meaningful insights for
decision-making and further analysis
</p>

#### Multiple Regression Analysis

``` r
library(dplyr)
library(readxl)
library(car)
```

    ## Warning: package 'car' was built under R version 4.3.2

    ## Loading required package: carData

    ## Warning: package 'carData' was built under R version 4.3.2

    ## 
    ## Attaching package: 'car'

    ## The following object is masked from 'package:psych':
    ## 
    ##     logit

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     recode

``` r
library(ggplot2)

social_media <- read_excel("social_media_cleaned.xlsx")

social_media_numeric <- select_if(social_media, is.numeric)

model <- lm( `How you felt the entire week?` ~ Instagram + LinkedIn + SnapChat + Twitter + `Whatsapp/Wechat` + youtube + OTT + Reddit,
  data = social_media_numeric
)

summary(model)
```

    ## 
    ## Call:
    ## lm(formula = `How you felt the entire week?` ~ Instagram + LinkedIn + 
    ##     SnapChat + Twitter + `Whatsapp/Wechat` + youtube + OTT + 
    ##     Reddit, data = social_media_numeric)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.14225 -0.32574 -0.04625  0.48405  1.49274 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)        2.72840    0.67742   4.028  0.00168 **
    ## Instagram         -0.03070    0.06417  -0.478  0.64091   
    ## LinkedIn           0.13054    0.08635   1.512  0.15647   
    ## SnapChat           0.04185    0.06246   0.670  0.51553   
    ## Twitter            0.16850    0.27181   0.620  0.54692   
    ## `Whatsapp/Wechat`  0.05279    0.06756   0.781  0.44969   
    ## youtube            0.03106    0.13556   0.229  0.82263   
    ## OTT               -0.07842    0.09259  -0.847  0.41357   
    ## Reddit            -0.03435    0.12297  -0.279  0.78475   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.8174 on 12 degrees of freedom
    ## Multiple R-squared:  0.2805, Adjusted R-squared:  -0.1991 
    ## F-statistic: 0.5849 on 8 and 12 DF,  p-value: 0.7729

``` r
vif(model)
```

    ##         Instagram          LinkedIn          SnapChat           Twitter 
    ##          1.524923          1.367600          1.402359          3.684365 
    ## `Whatsapp/Wechat`           youtube               OTT            Reddit 
    ##          2.216573          1.660846          3.199078          1.148110

``` r
# Plot the residuals vs fitted values
ggplot(social_media_numeric, aes(x = fitted(model), y = residuals(model))) +
  geom_point(alpha = 0.5) +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals")
```

![](Social-Media_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
# QQ-plot for normality check
qqnorm(residuals(model))
qqline(residuals(model))
```

![](Social-Media_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->
