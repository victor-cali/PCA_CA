#import libraries
library("FactoMineR")
library("factoextra")
library("dplyr")
library(ca)
library("gplots")
#original database
data(wg93)
head(wg93)
?wg93
#non-modified database structure
summary(wg93)
#We proceed to clean the database, since we are only interested in question 2: "Overall, modern science does more harm than good"
q2<-wg93[,-c(1,3,4)]
head(q2)
summary(q2)

#Given the types of answers, ranging from 1 (Agree Strongly)
#up to 5 (Disagree Strongly)
#We will consider as our study sample the people who support the opposite idea to the one stated in the question (science does more good than harm). This means we will only consider answers 4 (Disagree) and 5 (Strongly Disagree)
q2_filter<-filter(q2,q2$B==4 | q2$B ==5)
head(q2_filter)
summary(q2_filter)

#The reseach/motivation question is to look for possible ways in which the age and education level variables can be related (over the specified sub-sample previously mentioned)

## Contingency Table:

#We proceed to create the contingency table from the data by separating the variables of interest:
x<-table(q2_filter$age)
y<-table(q2_filter$edu)
cont_tab<-table(q2_filter$age,q2_filter$edu)
#We will change the names of the rows and columns to the specified categories/intervals stated in the report
# changing row names of data frame
rownames <- rownames(cont_tab)
rownames(cont_tab) <- c("18-23","24-29","30-35","36-41","42-47","48-53")

# changing column names of data frame
colnames(cont_tab) <- c('LSnVC','LSwVC','UpSC','GrSC','UniBD','PostGr')

cont_tab

#dt <- as.table(as.matrix(cont_tab))
# Now, use the function: 
balloonplot(cont_tab, main ="", xlab ="age", ylab="education level",label = TRUE, show.margins = TRUE)

## Chi-Square Test
C2T <- chisq.test(cont_tab)
C2T
# Remember: the test consists in the following hypotheses: 
### H0: "there is no relationship between categories (row vs columns)
### HA: "there is a relationship between categories 

# The pvalue is really small; hence, there is a lot of evidence against H0. 
# We reject H0 favoring HA. 

### 3. Computing CA 
# Use the function CA() to perform CA. A simplified format is
# CA(X, ncp = 5, graph = TRUE)
# X : a data frame (contingency table)
# ncp : number of dimensions kept in the final results.
# graph : a logical value. If TRUE a graph is displayed.

# To compute correspondence analysis, do the following:
ob.ca <- CA(cont_tab, graph = FALSE)

# The output is a list including various pieces of information.
ob.ca

### 4. Visualization and interpretation
# The first step is to evaluate whether there is a significant relationship
# between the rows and columns.This is done with the Chi-Square test. 
# As we know, the test was significant. However, it also appears at the 
# beginning of the output ob.ca. 

# As in PCA, we examine the eigenvalues to determine the number of axis to 
# consider when representing the data in a few dimensions. 
# The eigenvalues and the proportion of variances retained by the different
# axes can be extracted using 'get_eigenvalue()'.

eig.val <- get_eigenvalue(ob.ca)
eig.val
avg_eig.val<-(eig.val[,c(2)])
avg_eig.val
mean(avg_eig.val)
# Eigenvalues represent the amount of information retained by each axis.
# Dimensions are ordered decreasingly and listed according to the amount of
# variance explained.
# Dimension 1 explains the most variance (64%), followed by dimension 2 (21% approx.), etc.
# The first two dimensions cover almost 85% of the variation in the data. Sounds good. 

# We can also rely (visually) in the scree plot: 
fviz_screeplot(ob.ca, addlabels = TRUE, ylim = c(0, 70))

# visually, it's pretty clear that a 2 dim solution is acceptable and should perform well.   

# The next piece of R code draws the scree plot with a red dashed line  specifying the average eigenvalue:
fviz_screeplot(ob.ca) +
  geom_hline(yintercept=mean(avg_eig.val), linetype=2, color="red")

# Since there are 5 components, the average eigenvalue should cover 20% or 1/5 of the
# total variation. Notice the first Component is considerably above the average; and the second one hits just right on the average value. Given the previous statement, choosing 2-dim as a solution sounds reasonable Biplot
# We can now draw the biplot by using this function: 
fviz_ca_biplot(ob.ca, repel = TRUE)
# Rows are represented by blue points and columns by red triangles.
# The distance between any row points or column points gives a measure of their similarity. 
# Row points with similar profile are close on the factor map. 
# The same holds true for column points. 

# Symetric plot represents the row and column profiles simultaneously in a common space. 
# In this case, only the distance between row points or the distance between column points can be really interpreted.
# The distance between any row and column items is not meaningful.
# In order to interpret the distance between column and row points, the column profiles must be presented in row space or vice-versa. This is called asymmetric biplot. 

# The next step for the interpretation is to determine which row and column variables contribute the most in the definition of the dimensions retained in the model.

#### Graph of row variables
# The function get_ca_row() is used to extract the results for row variables.
# This function returns a list containing 
# -- the coordinates,
# -- the cos2, 
# -- the contribution and the variance row variables. 

row <- get_ca_row(ob.ca)
row

# The components in 'row' can be used in the plot of rows as follows:
# row$coord: coordinates of each row point in each dimension (1, 2) Used to create the plot.
# row$cos2: quality of representation of rows.
# var$contrib: contribution of rows (in %) to the definition of the dimensions.

# To get a taste of them, try:
# Coordinates
head(row$coord)
# Cos2: quality on the factor map
head(row$cos2)
# Contributions to the principal components
head(row$contrib)

# row$coord shows the coordinates of each row point in each dimension 
# If we'd like to visualize only row points: 
fviz_ca_row(ob.ca, repel = TRUE)

# You might like to change color or symbol, like in here:
fviz_ca_row(ob.ca, col.row="darkorange", shape.row = 14)

# REMARKS: 
# -- Rows with a similar profile are grouped together.
# -- Negatively correlated rows are positioned on opposite sides of the plot origin (opposed quadrants).
# -- The distance between row points and the origin measures the quality of the row points on the factor map.
# -- Row points that are away from the origin are well represented on the factor map.

### About the quality of representation of rows
# The result of the analysis shows that the contingency table has been successfully represented in 2-dim. Both cover 88.6% of the total (variation) in the data.
# However, not all the points are equally well displayed in this reduced space

# The quality of representation of the rows on the biplot is called the squared cosine (cos2) or the squared correlations.

# The cos2 measures the degree of association between rows/columns and a particular axis. The cos2 of row points can be extracted as follow:

head(row$cos2, 6)

# The values of the cos2 are between 0 and 1. The sum of the cos2 for rows on all the CA dimensions is equal to one.

apply(row$cos,1,sum)

# The quality of representation of a row or column in n dimensions is simply the sum of the squared cosine of that row or column over those n dimensions.
# In this case, we'd like to consider the sum over the first two dimensions. 

round(1-row$cos[,3],2)

# As can be seen, most of row points are well represented. All groups happen to be represented in this new 2 dimensional space except for the 24-29 and 36-41 groups (which happens to be quite well represented over the third component).   

# We can represent via colors how well a row point is represented in the biplot. 
# Color by cos2 values: quality on the biplot
fviz_ca_row(ob.ca, col.row = "cos2",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
            repel = TRUE)

# As before, practically all of them are very well represented, except for the 24-29 and 36-41 groups of age 

# You can visualize the cos2 of row points on all the dimensions using the corrplot package:
library("corrplot")
corrplot(row$cos2, is.corr=FALSE)

# As you can see, the 24-29 and 36 to 41 age groups are well represented using 3 components, while the others are well represented with just 2 dimensions. 
# We can also create a bar plot of rows' cos2 over the first 2 dim. 
fviz_cos2(ob.ca, choice = "row", axes = 1:2)

#Given the previous, we must be cautious on how we interpret the 24-29 and 36 to 41 age groups due to their bad quality of representation on the biplot.

### Contributions of rows to the dimensions (Components)
# The contribution of rows (in %) to the definition of the dimensions can be extracted as follows:
row$contrib

# The row variables with the larger value, contribute the most to the definition of the dimensions. 
# Rows that contribute the most to Dim.1 and Dim.2 are the most important in explaining the variability in the data set.
# Rows that do not contribute much to any dimension or that contribute to the last dimensions are less important.
# It’s possible to use the function corrplot() to highlight the most contributing row points for each dimension:

library("corrplot")
corrplot(row$contrib, is.corr=FALSE)
?corrplot

apply(row$contrib,2,sum)

# The function fviz_contrib() can be used to draw a bar plot of row contributions.
# If the data contains many rows, you can decide to show only the top contributing rows. 
# The R code below shows the top 10 rows contributing to the dimensions:

# Contributions of rows to dimension 1
fviz_contrib(ob.ca, choice = "row", axes = 1, top = 10)
mean(row$contrib[,1])

# Contributions of rows to dimension 2
fviz_contrib(ob.ca, choice = "row", axes = 2, top = 10)
mean(row$contrib[,2])

# Total contribution in both dimension 1 and 2
fviz_contrib(ob.ca, choice = "row", axes = 1:2, top = 10)
rev(sort(apply(row$contrib[,1:2],1,mean)))

# The most important (or, contributing) row points can be highlighted on the scatter plot as follow:
fviz_ca_row(ob.ca, col.row = "contrib",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
            repel = TRUE)

#### Graph of column variables
# The function get_ca_col()  is used to extract the results for column variables. This function returns a list containing the coordinates, the cos2, the contribution and the variance of columns variables:                                                                      

col <- get_ca_col(ob.ca)
col

# The result for columns gives the same information as described for rows. 

# The fviz_ca_col() is used to produce the graph of column points. To create a simple plot, type this:
fviz_ca_col(ob.ca)

# Like row points, it's also possible to color column points by their cos2 values:
fviz_ca_col(ob.ca, col.col = "cos2",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
            repel = TRUE)

# Recall the value of the cos2 is between 0 and 1. A cos2 closed to 1 corresponds to a column/row variables that are well represented on the factor map.
# Note that, only the column item  is not very well displayed on the first two dimensions. The position of this item must be interpreted with caution in the space formed by dimensions 1 and 2.

# To visualize the contribution of rows to the first two dimensions, type this:
fviz_contrib(ob.ca, choice = "col", axes = 1:2)
col$cos2
