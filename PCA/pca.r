rm(list = ls())

# Run preprocessing python script to generate bci.csv
bci = read.csv("./bci.csv")

features<-bci[1:12]
targets<-bci[,13]

# Correlation pair plots
pairs(features)

# Numerical correlations
round(cor(features),2)

# Apply PCA
eig<-prcomp(features, center = TRUE, scale. = FALSE)

# Eigenvalues
sort(lam<-eig$sdev^2)

# Sum of sample variance for each feature
sum(diag(cov(features)))

# Sum of variance along the PCs
sum(lam) 

# Proportion of the total variation that represents each PC
(per_var<-round(lam/sum(lam),3))


# scree plot
plot(seq(1,12), per_var, type="b", xlab="Principal Component", ylab="Proportion of variance explained", main="Scree plot")

# Composition of the PCs in relation with the original data
eig$rotation

pc2<-eig$x[,1:2]

# Score plot
plot(pc2[,1], pc2[,2], xlab="PC1", ylab="PC2", main="Score plot")
abline(h=0, v=0, lty=2, col="red", lwd=1)

# Visualizing C1
points(pc2[198:396,1],pc2[198:396,2], col="orange", pch=16)

# Visualizing C2
points(pc2[1:197,1],pc2[1:197,2], col="skyblue", pch=16)
