# panggil beberapa library yang dibutuhkan
library(cluster)
library(factoextra)
library(clustertend)

# Preprocessing Data

## Persiapkan data yang akan dianalisis, dalam hal ini akan menggunakan data USArrest yang tersedia di R
## Melihat data USArrest
print(USArrests)
## Simpan ke dalam variabel untuk dianalisis
df = USArrests
## memeriksa apakah terdapat data yang hilang
is.na(df)
## melakukan standarisasi (pernormalan) pada data sebelum di klaster
scaled.df = scale(df)
## melihat data yang telah di standarisasi
print(scaled.df)

# Analisis PCA

## Melakukan PCA dengan fungsi prcomp, hasilnya dimasukkan kedalam variabel PCA.df
PCA.df = prcomp(scaled.df)
## melihat hasil PCA
print(PCA.df)
## melakukan visualisasi dengan menggunakan fviz_pca_ind()
fviz_pca_ind(PCA.df, title="Visualisasi PCA.df", geom="point", stand=FALSE, palette="jco", ggtheme=theme_light())
# Menguji tendensi klaster

## menguji tendensi klaster menggunakan Hopkins
hopkins(scaled.df,n = nrow(scaled.df)-1)
## melihat tendensi klaster menggunakan uji VAT
fviz_dist(dist(scaled.df), show_labels = FALSE)+labs(title="USArrest (scaled) Data VAT")

# Mencari jumlah klaster optimal

## mencari jumlah klaster optimal menggunakan silhouette method
fviz_nbclust(scaled.df, kmeans, method="silhouette") + labs(subtitle="Silhouette Method")
## mencari jumlah klaster optimal menggunakan metode elbow
fviz_nbclust(scaled.df, kmeans, method="wss") + labs(subtitle="Elbow Method")

#Melakukan analisis klaster dengan menggunakan fanny

## melakukan analisis klaster dengan jumlah klaster 2, jarak euclidean
fanny.df = fanny(scaled.df,k=2, metric = "euclidean", stand = FALSE)
## melihat derajat keanggotaan tiap label
fanny.df$membership
## melihat koefisien partisi Dunn untuk melihat seberapa baku atau acaknya hasil klaster
fanny.df$coeff
## memvisualisasikan hasil klaster
fviz_cluster(fanny.df, ellipse.type = "norm", repel=TRUE, palette="jco", ggtheme = theme_minimal(), legend="bottom")
## memvisualisasikan plot silhouette
fviz_silhouette(fanny.df, palette="jco", ggtheme=theme_minimal())
