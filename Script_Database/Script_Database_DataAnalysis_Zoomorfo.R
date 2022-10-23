###___________________________________________________________________________##
## Step 1 - Install (if needed) and load packages in the script.

# Package names:
packages <- c("readODS", "dplyr","factoextra", "cowplot")

# Install packages not yet installed:
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading:
invisible(lapply(packages, library, character.only = TRUE))

# Package function:
#ReadODS - Load spreadsheet in format .ODS (OpenOffice).
#Dplyr - Manipulate Database.
#Factoextra - Produce graphics (Dendrogram e k-means).
#Cowplot - Add-on graphics resources.


###___________________________________________________________________________##
## Step 2 - Import (download and load) of the database:
# Database (database.ods) available on the project's GitHub.

URL_database <- 'https://raw.githubusercontent.com/F0ns3c4/Projeto_Artigo_Classificacao_Zoomorfo/main/Script_Database/database.ods'
download.file(URL_database, destfile =  "C:\\Users\\Public\\Documents\\database.ods",
              mode = "wb",
              method = 'auto') #download the database on the computer.

Base_dados_zoomorfo=read_ods(path='C:\\Users\\Public\\Documents\\database.ods') #import database.


###___________________________________________________________________________##
## Step 3 - Preparation of the Database (using Dplyr):

# Define pictorial units column in 'rowname'
rownames(Base_dados_zoomorfo) <- Base_dados_zoomorfo$ID
Base_dados_zoomorfo<-Base_dados_zoomorfo[-c(1)]

# Transform the columns in factors:
Base_dados_zoomorfo<-Base_dados_zoomorfo %>% mutate_if(is.character, as.factor)

# Transform the columns in numeric vectors:
Base_dados_zoomorfo<-Base_dados_zoomorfo %>% mutate_if(is.factor, as.numeric)

# Segregate Database into Categories - Bipeds and Quadrupeds:

DFBipede<-filter(Base_dados_zoomorfo,Membros=="1"|Membros=="3")
# Form Dataframe only associated with Biped.
DFQuadrupede <- filter(Base_dados_zoomorfo,Membros == "4")
# Form Dataframe only associated with Quadruped.


###___________________________________________________________________________##
## Step 4 - Cluster analysis (Ward.D and Ward.D2 Methods):

# Bipeds____________:
# Formation of Biped Groups:
Cluster_Bipede = (hclust(dist(DFBipede), method = "ward.D"))

# Hierarchical Plotting of Biped Cluster Result:
graphic_1<-fviz_dend(Cluster_Bipede, cex = 0.9, lwd = 1, k = 5, 
          rect = T, 
          rect_border = "gray",
          rect_fill = T,
          horiz = F,
          ylab = "Similaridade",
          xlab = "Unidade Pictórica",
          main = "Agrupamento Hierárquico (Dendograma Bípedes)")

# Quadrupeds____________:
# Formation of Quadruped Groups:
Cluster_quadruped = (hclust(dist(DFQuadrupede), method = "ward.D2"))

# Hierarchical Plotting of Quadruped Cluster Result:
graphic_3<-fviz_dend(Cluster_quadruped, cex = 1, lwd = 1, k = 8,
          rect = T, 
          rect_border = "gray",
          rect_fill = T,
          horiz = F,
          ylab = "Similaridade",
          xlab = "Unidade Pictórica",
          main = "Agrupamento Hierárquico (Dendograma Quadrúpedes)")

###___________________________________________________________________________##
## Step 5 - K-means analysis of clusters:

# Bipeds____________:
# Definition of Bidepes scale:
Dados_escala_bipede = scale(na.omit(DFBipede))

# Generate Kmeans data:
Resultado_kmeans_bipede = kmeans(Dados_escala_bipede, center=5)

# Plotting Kmeans results:
graphic_2<-fviz_cluster(Resultado_kmeans_bipede, data = Dados_escala_bipede,
             ellipse.type = "euclid",
             repel = T,
             labelsize = 12,
             xlab = FALSE,
             ylab = FALSE,
             ggtheme = theme_gray(),
             main = "Agrupamentos K-média dos Bípedes")

# Quadrupeds____________:
# Definition of Quadrupeds scale>
Dados_escala_quadrupede = scale(na.omit(DFQuadrupede))

# Generate Kmeans data:

Resultado_kmeans_quadrupede = kmeans(Dados_escala_quadrupede[,-4], center=8)

# Plotting Kmeans results:
graphic_4<-fviz_cluster(Resultado_kmeans_quadrupede, data = Dados_escala_quadrupede[,-4],
             ellipse.type = "euclid",
             repel = T,
             labelsize = 11,
             xlab = FALSE,
             ylab = FALSE,
             ggtheme = theme_gray(),
             main = "Agrupamentos K-média dos Quadrúpedes")

#Plot all article graphs.
plot_grid(graphic_1, graphic_2, graphic_3, graphic_4)


### Better view all graph results in Plots\Zoom______________________________###
### End of process___________________________________________________________###