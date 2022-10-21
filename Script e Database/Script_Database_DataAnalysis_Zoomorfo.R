###___________________________________________________________________________##
## Step 1 - Install (if needed) and load packages in the script.

# Package names:
packages <- c("readODS", "dplyr","factoextra")

# Install packages not yet installed:
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading:
invisible(lapply(packages, library, character.only = TRUE))

# Package function:
#ReadODS - Load spreadsheet in format .ODS (OpenOffice).
#Factoextra - Produce graphics (Dendrogram e k-means).
#Dplyr - Manipulate Database.


###___________________________________________________________________________##
## Step 2 - Import and Download of the Database:
# Database (database.ods) available on the project's GitHub.

URL_database <- 'https://raw.githubusercontent.com/F0ns3c4/Projeto_Artigo_Classificacao_Zoomorfo/main/Script%20e%20Database/database.ods'
download.file(URL_database, destfile =  "C:\\Users\\Public\\Documents\\database.ods",
              mode = "wb",
              method = 'auto')


Base_dados_zoomorfo=read_ods(path='C:\\Users\\Public\\Documents\\database.ods') #import database.

###___________________________________________________________________________##
## Step 3 - Preparation of the Database:

# Segregate columns to create factors and later numeric vectors:
ID = c(Base_dados_zoomorfo$ID)
cabeça = c(Base_dados_zoomorfo$Cabeça)
Pescoço = c(Base_dados_zoomorfo$Pescoço)
Tronco = c(Base_dados_zoomorfo$Tronco)
Membros = c(Base_dados_zoomorfo$Membros)
Patas = c(Base_dados_zoomorfo$Patas)
Cauda = c(Base_dados_zoomorfo$Cauda)

# Create column factors:
factor_cabeca = factor(cabeça)
factor_pescoco = factor(Pescoço)
factor_tronco = factor(Tronco)
factor_membros = factor(Membros)
factor_patas = factor(Patas)
factor_cauda = factor(Cauda)

# Transform columns into numeric vectors:
cabeca_numeric = as.numeric(factor_cabeca)
pescoco_numeric = as.numeric(factor_pescoco)
tronco_numeric = as.numeric(factor_tronco)
membro_numeric = as.numeric(factor_membros)
patas_numeric = as.numeric(factor_patas)
cauda_numeric = as.numeric(factor_cauda)

# Dataframe of the Database:
Dataframe_zoomorfo = data.frame(ID,
                                cabeca_numeric,
                                pescoco_numeric,
                                tronco_numeric,
                                membro_numeric,
                                patas_numeric,
                                cauda_numeric,
                                row.names = 1)

# Segregate Database into Categories (Bipeds and Quadrupeds):

DFBipede<-filter(Dataframe_zoomorfo,membro_numeric=="1"|membro_numeric=="3")
# Form Dataframe only associated with Biped.
DFQuadrupede <- filter(Dataframe_zoomorfo,membro_numeric == "4")
# Form Dataframe only associated with Quadruped.


###___________________________________________________________________________##
## Step 4 - Cluster Analysis (Ward.D2 Method):

# Bipeds____________:
# Formation of Biped Groups:
Cluster_Bipede = (hclust(dist(DFBipede), method = "ward.D2")) 

# Hierarchical Plotting of Biped Cluster Result:
fviz_dend(Cluster_Bipede, cex = 0.9, lwd = 1, k = 5, 
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
fviz_dend(Cluster_quadruped, cex = 1, lwd = 1, k = 8,
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
# Definition of Bidepes Scale:
Dados_escala_bipede = scale(na.omit(DFBipede))

# Generate Kmeans data:
Resultado_kmeans_bipede = kmeans(Dados_escala_bipede, center=5)

# Plotting Kmeans results:
fviz_cluster(Resultado_kmeans_bipede, data = Dados_escala_bipede,
             ellipse.type = "euclid",
             repel = T,
             labelsize = 12,
             xlab = FALSE,
             ylab = FALSE,
             main = "Agrupamentos K-média dos Bípedes")

# Quadrupeds____________:
# Definition of Quadrupeds Scale>
Dados_escala_quadrupede = scale(na.omit(DFQuadrupede))

# Generate Kmeans data:

Resultado_kmeans_quadrupede = kmeans(Dados_escala_quadrupede[,-4], center=8)

# Plotting Kmeans results:
fviz_cluster(Resultado_kmeans_quadrupede, data = Dados_escala_quadrupede[,-4],
             ellipse.type = "euclid",
             repel = T,
             labelsize = 11,
             xlab = FALSE,
             ylab = FALSE,
             main = "Agrupamentos K-média dos Quadrúpedes")

###__________________________________________________________________________###
