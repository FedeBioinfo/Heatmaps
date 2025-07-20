# I set the language to ENG
Sys.setenv(LANG = "en")

# I set the wd
setwd("C:/Users/Fede/Nextcloud/Data/Thomas_Phylogenomics/Heatmaps")

# I load the library of pheatmap
library(pheatmap)
library(RColorBrewer)
library(grid)

# I import the data, which is saved as tab-delimited text
SCPL<- read.delim("SCPL copy number.txt")

# rownames are the first column. 1st column must contain a single value (no spaces, dots, etc.) 
rownames(SCPL)<- SCPL[,1]

# I delete the first column. Now the first column 
# (column zero) contains the row names
SCPL[,1] <-NULL

# Now I can build a matrix from the data.frame
# pheatmap wants data as matrix
SCPL_matrix <- as.matrix(SCPL)

# I import the annotation file of the species
# it is better to prepare the file in excel and then copy and save it in .txt in notepad++
# the file annotation_species.txt is common to both BAHD and SCPL
an_species<- read.delim("annotation_species.txt")

# and set the first column to rownames as above
rownames(an_species)<- an_species[,1]
an_species[,1] <-NULL

# I import the annotation file for the OG (Clades)
an_OG<- read.delim("annotation_OG_SCPL.txt")

# and I set as above the first column as rownames
rownames(an_OG)<- an_OG[,1]
an_OG[,1] <-NULL

# I include annotations for the rows and columns
ann_colors= list(SCPL_Clade = c(I = "lightskyblue", II = "lightgoldenrod",
                                IV = "chartreuse4", V = "coral", VI = "gold", unassigned = "darkgrey"), 
                 Major_clades = c("Cyanobacteria_SAR_Algae" = "azure3", 
                 "Non-vascular_plants" = "darkkhaki", "Early_Tracheophytes" = "darkolivegreen",
                 Gymnosperms = "forestgreen", "Basal Angiosperms" = "burlywood1", 
                 "Non-commelinid_monocots" = "gold",
                 Commelinid_monocots = "goldenrod3", Eudicots = "tomato1"), 
                 Major_families = c(Other = "white", Poaceae = "yellow2", Solanaceae = "mediumorchid",
                                         Fabaceae = "steelblue1", Cucurbitaceae = "darkorange1",
                                         Brassicaceae = "yellow4"))
                 
                 
# if I need to include only one column from the data.frame, 
# it is taken as default without comma!
# e.g. an_OG[2]
# which means I take only the 2nd column

#I define the intervals for colors according to the number of genes
# 107 is the max BAHD copy number
# 84 is the max SCPL copy number
# I need to start from -1, so that all values from -1 to zero are labelled as azure3 
# in this way cells with no genes are labelled as azure3
breaks <- c(-1,0,2,6,10,30,50,84)

# the number of colors is equal to the number of values in breaks-1
colors <- c("azure3","khaki","darkgoldenrod1","coral","coral3","red3","brown4")

# first I check the pdf
pheatmap(SCPL_matrix, cluster_rows = FALSE,
         cluster_cols = FALSE, border_color="grey60", annotation_col = an_OG[2],
         annotation_row = an_species[, c(8,9)], annotation_colors = ann_colors, 
        show_rownames = F, breaks=breaks, color=colors, 
         legend_breaks = c(0,6,10,30,50,84), legend=T, cellwidth = 10, 
         cellheight = 4, fontsize = 8, filename = "SCPL copy number.pdf")

# then if I want to export a high-res image I select .tiff
pheatmap(SCPL_matrix, cluster_rows = FALSE,
         cluster_cols = FALSE, border_color="grey60", annotation_col = an_OG[2],
         annotation_row = an_species[, c(8,9)], annotation_colors = ann_colors, 
         show_rownames = F, breaks=breaks, color=colors, 
         legend_breaks = c(0,6,10,30,50,84), legend=T, cellwidth = 10, 
         cellheight = 4, fontsize = 8, filename = "SCPL copy number.tiff")

