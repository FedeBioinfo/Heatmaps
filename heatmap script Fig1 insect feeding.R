# I set the language to ENG
Sys.setenv(LANG = "en")

# I set the wdir on the institute's PC
setwd("C:/Users/COMPUTER/Nextcloud/Data/Flytrap Ines/Submission Plant J/1st round major revisions Plant J/Fig1 insect feeding")

# or I set the wdir on my computer at home
setwd("C:/Users/Fede/Nextcloud/Data/Flytrap Ines/Submission Plant J/Resubmission Plant J/Fig1 insect feeding")

# I load the library of pheatmap
library(pheatmap)
library(RColorBrewer)
library(grid)

?pheatmap

# I import the data, which is saved as tab-delimited text
# if some values are missing, NAs will be imputed automatically 
datalog2mc<- read.delim("meancentered_log2data_heatmapfig1.txt")

# rownames are the first column. 1st column must contain a single value (no spaces, dots, etc.) 
rownames(datalog2mc)<- datalog2mc[,1]

# I delete the first column. Now the first column 
# (column zero) contains the row names
datalog2mc[,1] <-NULL

# Now I can build a matrix from the data.frame
# pheatmap wants data as matrix
data_matrix_log2mc <- as.matrix(datalog2mc)

# are the NAs properly recognized?
is.na(data_matrix_log2mc)

# I import the annotation file of the metabolites
# it is better to prepare the file in excel and then copy and save it in .txt in notepad++
metabolite_annotation<- read.delim("metabolite_annotation.txt")

# and set the first column to rownames as above
rownames(metabolite_annotation)<- metabolite_annotation[,1]
metabolite_annotation[,1] <-NULL

# I import the annotation file for the conditions (columns)
condition_annotation<- read.delim("condition_annotation.txt")

# and I set as above the first column as rownames
rownames(condition_annotation)<- condition_annotation[,1]
condition_annotation[,1] <-NULL

#I include annotations for the rows and columns- this is for Fig 1 (insect experiment)
#here I don't see the annotation for the columns because I have the spacer columns !
# remember that the order of the labels here is as it should appear in the legend next to the heatmap!

ann_colors= list(Tissue = c("TRAP" = "brown4", "PETIOLE" = "green4"), 
                Treatment = c("CONTROL"= "ivory", "MECHANOSTIMULATION" = "lightblue",
                              "CORONATINE" = "antiquewhite3","INSECT" = "goldenrod2"), 
                Timepoint = c("0h"="gray100" , "1d"= "gray70", "3d"= "gray40", "6d"= "gray10"), 
                Class = c("Amino acids" = "gold", "Organic acids" = "royalblue1", Carbohydrates = "violet",  
                          "Alcohols and polyols" = "mediumvioletred", "Sugar-P" = "purple", 
                          "N-containing compounds" = "khaki1",
                          Others = "springgreen3"))

# I include annotations for the rows and columns- this is for Fig 4 (feeding substances experiment)
ann_colors= list(Tissue = c(Trap = "palevioletred2", Petiole = "palegreen3"), 
                 Treatment = c(Water = "azure", Urea = "yellow", Chitin = "tan3", Casein = "bisque", 
                               DNA = "rosybrown1", 
                               Phospholipids = "powderblue"), 
                 Timepoint = c("0d" = "gray99", "1d"= "snow", "3d"= "snow3", "6d"= "snow4"), 
                 Class = c(Aminoacids = "gold", "Organic acids" = "royalblue1", Carbohydrates = "violet",  
                           "Alcohols and polyols" = "mediumvioletred", "Sugar-P" = "purple", 
                           "N-containing compounds" = "khaki1",
                           Others = "springgreen3"))


# if I need to include only one column from the data.frame, 
# it is taken as default without comma!
# e.g. an_OG[2]
# which means I take only the 2nd column

# if I need to take multiple columns from condition_annotation, I need to bind them
# e.g. annotation_col = condition_annotation[, c(1,2,3)] 
# in this case I use columns 1, 2 and 3 from the obj condition_annotation

# first I check the pdf
# even if there are NAs, clustering can be done

# if I dont' scale values, and don't do a log transformation, I need a sequential palette
display.brewer.all()

# this is to check with absolute data
pheatmap(data_matrix, color = colorRampPalette((brewer.pal(n = 9, name = "YlOrRd")))(100), 
         cluster_rows = FALSE, cluster_cols = FALSE, border_color = "grey60", angle_col = 45,
         annotation_col = condition_annotation[, c(1,2,3)],
         annotation_row = metabolite_annotation[1], 
         annotation_colors = ann_colors,
         show_rownames = T, fontsize_row = 5, show_colnames = T, fontsize_col = 5,
         gaps_row = c(25,46,56,61,66,73),
         gaps_col = c(2,5,8,11,13,16,19),
         legend=T, cellwidth = 10, 
         cellheight = 5, fontsize=8)

# export
pheatmap(data_matrix, color = colorRampPalette((brewer.pal(n = 9, name = "YlOrRd")))(100), 
         cluster_rows = FALSE, cluster_cols = FALSE, border_color = "grey60", angle_col = 45,
         annotation_col = condition_annotation[, c(1,2,3)],
         annotation_row = metabolite_annotation[1], 
         annotation_colors = ann_colors,
         show_rownames = T, fontsize_row = 5, show_colnames = T, fontsize_col = 5,
         gaps_row = c(25,46,56,61,66,73),
         gaps_col = c(2,5,8,11,13,16,19),
         legend=T, cellwidth = 10, 
         cellheight = 5, fontsize=8, filename="Fig 1 with absolute intensities.jpg")

dev.off()

# I try now with log10-transformed data, so I simply did log10 of the absolute data
log10data<- read.delim("log10data_heatmapfig1.txt")

# rownames are the first column. 1st column must contain a single value (no spaces, dots, etc.) 
rownames(log10data)<- log10data[,1]

# I delete the first column. Now the first column 
# (column zero) contains the row names
log10data[,1] <-NULL

# Now I can build a matrix from the data.frame
# pheatmap wants data as matrix
log10data_matrix <- as.matrix(log10data)

?brewer.pal
display.brewer.all()
?pheatmap

dev.off()

# now I check the heatmap with log10-transformed data
pheatmap(log10data_matrix, color = colorRampPalette((brewer.pal(n = 9, name = "YlOrRd")))(100), 
         cluster_rows = FALSE, cluster_cols = FALSE, border_color = "grey60", angle_col = 45,
         annotation_col = condition_annotation[, c(1,2,3)],
         annotation_row = metabolite_annotation[1], 
         annotation_colors = ann_colors,
         show_rownames = T, fontsize_row = 5, show_colnames = T, fontsize_col = 5,
         gaps_row = c(25,46,56,61,66,73),
         gaps_col = c(2,5,8,11,13,16,19),
         legend=T, cellwidth = 10, 
         cellheight = 5, fontsize=8)

dev.off()

# export
pheatmap(log10data_matrix, color = colorRampPalette((brewer.pal(n = 9, name = "YlOrRd")))(100), 
         cluster_rows = FALSE, cluster_cols = FALSE, border_color = "grey60", angle_col = 45,
         annotation_col = condition_annotation[, c(1,2,3)],
         annotation_row = metabolite_annotation[1], 
         annotation_colors = ann_colors,
         show_rownames = T, fontsize_row = 5, show_colnames = T, fontsize_col = 5,
         gaps_row = c(25,46,56,61,66,73),
         gaps_col = c(2,5,8,11,13,16,19),
         legend=T, cellwidth = 10, 
         cellheight = 5, fontsize=8, filename="Fig 1 with log10 data.jpg")

dev.off()

# I try now with mean-centered (mc) log10-transformed data
mclog10data<- read.delim("meancentered_log10data_heatmapfig1.txt")

# rownames are the first column. 1st column must contain a single value (no spaces, dots, etc.) 
rownames(mclog10data)<- mclog10data[,1]

# I delete the first column. Now the first column 
# (column zero) contains the row names
mclog10data[,1] <-NULL

# Now I can build a matrix from the data.frame
# pheatmap wants data as matrix
mclog10data_matrix <- as.matrix(mclog10data)

# now, with mean-centered data, I need a red/blue color scale ! (always reversed)
pheatmap(mclog10data_matrix, color = colorRampPalette(rev(brewer.pal(n = 9, name = "RdBu")))(100), 
         cluster_rows = FALSE, cluster_cols = FALSE, border_color = "grey60", angle_col = 45,
         annotation_col = condition_annotation[, c(1,2,3)],
         annotation_row = metabolite_annotation[1], 
         annotation_colors = ann_colors,
         show_rownames = T, fontsize_row = 5, show_colnames = T, fontsize_col = 5,
         gaps_row = c(25,46,56,61,66,73),
         gaps_col = c(2,5,8,11,13,16,19),
         legend=T, cellwidth = 10, 
         cellheight = 5, fontsize=8)

# export
pheatmap(mclog10data_matrix, color = colorRampPalette(rev(brewer.pal(n = 9, name = "RdBu")))(100), 
         cluster_rows = FALSE, cluster_cols = FALSE, border_color = "grey60", angle_col = 45,
         annotation_col = condition_annotation[, c(1,2,3)],
         annotation_row = metabolite_annotation[1], 
         annotation_colors = ann_colors,
         show_rownames = T, fontsize_row = 5, show_colnames = T, fontsize_col = 5,
         gaps_row = c(25,46,56,61,66,73),
         gaps_col = c(2,5,8,11,13,16,19),
         legend=T, cellwidth = 10, 
         cellheight = 5, fontsize=8, filename="Fig 1 with mean centered log10 data.jpg")

pheatmap(data_matrix, color = colorRampPalette((brewer.pal(n = 9, name = "YlOrRd")))(1000), 
         cluster_rows = FALSE, cluster_cols = FALSE, border_color = "grey60", 
         annotation_col = condition_annotation[, c(1,2,3)],
         annotation_row = metabolite_annotation[1], 
         annotation_colors = ann_colors,
         show_rownames = T, fontsize_row = 4, show_colnames = T, fontsize_col = 4,
         legend=T, cellwidth = 10, 
         cellheight = 4, fontsize = 6, filename = "Fig 1.pdf")

pheatmap(data_matrix, cluster_rows = FALSE, color = colorRampPalette(rev(brewer.pal(n = 11, name = "RdBu")))(11), 
         cluster_cols = TRUE, clustering_distance_cols="euclidean", border_color = "grey60", 
         annotation_col = condition_annotation[, c(1,2,3)],
         annotation_row = metabolite_annotation[1], 
         annotation_colors = ann_colors, cutree_cols = 3,
         show_rownames = T, fontsize_row = 4, show_colnames = T, fontsize_col = 4,
         legend=T, cellwidth = 10, 
         cellheight = 4, fontsize = 6, na_col = "gray",
           filename = "Fig 4_euclidean.pdf")

pheatmap(data_matrix, cluster_rows = FALSE, color = colorRampPalette(rev(brewer.pal(n = 11, name = "RdBu")))(11), 
         cluster_cols = TRUE, clustering_distance_cols="euclidean", border_color = "grey60", 
         annotation_col = condition_annotation[, c(1,2,3)],
         annotation_row = metabolite_annotation[1], 
         annotation_colors = ann_colors, cutree_cols = 3,
         show_rownames = T, fontsize_row = 4, show_colnames = T, fontsize_col = 4,
         legend=T, cellwidth = 10, 
         cellheight = 4, fontsize = 6, na_col = "gray",
         filename = "Fig 4_euclidean.jpg")

# If I don't want clustering on the columns
pheatmap(data_matrix, cluster_rows = FALSE, color = colorRampPalette(rev(brewer.pal(n = 11, name = "RdBu")))(11), 
         cluster_cols = FALSE, border_color = "grey60", 
         annotation_col = condition_annotation[, c(1,2,3)],
         annotation_row = metabolite_annotation[1], 
         annotation_colors = ann_colors, cutree_cols = 3,
         show_rownames = T, fontsize_row = 4, show_colnames = T, fontsize_col = 4,
         legend=T, cellwidth = 10, 
         cellheight = 4, fontsize = 6, na_col = "gray",
         filename = "Fig 4_noclustering.pdf")

?pheatmap

# then if I want to export a high-res image I select .tiff or .jpg in the filename

pheatmap(data_matrix, cluster_rows = FALSE, color = colorRampPalette(rev(brewer.pal(n = 11, name = "RdBu")))(11), 
         cluster_cols = TRUE, clustering_distance_cols="euclidean", border_color = "grey60", 
         annotation_col = condition_annotation[, c(1,2,3)],
         annotation_row = metabolite_annotation[1], 
         annotation_colors = ann_colors, 
         show_rownames = T, fontsize_row = 4, show_colnames = T, fontsize_col = 4,
         legend=T, cellwidth = 10, 
         cellheight = 4, fontsize = 6, filename = "Fig 1_euclidean.tiff")

pheatmap(data_matrix, cluster_rows = FALSE, color = colorRampPalette(rev(brewer.pal(n = 11, name = "RdBu")))(11), 
         cluster_cols = TRUE, clustering_distance_cols="euclidean", border_color = "grey60", 
         annotation_col = condition_annotation[, c(1,2,3)],
         annotation_row = metabolite_annotation[1], 
         annotation_colors = ann_colors, 
         show_rownames = T, fontsize_row = 4, show_colnames = T, fontsize_col = 4,
         legend=T, cellwidth = 10, 
         cellheight = 4, fontsize = 6, filename = "Fig 1_euclidean.jpg")

###############with log2 and mean-centered data and fixed color scale#####################
# this is to check with log2 data (so, a two-color scale, "diverging color palette")
# max number of colors for RdBu is 11
brewer.pal.info
?pheatmap

pheatmap(data_matrix_log2mc, color = colorRampPalette(rev(brewer.pal(n = 6, name = "RdBu")))(6), 
         cluster_rows = FALSE, cluster_cols = FALSE, border_color = "grey60", angle_col = 45,
         annotation_col = condition_annotation[, c(1,2,3)],
         annotation_row = metabolite_annotation[1], 
         annotation_colors = ann_colors,
         show_rownames = T, fontsize_row = 5, show_colnames = T, fontsize_col = 5,
         gaps_row = c(25,46,56,61,66,73),
         gaps_col = c(2,5,8,11,13,16,19),
         legend=T, cellwidth = 10, 
         cellheight = 5, fontsize=8)

# not clear what it does in this way
pheatmap(data_matrix, color = colorRampPalette(rev(brewer.pal(n = 9, name = "RdBu")))(12)[6:12], 
         cluster_rows = FALSE, cluster_cols = FALSE, border_color = "grey60", angle_col = 45,
         annotation_col = condition_annotation[, c(1,2,3)],
         annotation_row = metabolite_annotation[1], 
         annotation_colors = ann_colors,
         show_rownames = T, fontsize_row = 5, show_colnames = T, fontsize_col = 5,
         gaps_row = c(25,46,56,61,66,73),
         gaps_col = c(2,5,8,11,13,16,19),
         legend=T, cellwidth = 10, 
         cellheight = 5, fontsize=8)

# so, I need to adjust the max and min color values to make some relatively minor changes more evident
# I need to impute colors manually using breaks !

# first, let's look at the colors in RdBu
display.brewer.pal(n = 11, name = 'RdBu')
# and get the hexadecimal code
brewer.pal(n = 11, name = "RdBu")

#I define the intervals (breaks) for colors according to the interval of the values in the heatmap
# MIN is -6.471
# MAX is +7.8759
# let's see if with these breaks it works nice
breaks <- c(-6.471, -3, -1.5, -0.75, -0.15, 0, 0.15, 0.75, 1.5, 3, 7.8759)

# the number of colors is equal to the number of values in breaks-1
colors <- c("#053061","#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B","#67001F")

# now, I include the breaks and the colors
pheatmap(data_matrix_log2mc, 
         color = colors, breaks=breaks,
         cluster_rows = FALSE, cluster_cols = FALSE, border_color = "grey60", angle_col = 45,
         annotation_col = condition_annotation[, c(1,2,3)],
         annotation_row = metabolite_annotation[1], 
         annotation_colors = ann_colors,
         show_rownames = T, fontsize_row = 5, show_colnames = T, fontsize_col = 5,
         gaps_row = c(25,46,56,61,66,73),
         gaps_col = c(2,5,8,11,13,16,19),
         legend=T, cellwidth = 10, 
         cellheight = 5, fontsize=8)

?pheatmap
# If I want to set the points of the legend breaks for the colors, but this is not really necessary,
# the automatic setpoints are OK
legend_breaks = c(-6, -3, -1.5, 0, 1.5, 3, 6)

#Since the older figure has darker colors, I try to reduce the breaks
breaks2 <- c(-6.471, -3, -1.5, -0.5, 0, 0.5, 1.5, 3, 7.8759)

# and I adjust the number of colors, which has to br equal to the number of values in breaks-1
colors2 <- c("#053061","#2166AC","#4393C3","#92C5DE","#F4A582","#D6604D","#B2182B","#67001F")

#this is now OK
pheatmap(data_matrix_log2mc, 
         color = colors2, breaks=breaks2,
         legend_breaks = c(-6, -3, -1.5, 0, 1.5, 3, 6),
         cluster_rows = FALSE, cluster_cols = FALSE, border_color = "grey60", angle_col = 45,
         annotation_col = condition_annotation[, c(1,2,3)],
         annotation_row = metabolite_annotation[1], 
         annotation_colors = ann_colors,
         show_rownames = T, fontsize_row = 5, show_colnames = T, fontsize_col = 5,
         gaps_row = c(25,46,56,61,66,73),
         gaps_col = c(2,5,8,11,13,16,19),
         legend=T, cellwidth = 10, 
         cellheight = 5, fontsize=8)

#export to jpg
pheatmap(data_matrix_log2mc, 
         color = colors2, breaks=breaks2,
         legend_breaks = c(-6, -3, -1.5, 0, 1.5, 3, 6),
         cluster_rows = FALSE, cluster_cols = FALSE, border_color = "grey60", angle_col = 45,
         annotation_col = condition_annotation[, c(1,2,3)],
         annotation_row = metabolite_annotation[1], 
         annotation_colors = ann_colors,
         show_rownames = T, fontsize_row = 5, show_colnames = T, fontsize_col = 5,
         gaps_row = c(25,46,56,61,66,73),
         gaps_col = c(2,5,8,11,13,16,19),
         legend=T, cellwidth = 10, 
         cellheight = 5, fontsize=8,
         filename = "Fig 1_log2 mean centered mechano.jpg")

