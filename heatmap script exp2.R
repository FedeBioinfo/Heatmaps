# I set the language to ENG
Sys.setenv(LANG = "en")

# I set the wdir on the institute's PC
setwd("C:/Users/COMPUTER/Nextcloud/Data/Flytrap Ines/Submission Plant J/Resubmission Plant J/Fig1")

# or I set the wdir on my computer at home
setwd("C:/Users/Fede/Nextcloud/Data/Flytrap Ines/Submission Plant J/Resubmission Plant J/Fig 4 substance feeding/Heatmap for Fig 4")

# I load the library of pheatmap
library(pheatmap)
library(RColorBrewer)
library(grid)

?pheatmap

# I import the data, which is saved as tab-delimited text
# if some values are missing, NAs will be imputed automatically 
datalog2mc<- read.delim("data_heatmapfig4_log2_mc.txt")

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

ann_colors= list(Tissue = c("TRAP" = "brown4", "PETIOLE" = "green4"), 
                Treatment = c("control"= "ivory", "CORONATINE" = "antiquewhite1",
                              "H2O" = "lightblue", "INSECT" = "goldenrod2"), 
                Timepoint = c("0h"="gray100" , "1d"= "gray70", "3d"= "gray40", "6d"= "gray10"), 
                Class = c("Amino acids" = "gold", "Organic acids" = "royalblue1", Carbohydrates = "violet",  
                          "Alcohols and polyols" = "mediumvioletred", "Sugar-P" = "purple", 
                          "N-containing compounds" = "khaki1",
                          Others = "springgreen3"))

# and this is for Fig. 4 (experiment2)
ann_colors= list(Tissue = c("TRAP" = "brown4", "PETIOLE" = "green4"), 
                 Treatment = c("control"= "ivory", "CORONATINE" = "antiquewhite1",
                               "urea" = "lightgoldenrod1", "chitin" = "tan", 
                               "casein" = "lavender", "DNA" = "thistle3",
                               "phospholipids" = "burlywood1"), 
                 Timepoint = c("0h"="gray100" , "1d"= "gray70", "3d"= "gray40", "6d"= "gray10"), 
                 Class = c("Amino acids" = "gold", "Organic acids" = "royalblue1", Carbohydrates = "violet",  
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

dev.off()
# with mean-centered data, I need a red/blue color scale ! (always reversed)
pheatmap(data_matrix, color = colorRampPalette(rev(brewer.pal(n = 9, name = "RdBu")))(100), 
         cluster_rows = FALSE, cluster_cols = FALSE, border_color = "grey60", angle_col = 45,
         annotation_col = condition_annotation[, c(1,2,3)],
         annotation_row = metabolite_annotation[1], 
         annotation_colors = ann_colors,
         show_rownames = T, fontsize_row = 7, show_colnames = T, fontsize_col = 6,
         gaps_row = c(13,21,41,46,47,48),
         gaps_col = c(2,8,14,20,26,32),
         legend=T, cellwidth = 10, 
         cellheight = 6, fontsize=8)

# export
pheatmap(data_matrix, color = colorRampPalette(rev(brewer.pal(n = 9, name = "RdBu")))(100), 
         cluster_rows = FALSE, cluster_cols = FALSE, border_color = "grey60", angle_col = 45,
         annotation_col = condition_annotation[, c(1,2,3)],
         annotation_row = metabolite_annotation[1], 
         annotation_colors = ann_colors,
         show_rownames = T, fontsize_row = 7, show_colnames = T, fontsize_col = 6,
         gaps_row = c(13,21,41,46,47,48),
         gaps_col = c(2,8,14,20,26,32),
         legend=T, cellwidth = 10, 
         cellheight = 6, fontsize=8,
         filename= "heatmap Fig4.jpg")

################## heatmap wuith log2 and mc data, with discrete colors#################
# so, I need to adjust the max and min color values to make some relatively minor changes more evident
# I need to impute colors manually using breaks !

# first, let's look at the colors in RdBu
display.brewer.pal(n = 11, name = 'RdBu')
# and get the hexadecimal code
brewer.pal(n = 11, name = "RdBu")

#I define the intervals (breaks) for colors according to the interval of the values in the heatmap
# MIN is -6.044
# MAX is +6.5199
# let's see if with these breaks it works nice
breaks <- c(-6.044, -3, -1.5, -0.75, -0.15, 0, 0.15, 0.75, 1.5, 3, 6.5199)

# the number of colors is equal to the number of values in breaks-1
colors <- c("#053061","#2166AC","#4393C3","#92C5DE","#D1E5F0","#FDDBC7","#F4A582","#D6604D","#B2182B","#67001F")

pheatmap(data_matrix_log2mc, color = colors, breaks=breaks, 
         cluster_rows = FALSE, cluster_cols = FALSE, border_color = "grey60", angle_col = 45,
         annotation_col = condition_annotation[, c(1,2,3)],
         annotation_row = metabolite_annotation[1], 
         annotation_colors = ann_colors,
         show_rownames = T, fontsize_row = 7, show_colnames = T, fontsize_col = 6,
         gaps_row = c(13,21,41,46,47,48),
         gaps_col = c(2,8,14,20,26,32),
         legend=T, cellwidth = 10, 
         cellheight = 6, fontsize=8)

# in case I can add legend breaks, but it is not necessary
legend_breaks = c(-6, -3, -1.5, 0, 1.5, 3, 6)

# to export the image file

pheatmap(data_matrix_log2mc, color = colors, breaks=breaks, 
         cluster_rows = FALSE, cluster_cols = FALSE, border_color = "grey60", angle_col = 45,
         annotation_col = condition_annotation[, c(1,2,3)],
         annotation_row = metabolite_annotation[1], 
         annotation_colors = ann_colors,
         show_rownames = T, fontsize_row = 7, show_colnames = T, fontsize_col = 6,
         gaps_row = c(13,21,41,46,47,48),
         gaps_col = c(2,8,14,20,26,32),
         legend=T, cellwidth = 10, 
         cellheight = 6, fontsize=8,
         filename="Fig 4_log2 mean centered.jpg")
dev.off()




















