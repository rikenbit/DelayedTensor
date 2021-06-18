library(magrittr)
library(dplyr)

# Download
paste(tempdir(), "human.txt.gz", sep="/") -> outfile1
paste(tempdir(), "mouse.txt.gz", sep="/") -> outfile2
download.file("https://ftp.ncbi.nlm.nih.gov/geo/series/GSE76nnn/GSE76381/suppl/GSE76381%5FEmbryoMoleculeCounts%2Ecef%2Etxt%2Egz", outfile1)
download.file("https://ftp.ncbi.nlm.nih.gov/geo/series/GSE76nnn/GSE76381/suppl/GSE76381%5FMouseEmbryoMoleculeCounts%2Ecef%2Etxt%2Egz", outfile2)

# Preprocessing
read.delim(outfile1, sep="\t", row.names=1, skip=4) %>%
	select(., -X) -> human
read.delim(outfile2, sep="\t", row.names=1, skip=6) %>%
	select(., -X) -> mouse

read.delim(outfile1, sep="\t", skip=1,  nrow=1, header=FALSE) %>%
	unlist %>%
		as.vector %>%
			.[-c(1,2)] %>%
				paste0("X", .) -> colnames(human)
read.delim(outfile2, sep="\t", skip=2, nrow=1, header=FALSE) %>%
	unlist %>%
		as.vector %>%
			.[-c(1,2)] %>%
				paste0("X", .) -> colnames(mouse)

toupper(rownames(mouse)) -> rownames(mouse)
intersect(rownames(human), rownames(mouse)) -> common.name

# reduce the number of genes
apply(human[common.name,], 1, var) -> var_human
apply(mouse[common.name,], 1, var) -> var_mouse
var_human + var_mouse -> var_both
var_both[rank(1/var_both) <= 500] -> high_var_genes
human[names(high_var_genes), ] %>% as.matrix -> human_mid_brain
mouse[names(high_var_genes), ] %>% as.matrix -> mouse_mid_brain

# Output
save(human_mid_brain, file="DelayedTensor/data/human_mid_brain.rda")
save(mouse_mid_brain, file="DelayedTensor/data/mouse_mid_brain.rda")
