fix.constant.call = function ( x ) {
  
  light.chains.genes = c("IGK", "IGL")
  
  for (chain in light.chains.genes) {
    x[ grep( chain,  x$V_Gene), "Isotype" ] = gsub("IG","Ig", chain)
  }
  
  return(x)
  
}

###########################################################################################################################

all.files = list.files( "~/GitHub/VGene/database", pattern = "*.txt", full.names = T)

for (file in all.files) {

  x = read.delim(file, header = FALSE)
  
  x = x %>% mutate_all(as.character)
  colnames(x) = c("V_Gene", "CDR3_AA", "Isotype", "Ident")
  
  # x = subset(x, !(Isotype %in% unique(x$Ident)) )
  # x = subset(x, Isotype %in% c("IgG","IgL","IgK") )
  
  x$Isotype = gsub("IGHG\\d+{1}", "IgG", x$Isotype )
  x$Isotype = gsub("IGH(\\w+{1})", "Ig\\1", x$Isotype )
  x$Isotype = gsub("IG", "Ig", x$Isotype)
  # x = subset(x, CDR3_AA != "")
  x$V_Gene = gsub("(^IG\\S+)\\*.*","\\1", x$V_Gene)
  x$V_Gene = gsub("(^IG\\S+)D","\\1", x$V_Gene)
  
  x = fix.constant.call(x = x)
  x = x %>% dplyr::mutate( Isotype = case_when( grepl("IgA|IgG|IgM", Isotype) ~ "IgH", !grepl("IgA|IgG|IgM", Isotype) ~ Isotype ) )
  
  x.freq = x %>% group_by(V_Gene, Isotype, Ident) %>% tally() %>% group_by(Ident, Isotype) %>% mutate(freq = (n / sum(n)) * 100 )
  colnames(x.freq)[1] = "V_CALL"
  
  write.table(x.freq, paste0("~/GitHub/VGene/database/",gsub("\\.txt","_freq\\.txt",basename(file)) ), quote = FALSE,col.names = T, row.names = F )
    
}

