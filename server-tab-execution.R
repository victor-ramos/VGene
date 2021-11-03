

rv = reactiveValues(rv.transfer.handle = NULL, tsv.file = "", done = 0, folder.name = "")

rename.files = function (x, folder.name){
    file.copy(from = x[4], to = paste0(folder.name, x[1]))
}

inputDataReactive <- reactive({
    if (!is.null(input$tsv_file)) {
        return(TRUE)
    }
})

output$databasesPanel <- reactive({
    return(!is.null(inputDataReactive()))
})
outputOptions(output, 'databasesPanel', suspendWhenHidden=FALSE)

inputDataReactiveCheckBox <- reactive({
    if (!is.null(input$databases)) {
        return(TRUE)
    }
})

output$fileUploaded <- reactive({
    return(!is.null(inputDataReactiveCheckBox()))
})

outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)




observe({
    if (rv$done == 0) {
        # plots = list.files( rv$folder.name, pattern = "*.svg" ,full.names = T)
        plots = c("/tmp/v_gene_freq-2021-11-02_18_36_37/IgG_frequency_plot.svg",
                  "/tmp/v_gene_freq-2021-11-02_18_36_37/IgK_frequency_plot.svg",
                  "/tmp/v_gene_freq-2021-11-02_18_36_37/IgL_frequency_plot.svg")
        
        output$data_panel_teste = renderUI({
            
            do.call(tabBox, c(id = "tabset1",
                              lapply(plots, function(x) {
                                  tabPanel( paste0( gsub("(\\S+)_frequency.*","\\1", basename(x)) ),
                                            shinydashboard::box(
                                                value = paste0("panel_",gsub("(\\S+)_frequency.*","\\1", basename(x))  ),
                                                imageOutput(paste0("img_",gsub("(\\S+)_frequency.*","\\1", basename(x)) )))
                                  )
                              })))
        })
        
        lapply(plots, function(x){
            output[[ paste0("img_", gsub("(\\S+)_frequency.*","\\1", basename(x)) )  ]] = renderImage( {list(src = x, contentType = 'image/svg+xml' ) },deleteFile = FALSE)
        })
        
    } else {
        tags$div()
    }            
    
})

output$restart_button = renderUI({
    if (rv$done == 0) {
        shinydashboard::box(
            value = 'data_panel2',
            style = 'info',
            title = "New Analysis",
            width = NULL,
            
            actionButton("new_analysis","New Analysis",
                         style="color: #fff; background-color: #20639B; border-color: #173F5F")
        )
        
    } else {
        tags$div()
    }
})


observeEvent(input$new_analysis, {
    js$reload();
})

################################################################################

get.barplot.only = function ( x.freq.filtered, order.bars.by = "uploaded_file", sig.diffences.only = sig.diffences.only ) {
    
    x.freq.filtered = as.data.frame(x.freq.filtered)
    
    plots.list = list()
    
    color = c(rgb(221,129,65, maxColorValue = 255), rgb(82,113,177, maxColorValue = 255), rgb(110, 189, 169, maxColorValue = 255))
    names(color) = unique(x.freq.filtered$Ident)
    
    for (chain in unique(x.freq.filtered$Class)) {
        
        subset.genes = subset(x.freq.filtered, Class == chain) %>% group_by(V_CALL) %>% tally() %>% filter(n == length(unique(x.freq.filtered$Ident)) ) %>% pull(V_CALL)
        
        if (length(subset.genes) != 0 ) {
            
            subset.data = x.freq.filtered %>% filter(Class == chain, V_CALL %in% subset.genes )
            t = subset.data %>% arrange(freq, Ident)
            
            top.20.genes = (t %>% filter(Ident == order.bars.by) %>% arrange(desc(freq)) %>% pull(V_CALL))[1:20]
            t = subset(t, V_CALL %in% top.20.genes)
            
            t$V_CALL = factor(t$V_CALL, levels = unique( subset(subset.data, Ident == order.bars.by) %>% arrange(freq) %>% pull(V_CALL)))
            t$Ident = factor(t$Ident, levels =  unique(t$Ident) )
            
            subset.data.p.val = sig.diffences.only[[chain]]
            subset.data.p.val = subset(subset.data.p.val, V_CALL %in% as.character(t$V_CALL))
            subset.data.p.val$V_CALL = factor(subset.data.p.val$V_CALL, levels = subset(t, Ident == order.bars.by) %>% pull(V_CALL))
            
            colors = c("#000000", "#FF0000", "#0000FF")
            # names(colors) = c("1y_Vaccinees_vs_1y_Non-Vaccinees", "1y_Vaccinees_vs_Conv_1.3m", "1y_Non-Vaccinees_vs_Conv_1.3m")
            names(colors) = as.character( unique( subset.data.p.val$binomial_test ) )
            
            plots.list[[chain]] = ggplot(data = t ,aes(x = V_CALL, y = freq) ) +
                geom_bar(aes(fill=Ident), stat = "identity", position=position_dodge() ) + 
                scale_fill_manual(values = color, drop = FALSE) + 
                scale_color_manual(values = colors) +
                coord_flip(expand = FALSE, ylim = c(0, max(t$freq) + 0.5 )) +
                scale_y_continuous(position = "right") +
                ylab(paste0(chain, " - V Frequency (%)")) + 
                xlab("") +
                theme_classic() + 
                theme(panel.grid.major.x= element_line(colour = "grey", linetype = "dashed")) +
                stat_pvalue_manual(data = subset.data.p.val,
                                   color = "binomial_test",
                                   x = "V_CALL",
                                   bracket.size = 1, bracket.shorten = 0.1, remove.bracket = FALSE,step.increase = 0.05,tip.length = 0,coord.flip = TRUE,
                                   label = "star",
                                   y.position = "y",
                                   label.size = 3,
                                   position = position_dodge(width = 0.5))
        }
        
    }
    
    return(plots.list)
    
}

correct.p.value.and.select.sig = function ( binom.test.tbl ) {
    
    test.list = split(binom.test.tbl, binom.test.tbl$binomial_test)
    
    for (comparison in names(test.list)) {
        
        binom.test.tbl.aux = test.list[[comparison]]
        binom.test.tbl.aux$p_value_adj = p.adjust(binom.test.tbl.aux$p_value, method = "fdr")
        
        binom.test.tbl.aux = binom.test.tbl.aux %>% filter(p_value_adj < 0.05) %>% mutate(star = case_when( p_value_adj <= 0.0001 ~ "****",
                                                                                                            p_value_adj <= 0.001 ~ "***",
                                                                                                            p_value_adj <= 0.01 ~ "**",
                                                                                                            p_value_adj <= 0.05 ~ "*",
        ))
        
        test.list[[comparison]] = as_tibble(binom.test.tbl.aux)
    }
    
    binom.test.tbl.final <- do.call("rbind", test.list)
    
    return( binom.test.tbl.final )  
    
}

fix.constant.call = function ( x ) {
    
    light.chains.genes = c("IGK", "IGL")
    
    for (chain in light.chains.genes) {
        x[ grep( chain,  x$V_Gene), "Isotype" ] = gsub("IG","Ig", chain)
    }
    
    return(x)
    
}

generate.freq.plots = function( tsv.file.uploaded, databases ) {
    
    binomial.test.parallel = function ( v.gene, ig.counts.per.group, isotype, total.per.group ) {
        
        library( tidyverse )
        
        df.result = NULL
        
        ig.counts.per.group.as.df = as.data.frame( ig.counts.per.group )
        
        ig.counts.per.group.as.df.isotype = ig.counts.per.group %>% filter( Class == isotype )
        
        groups.count = ig.counts.per.group.as.df.isotype %>% filter( V_CALL == v.gene ) %>% pull(Ident) %>% length()
        
        ig.counts.per.group %>% group_by(Ident) %>% tally()
        
        if ( groups.count >= 2 ) {
            
            groups.v.gene = ig.counts.per.group.as.df.isotype %>% filter( V_CALL == v.gene ) %>% pull( Ident )
            groups.comb = combn( groups.v.gene, 2 )
            
            for (i in 1:ncol(groups.comb)) {
                
                if ( total.per.group %>% filter(Ident ==  groups.comb[1,i] ) %>% pull(n) > total.per.group %>% filter(Ident ==  groups.comb[2,i] ) %>% pull(n) ) {
                    group.2 = groups.comb[1,i] 
                    group.1 = groups.comb[2,i] 
                } else {
                    group.1 = groups.comb[1,i] 
                    group.2 = groups.comb[2,i] 
                }
                
                # if (groups.comb[1,i] == "database") {
                
                # } else {
                #   group.1 = groups.comb[1,i] 
                #   group.2 = groups.comb[2,i] 
                # }
                
                v.gene.repertoire.count = ig.counts.per.group.as.df.isotype %>% filter( V_CALL == v.gene, Ident == group.1) %>% pull(n)
                total.v.seq.repertoire = ig.counts.per.group.as.df %>% filter(Class == isotype, Ident == group.1) %>% pull(n) %>% sum()
                v.gene.srp.count = ig.counts.per.group.as.df.isotype %>% filter( V_CALL == v.gene, Ident == group.2) %>% pull(n)
                total.v.seq.srp = ig.counts.per.group.as.df %>% filter(Class == isotype, Ident == group.2) %>% pull(n) %>% sum()
                
                binom.test.result = binom.test(x = v.gene.repertoire.count, n = total.v.seq.repertoire, p = (v.gene.srp.count / total.v.seq.srp), alternative = "two.sided")
                
                binom.result.p = binom.test.result$p.value
                
                df.result.test = data.frame(V_CALL = v.gene,
                                            y = max((v.gene.repertoire.count / total.v.seq.repertoire),  (v.gene.srp.count / total.v.seq.srp)) * 100,
                                            group1 = group.1,
                                            group2 = group.2,
                                            p_value = binom.result.p,
                                            binomial_test = paste0(group.1, "_vs_", group.2),
                                            IG_Type = gsub("(^IG\\w{1}).*", "\\1", v.gene) )
                
                df.result = rbind(df.result, df.result.test)
            }
            
        }
        
        return(df.result)
        
    }
    
    # tsv.obj = read.delim( tsv.file.uploaded, header = T )
    # tsv.modif = tsv.obj %>% dplyr::select( V_CALL, cdr3_aa ) %>% 
    #     mutate(Class = case_when( grepl("IGH", V_CALL) ~ "HC",
    #                               grepl("IGK", V_CALL) ~ "IgK",
    #                               grepl("IGL", V_CALL) ~ "IgL"),
    #            Ident = "uploaded_file") %>%
    #     filter(cdr3_aa != "")
    #     
    
    tsv.modif = read.delim("~/Dropbox/Rockefeller/Analysis/Vinci/barplot_1y_vacc_and_non-vac_and_conv_1.3m/Repertoire_Heavy_and_Light_1y_and_1.3m.txt", header = F)
    colnames(tsv.modif) = c("V_CALL", "cdr3_aa", "Class", "Ident")
    tsv.modif = tsv.modif %>% filter(cdr3_aa != "", Ident != "") %>% mutate(Ident = gsub("1y_Vaccinees", "uploaded_file", Ident))
    
    
    total.uploaded = nrow(tsv.obj)
    filtered.out = total.uploaded - nrow(tsv.modif)
    
    total.per.group = tsv.modif %>% group_by(Ident) %>% tally()
    
    #### Selecting unique shared genes
    v.genes.repertoire = tsv.modif %>% group_by(V_CALL, Ident) %>% 
        tally() %>% 
        filter(n >= length(unique(tsv.modif$Ident))) %>%
        pull(V_CALL) %>% 
        unique()
    
    ig.counts.per.group = tsv.modif %>% group_by(V_CALL, Class, Ident) %>% tally()
    result.list.binomial = list()
    for (isotype in unique(ig.counts.per.group$Class)) {
        
        cl = parallel::makeCluster(1, setup_strategy = "sequential")
        registerDoParallel(cl)
        binom.test.tbl = foreach( i = 1:length(v.genes.repertoire), .combine = rbind ) %dopar%{ 
            binomial.test.parallel( v.gene = v.genes.repertoire[i], ig.counts.per.group = ig.counts.per.group, isotype = isotype, total.per.group = total.per.group )
        }
        stopCluster(cl)  
        
        result.list.binomial[[ isotype ]] = binom.test.tbl
        
    }
    
    sig.diffences.only = mclapply(result.list.binomial, correct.p.value.and.select.sig, mc.cores = length(result.list.binomial))
    
    x.filter = tsv.modif %>% filter( V_CALL %in% v.genes.repertoire )
    x.freq = tsv.modif %>% group_by(V_CALL, Class, Ident) %>% tally() %>% group_by(Ident, Class) %>% mutate(freq = (n / sum(n)) * 100 )
    
    plots.list = get.barplot.only( x.freq.filtered = x.freq, sig.diffences.only = sig.diffences.only )
    
    return( plots.list )
    
}

################################################################################

observeEvent(input$ssh_bt, {
    
    showNotification("The application execution has started. DO NOT close this window until the execution is done.", duration = 45, type = 'message')
    
    shinyjs::disable("tsv_file")
    shinyjs::disable("databases")
    shinyjs::disable("ssh_bt")
    
    tsv.file = input$tsv_file
    
    folder.name = paste0("/tmp/v_gene_freq-", gsub("[\\s+\\:]", "_", Sys.time(), perl = T), "/data/")
    rv$folder.name = dirname(folder.name)
    
    dir.create(folder.name, showWarnings = FALSE, recursive = TRUE)
    apply(tsv.file, 1, rename.files, folder.name)
    
    rv$tsv.file = list.files(path = folder.name ,pattern = "*.tsv", all.files = T, full.names = T, recursive = T )
    
    selected.databases = input$databases
    
    plots.list = generate.freq.plots( tsv.file.uploaded, selected.databases )
    
    lapply(names(plots.list), function(x){
        svglite::svglite(filename=paste(dirname(folder.name),"/",x,"_frequency_plot.svg",sep=""))
        print(plots.list[[x]])
        dev.off()
    })
    
    rv$done = 1
    
    
})