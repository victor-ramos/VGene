rv = reactiveValues(tsv.file = "", done = 0, folder.name = "", databases.folder = "/home/developer/GitHub/VGene/database/" )
# rv = reactiveValues(tsv.file = "", done = 0, folder.name = "", databases.folder = "~/GitHub/VGene/database/" )

rename.files = function (x, folder.name){
    file.copy(from = x[4], to = paste0(folder.name, x[1]))
}

inputDataReactive <- reactive({
    if (!is.null(input$tsv_file)) {
        return(TRUE)
    }
})

inputDataReactiveCheckBox <- reactive({
    
    if ( input$execution_mode == 'new_file_mode' ) {
    
        if (!is.null(input$databases) & length(input$databases) >= 1 ) {
            return(TRUE)
        }
            
    } else {
        if (!is.null(input$databases) & length(input$databases) > 1 ) {
            return(TRUE)
        }    
    }
    
    
})

output$fileUploaded <- reactive({
    return(!is.null(inputDataReactiveCheckBox()))
})

outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)

observe({
    if (rv$done == 1 ) {
        plots = list.files( rv$folder.name, pattern = "*.svg" ,full.names = T)
        # plots = c("/tmp/v_gene_freq-2021-11-02_18_36_37/IgG_frequency_plot.svg",
        #           "/tmp/v_gene_freq-2021-11-02_18_36_37/IgK_frequency_plot.svg",
        #           "/tmp/v_gene_freq-2021-11-02_18_36_37/IgL_frequency_plot.svg")
        
        output$data_panel_teste = renderUI({
            
            do.call(tabBox, c(id = "tabset1",
                              lapply(plots, function(x) {
                                  tabPanel( paste0( gsub("(\\S+)_frequency.*","\\1", basename(x)) ),
                                            shinydashboard::box(style = 'info',height = 800,
                                                value = paste0("panel_",gsub("(\\S+)_frequency.*","\\1", basename(x))  ),
                                                imageOutput(paste0("img_",gsub("(\\S+)_frequency.*","\\1", basename(x)) ))
                                            )
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
    if (rv$done == 1 | rv$done == "error") {
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

observe({
    
    if ( input$execution_mode == 'database_mode' ) {
        max = 3
    } else if ( input$execution_mode == 'new_file_mode' ) {
        max = 2
    }
    
    if(length(input$databases) > max){
        updateCheckboxGroupInput(session, "databases", selected= tail(input$databases,max))
    }
    
})

################################################################################

get.barplot.only = function ( x.freq.filtered, order.bars.by = "", sig.diffences.only = sig.diffences.only ) {
    
    x.freq.filtered = as.data.frame(x.freq.filtered)
    
    plots.list = list()
    
    color = c(rgb(221,129,65, maxColorValue = 255), rgb(82,113,177, maxColorValue = 255), rgb(110, 189, 169, maxColorValue = 255))
    names(color) = unique(x.freq.filtered$IDENT)
    
    for (chain in unique(x.freq.filtered$ISOTYPE)) {
        
        subset.genes = subset(x.freq.filtered, ISOTYPE == chain) %>% 
            # group_by(V_CALL) %>% 
            # tally() %>%
            # filter(n == length(unique( x.freq.filtered$IDENT )) ) %>%
            pull(V_CALL)
        
        if (length(subset.genes) != 0 ) {
            
            subset.data = x.freq.filtered %>% filter(ISOTYPE == chain, V_CALL %in% subset.genes )
            t = subset.data %>% arrange(freq, IDENT)
            
            # top.20.genes = (t %>% filter(IDENT == order.bars.by) %>% arrange(desc(freq)) %>% pull(V_CALL))[1:20]
            top.20.genes = (t %>% filter(IDENT == order.bars.by) %>% arrange(desc(freq)) %>% pull(V_CALL))
            t = subset(t, V_CALL %in% top.20.genes)
            
            t$V_CALL = factor(t$V_CALL, levels = unique( subset(subset.data, IDENT == order.bars.by) %>% arrange(freq) %>% pull(V_CALL)))
            t$IDENT = factor(t$IDENT, levels =  unique(t$IDENT) )
            
            subset.data.p.val = sig.diffences.only[[chain]]
            subset.data.p.val = subset(subset.data.p.val, V_CALL %in% as.character(t$V_CALL))
            subset.data.p.val$V_CALL = factor(subset.data.p.val$V_CALL, levels = subset(t, IDENT == order.bars.by) %>% pull(V_CALL))
            
            colors = c("#000000", "#FF0000", "#0000FF")
            names(colors) = as.character( unique( subset.data.p.val$binomial_test ) )
            
            plots.list[[chain]] = ggplot(data = t ,aes(x = V_CALL, y = freq) ) +
                geom_bar(aes(fill=IDENT), stat = "identity", position=position_dodge() ) + 
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
    
    return( plots.list )
    
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

generate.freq.plots = function( tsv.file.uploaded, selected.databases, execution.mode ) {
    
    tsv.obj = NULL
    
    check.header = function( tsv.obj ) {
        
        check = unique(  c("CDR3_AA", "V_CALL","ISOTYPE") %in% colnames(tsv.obj))
        
        if ( length(check) == 1 ) {
            
            if (check == TRUE) {
                
                tsv.obj$ISOTYPE = toupper( tsv.obj$ISOTYPE )
                check.isotypes = unique( unique(tsv.obj$ISOTYPE) %in% c("IGG","IGM","IGA", "IGH", "IGL", "IGK") )
                
                if ( length(check.isotypes) == 1) {
                    return( TRUE )
                } else {
                    wrong.isotype = paste0( unique(tsv.obj$ISOTYPE)[ !unique(tsv.obj$ISOTYPE) %in% c("IGG","IGM","IGA", "IGH", "IGL", "IGK") ], collapse = ", " )
                    rv$done = "error"
                    shinyalert("Unexpected isotype specified", paste0("Wrong isotype(s): ", wrong.isotype), type = "error")
                    return(FALSE)
                }
                
                
                return( TRUE )    
            } else {
                return( FALSE )
            }
            
        } else {
            rv$done = "error"
            shinyalert("Unexpected header", "Columns should be V_CALL, cdr3_aa and Isotype", type = "error")
            return( FALSE )
        }
        
    }
    
    binomial.test.parallel = function ( v.gene, ig.counts.per.group, isotype, total.per.group ) {
        
        library( tidyverse )
        
        df.result = NULL
        
        ig.counts.per.group.as.df = as.data.frame( ig.counts.per.group )
        
        ig.counts.per.group.as.df.isotype = ig.counts.per.group %>% filter( ISOTYPE == isotype )
        
        groups.count = ig.counts.per.group.as.df.isotype %>% filter( V_CALL == v.gene ) %>% pull(IDENT) %>% length()
        
        ig.counts.per.group %>% group_by(IDENT) %>% tally()
        
        if ( groups.count >= 2 ) {
            
            groups.v.gene = ig.counts.per.group.as.df.isotype %>% filter( V_CALL == v.gene ) %>% pull( IDENT )
            groups.comb = combn( groups.v.gene, 2 )
            
            for (i in 1:ncol(groups.comb)) {
                
                if ( total.per.group %>% filter(IDENT ==  groups.comb[1,i] ) %>% pull(n) > total.per.group %>% filter(IDENT ==  groups.comb[2,i] ) %>% pull(n) ) {
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
                
                v.gene.repertoire.count = ig.counts.per.group.as.df.isotype %>% filter( V_CALL == v.gene, IDENT == group.1) %>% pull(n)
                total.v.seq.repertoire = ig.counts.per.group.as.df %>% filter(ISOTYPE == isotype, IDENT == group.1) %>% pull(n) %>% sum()
                v.gene.srp.count = ig.counts.per.group.as.df.isotype %>% filter( V_CALL == v.gene, IDENT == group.2) %>% pull(n)
                total.v.seq.srp = ig.counts.per.group.as.df %>% filter(ISOTYPE == isotype, IDENT == group.2) %>% pull(n) %>% sum()
                
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
    
    if ( execution.mode == 'new_file_mode' ) {
        
        tsv.obj = read.delim( tsv.file.uploaded, header = T )
        colnames(tsv.obj) = toupper(colnames(tsv.obj))
        
        if ( check.header( tsv.obj = tsv.obj ) ) {
            
            tsv.obj$ISOTYPE = toupper( tsv.obj$ISOTYPE )
            tsv.obj$V_CALL = gsub("(^IG\\S+)\\*.*","\\1", tsv.obj$V_CALL)
            tsv.obj$V_CALL = gsub("(^IG\\S+)D","\\1", tsv.obj$V_CALL)
            
            tsv.obj = tsv.obj %>% mutate( IDENT = "uploaded_file") %>%
                filter(CDR3_AA != "")
            
            tsv.obj = tsv.obj %>% dplyr::mutate( ISOTYPE = case_when( grepl("IGA|IGG|IGM", ISOTYPE) ~ "IGH",
                                                                      !grepl("IGA|IGG|IGM", ISOTYPE) ~ ISOTYPE ) ) %>%
                group_by(V_CALL, ISOTYPE, IDENT) %>%
                tally() %>% 
                group_by(IDENT, ISOTYPE) %>%
                mutate(freq = (n / sum(n)) * 100 )
            
        }
        
        order.bars.by = "uploaded_file"
        
    }
    
    freq.files.list = lapply(selected.databases, function(x){
        freq.file = read.table( paste0( rv$databases.folder, x, ".txt"), header = T )
        colnames(freq.file)[1:3] = toupper(colnames(freq.file))[1:3]
        
        return(freq.file)
    } )
    freq.files = do.call("rbind", freq.files.list)
    
    tsv.modif = as.data.frame(rbind(tsv.obj, freq.files)) %>% mutate(ISOTYPE = toupper(ISOTYPE))
    tsv.modif = tsv.modif %>% dplyr::mutate( ISOTYPE = case_when( grepl("IGA|IGG|IGM", ISOTYPE) ~ "IGH",
                                                                  !grepl("IGA|IGG|IGM", ISOTYPE) ~ ISOTYPE ) )
    
    if ( execution.mode == 'database_mode' ) {
        
        if ("Repertoire_Heavy_and_Light_healthy_database_freq" %in% selected.databases ) {
            order.bars.by = grep("Conv|mRNA|TBEV|vax", tsv.modif$IDENT, value = T, ignore.case = T)[1]
        } else {
            order.bars.by = unique(tsv.modif$IDENT)[1]    
        }
        
        
        
    }
    
    total.per.group = tsv.modif %>% group_by(IDENT) %>% summarise(n = sum(n))
    
    v.genes.repertoire = tsv.modif %>% group_by(V_CALL) %>% 
        tally() %>% 
        filter(n >= length(unique(tsv.modif$IDENT)) -1 ) %>%
        pull(V_CALL) %>% 
        unique()
    
    
    ig.counts.per.group = tsv.modif %>% group_by( V_CALL, ISOTYPE, IDENT ) %>% summarise(n = sum(n))
    result.list.binomial = list()
    for (isotype in unique(ig.counts.per.group$ISOTYPE)) {
        
        cl = parallel::makeCluster(1, setup_strategy = "sequential")
        registerDoParallel(cl)
        binom.test.tbl = foreach( i = 1:length(v.genes.repertoire), .combine = rbind ) %dopar%{ 
            binomial.test.parallel( v.gene = v.genes.repertoire[i], ig.counts.per.group = ig.counts.per.group, isotype = isotype, total.per.group = total.per.group )
        }
        stopCluster(cl)  
        
        result.list.binomial[[ isotype ]] = binom.test.tbl
        
    }
    
    sig.diffences.only = parallel::mclapply(result.list.binomial, correct.p.value.and.select.sig, mc.cores = length(result.list.binomial))
    
    x.filter = tsv.modif %>% filter( V_CALL %in% v.genes.repertoire )
    
    plots.list = get.barplot.only( x.freq.filtered = x.filter, sig.diffences.only = sig.diffences.only, order.bars.by = order.bars.by )
    
    rv$done = 1
    
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
    if ( input$execution_mode == "new_file_mode" ) {
        apply(tsv.file, 1, rename.files, folder.name)    
    }
    
    rv$tsv.file = list.files(path = folder.name ,pattern = "*.tsv", all.files = T, full.names = T, recursive = T )
    
    selected.databases = input$databases
    
    plots.list = generate.freq.plots( tsv.file.uploaded = rv$tsv.file, selected.databases, execution.mode = input$execution_mode )
    
    if (rv$done == 1) {
    
        lapply(names(plots.list), function(x){
            svglite::svglite(filename=paste(dirname(folder.name),"/",x,"_frequency_plot.svg",sep=""))
            print(plots.list[[x]])
            dev.off()
        })
        
        shinyalert("Success!", "Click on the images to download them", type = "success")
              
    }
    
})
