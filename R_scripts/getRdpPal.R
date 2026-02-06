getShades <- function(spdf){

  hexcol <- unique(spdf$color)

  if (nrow(spdf) > 3){
    resdf <- spdf %>%
      mutate(cols = rep(shades(hexcol, variation = 0.25),
                        length.out = nrow(spdf)
      )
      )
  } else {
    resdf <- spdf %>%
      mutate(cols = shades(hexcol, variation = 0.25, ncolor = nrow(spdf)))
  }

  return(resdf)
}

getRdpPal <- function(tax) {

  require(tidyverse)
  require(yingtools2)

  tax <- tax %>%
    ungroup()

  if (class(tax)[1] %in% c("phyloseq", "taxonomyTable")) {
    tax <- get.tax(tax.obj)
  }
  ranks <- c("Kingdom", "Phylum", "Class", "Order", "Family",
             "Genus")

  if (!all(ranks %in% names(tax))) {
    stop("Error: need to have taxon levels: Kingdom, Phylum, Class, Order, Family, Genus, Species")
  }

  tax.dict <- tax %>%
    dplyr::select(all_of(ranks)) %>%
    distinct()

  # set all color to gray as base
  tax.dict <- tax.dict %>%
    mutate(color = rep(shades("gray", variation = 0.25),
                       length.out = nrow(tax.dict)))

# color for each level ------------------------------------------------------------
  phypal <- tibble(Phylum = c("Proteobacteria",
                              "Thermodesulfobacteriota",
                              "Pseudomonadota",
                              "Actinobacteria",
                              "Actinomycetota",
                              "Bacteroidetes",
                              "Bacteroidota"),
                   phycol = c("red","red","red",
                              "#A77097","#A77097","#51AB9B","#51AB9B"))
  ordpal <- tibble(Order = c("Clostridiales"),
                   ordcol = c("#9C854E"))
  fampal <- tibble(Family = c("Lachnospiraceae","Ruminococcaceae","Oscillospiraceae","Erysipelotrichaceae",
                              "Lactobacillaceae"),
                   famcol = c("#EC9B96","#9AAE73","#9AAE73","orange","#3b51a3"))
  genpal <- tibble(Genus = c("Enterococcus","Streptococcus","Staphylococcus",
                             "Lactobacillus"),
                   gencol = c("#129246","#9FB846","#f1eb25", "#3b51a3"))

  tax.split <- tax.dict %>%
    left_join(phypal) %>%
    left_join(ordpal) %>%
    left_join(fampal) %>%
    # ambiguous genus match
    mutate(gencol = case_when(
      grepl("Enterococcus$", Genus) ~ "#129246",
      grepl("Streptococcus$", Genus) ~ "#9FB846",
      grepl("Staphylococcus$", Genus) ~ "#f1eb25",
      TRUE ~ NA_character_
    )) %>%
    # left_join(genpal) %>%
    mutate(color = case_when(
      !is.na(gencol) ~ gencol,
      !is.na(famcol) ~ famcol,
      !is.na(ordcol) ~ ordcol,
      !is.na(phycol) ~ phycol,
      TRUE ~ color)
      ) %>%
    dplyr::select(Kingdom:Genus, color) %>%
    group_split(color)

    tax.color <- bind_rows(lapply(tax.split,getShades))
    tax.palette <- structure(tax.color$cols, names = as.character(tax.color$Genus))
    return(tax.palette)

}


generate_legend = function()
{
  require(cowplot)
  require(ggpubr)
  
  phypal <- tibble(name = c("Proteobacteria",
                              "Thermodesulfobacteriota",
                              "Pseudomonadota",
                              "Actinobacteria",
                              "Actinomycetota",
                              "Bacteroidetes",
                              "Bacteroidota"),
                   col = c("red","red","red",
                              "#A77097","#A77097","#51AB9B","#51AB9B"),
                   classification = "Phylum")
  
  
  ordpal <- tibble(name = c("Clostridiales"),
                   col = c("#9C854E"),
                   classification = "Order")
  
  fampal <- tibble(name = c("Lachnospiraceae","Ruminococcaceae","Oscillospiraceae","Erysipelotrichaceae",
                              "Lactobacillaceae"),
                   col = c("#EC9B96","#9AAE73","#9AAE73","orange","#3b51a3"),
                   classification = "Family")
  
  
  genpal <- tibble(name = c("Enterococcus","Streptococcus","Staphylococcus",
                             "Lactobacillus"),
                   col = c("#129246","#9FB846","#f1eb25", "#3b51a3"),
                   classification = "Genus")
  
  df_col = rbind(phypal, ordpal, fampal, genpal) %>% 
    mutate(name = paste0(classification, ":" , name)) %>% 
    data.frame()
  
  df_col[nrow(df_col) + 1, ] = c("Other", "gray", "Other")
  
  df_col %>% 
    mutate(classification = factor(classification, levels = c("Phylum", "Order", "Family", "Genus", "Other"))) %>% 
    arrange(classification)
  
  temp = structure(df_col$col, names = as.character(df_col$name))

  plot = df_col %>% 
    ggplot(aes(x = name, y = 1, fill = name)) +
    geom_col() +
    scale_fill_manual(values = temp, breaks = names(temp)) +
    labs(fill = "")
  
  legend = cowplot::get_legend(plot)
  as_ggplot(legend)
}



