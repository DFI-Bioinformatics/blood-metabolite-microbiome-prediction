library(tidyverse)
library(here)
library(data.table)

# Metaphlan

df_mp_raw = rbindlist(
  lapply(c('statq_0.20', 'statq_0.05'), function(statq) {
    rbindlist(
      lapply(
        list.files(here('data', 'metaphlan', statq, 'profiles'), pattern = 'mp4\\.txt$', full.names = T),
        \(f) fread(f)[, `:=`(
          seq_id = sub('_mp4\\.txt$', '', basename(f)),
          statq  = statq
        )]
      ),
      use.names = T,
      fill = T
    )
  }),
  use.names = T,
  fill = T
) |> 
  select(statq, seq_id, clade_name = `#clade_name`, ra = relative_abundance, reads = estimated_number_of_reads_from_the_clade, cov = coverage)


df_mp = df_mp_raw |> 
  filter(
    grepl('k__Bacteria', clade_name),
    grepl('s__', clade_name),
    !grepl('t__', clade_name)
  ) |> 
  separate(clade_name, into = c('kingdom', 'phylum', 'class', 'order', 'family', 'genus', 'species'), sep = '\\|') |> 
  mutate(
    kingdom = gsub('k__', '', kingdom),
    phylum = gsub('p__', '', phylum),
    class = gsub('c__', '', class),
    order = gsub('o__', '', order),
    family = gsub('f__', '', family),
    genus = gsub('g__', '', genus),
    species = gsub('s__', '', species)
  ) |> 
  select(statq, seq_id, kingdom, phylum, class, order, family, genus, species, ra_raw = ra) |> 
  group_by(statq, seq_id) |> 
  mutate(ra = ra_raw/sum(ra_raw)) |> 
  ungroup() |> 
  distinct()

df_mp |> 
  group_by(statq, seq_id) |> 
  summarize(sum = sum(ra)) |> view()



# MICU

df_meta_micu = read.csv(here('data', 'df_meta_micu.csv'))

df_mp_raw_micu = rbindlist(
  lapply(c('statq_0.20', 'statq_0.05'), function(statq) {
    rbindlist(
      lapply(
        list.files(here('data', 'metaphlan_micu', statq, 'profiles'), pattern = 'mp4\\.txt$', full.names = T),
        \(f) fread(f)[, `:=`(
          seq_id = sub('_mp4\\.txt$', '', basename(f)),
          statq  = statq
        )]
      ),
      use.names = T,
      fill = T
    )
  }),
  use.names = T,
  fill = T
) |> 
  select(statq, seq_id, clade_name = `#clade_name`, ra = relative_abundance, 
         reads = estimated_number_of_reads_from_the_clade, cov = coverage)


df_mp_micu = df_mp_raw_micu |> 
  filter(seq_id %in% df_meta_micu$seq_id) |> 
  filter(
    grepl('k__Bacteria', clade_name),
    grepl('s__', clade_name),
    !grepl('t__', clade_name)
  ) |> 
  separate(clade_name, into = c('kingdom', 'phylum', 'class', 'order', 'family', 'genus', 'species'), sep = '\\|') |> 
  mutate(
    kingdom = gsub('k__', '', kingdom),
    phylum = gsub('p__', '', phylum),
    class = gsub('c__', '', class),
    order = gsub('o__', '', order),
    family = gsub('f__', '', family),
    genus = gsub('g__', '', genus),
    species = gsub('s__', '', species)
  ) |> 
  select(statq, seq_id, kingdom, phylum, class, order, family, genus, species, ra_raw = ra) |> 
  group_by(statq, seq_id) |> 
  mutate(ra = ra_raw/sum(ra_raw)) |> 
  ungroup() |> 
  distinct()

df_mp |> 
  group_by(statq, seq_id) |> 
  summarize(sum = sum(ra)) |> view()


# MICU

df_mp_raw_hd = rbindlist(
  lapply(c('statq_0.20', 'statq_0.05'), function(statq) {
    rbindlist(
      lapply(
        list.files(here('data', 'metaphlan_hd', statq, 'profiles'), pattern = 'mp4\\.txt$', full.names = T),
        \(f) fread(f)[, `:=`(
          seq_id = sub('_mp4\\.txt$', '', basename(f)),
          statq  = statq
        )]
      ),
      use.names = T,
      fill = T
    )
  }),
  use.names = T,
  fill = T
) |> 
  select(statq, seq_id, clade_name = `#clade_name`, ra = relative_abundance, 
         reads = estimated_number_of_reads_from_the_clade, cov = coverage)


df_mp_hd = df_mp_raw_hd |> 
  filter(
    grepl('k__Bacteria', clade_name),
    grepl('s__', clade_name),
    !grepl('t__', clade_name)
  ) |> 
  separate(clade_name, into = c('kingdom', 'phylum', 'class', 'order', 'family', 'genus', 'species'), sep = '\\|') |> 
  mutate(
    kingdom = gsub('k__', '', kingdom),
    phylum = gsub('p__', '', phylum),
    class = gsub('c__', '', class),
    order = gsub('o__', '', order),
    family = gsub('f__', '', family),
    genus = gsub('g__', '', genus),
    species = gsub('s__', '', species)
  ) |> 
  select(statq, seq_id, kingdom, phylum, class, order, family, genus, species, ra_raw = ra) |> 
  group_by(statq, seq_id) |> 
  mutate(ra = ra_raw/sum(ra_raw)) |> 
  ungroup() |> 
  distinct()

df_mp |> 
  group_by(statq, seq_id) |> 
  summarize(sum = sum(ra)) |> view()

write.csv(df_mp, here('data', 'df_mp.csv'), row.names = F)
write.csv(df_mp_micu, here('data', 'df_mp_micu.csv'), row.names = F)
write.csv(df_mp_hd, here('data', 'df_mp_hd.csv'), row.names = F)

