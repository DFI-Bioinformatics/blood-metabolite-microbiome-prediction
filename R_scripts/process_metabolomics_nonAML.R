
library(tidyverse)
library(here)
library(data.table)


path = '/Volumes/dfi-clinical/data/HMMF/2023_HMMF_Compiled/fromAshley/qual/Bile-acids/'
files = list.files(path, pattern = '*.csv')
files = files[!(files %in% c('20210310_BileAcid_CLIN021_qual.csv'))]

temp_list = list()
for(file in files)
{
  temp_list[[file]] = fread(file.path(path, file), check.names = F) |>
   filter(sampleid != '') |> 
   pivot_longer(!sampleid, names_to = 'compound', values_to = 'value') |> 
   mutate(sample_id = gsub('__[a-z]+$', '', sampleid)) |> 
   mutate(sample_id = gsub('.*CLIN[0-9]+__', '', sample_id)) |>  
   mutate(compound = tolower(compound))
}

file = '20210310_BileAcid_CLIN021_qual.csv'
temp_list[[file]] = fread(file.path(path, file), check.names = F) |>
  rename(sampleid = `Sample Name`) |> 
  filter(sampleid != '') |> 
  pivot_longer(!sampleid, names_to = 'compound', values_to = 'value') |> 
  mutate(sample_id = gsub('__[a-z]+$', '', sampleid)) |> 
  mutate(sample_id = gsub('.*CLIN[0-9]+__', '', sample_id)) |>  
  mutate(compound = tolower(compound)) |> 
  mutate(compound = gsub('^[0-9]+_', '', compound)) |> 
  mutate(compound = gsub('_[a-z]$', '', compound))



df = do.call(rbind, temp_list)

unique(df$sample_id) |> sort() |> view()
