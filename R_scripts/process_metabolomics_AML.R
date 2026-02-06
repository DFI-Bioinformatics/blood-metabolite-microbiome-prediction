
library(tidyverse)
library(here)
library(data.table)




path = '/Volumes/dfi-clinical/data/HMMF/AML_leukemia_HMMF_data/'
files <- list.files(
  path,
  pattern = '(?i)normalized.*\\.csv$',
  recursive = TRUE
)

files <- files[!grepl('/~\\$', files)]
files = files[!grepl('healthy', files)]



temp_list = list()
for(file in files)
{
  print(file)
  panel = case_when(
    grepl('Tryptophan', file, ignore.case = T) ~ 'Tryptophan',
    grepl('PFBBr', file, ignore.case = T) ~ 'SCFA',
    grepl('Bile', file, ignore.case = T) ~ 'Bile Acids',
    grepl('TMS', file, ignore.case = T) ~ 'TMS',
    T ~ NA)
  batch = stringr::str_extract(file, "[Cc][Ll][Ii][Nn][0-9]+")
  
  temp_list[[file]] = fread(file.path(path, file), check.names = F) |>
    filter(!grepl('Method Blank|NA indicates', sampleid)) |> 
    pivot_longer(!sampleid, names_to = 'compound', values_to = 'value') |> 
    mutate(sample_id = gsub('__[a-z]+$', '', sampleid)) |> 
    mutate(sample_id = gsub('.*CLIN[0-9]+__', '', sample_id)) |>  
    mutate(compound = tolower(compound)) |> 
    mutate(value = ifelse(is.na(value), 0, value)) |> 
    mutate(batch = batch, panel = panel, filename = gsub('.*\\/', '', file))
}


df_qual = do.call(rbind, temp_list) |> 
  mutate(compound = gsub('^[0-9]+_', '', compound)) |> 
  mutate(compound = gsub('_[a-z]+$', '', compound))



unique(df_qual$batch) |> view()
unique(df_qual$panel) |> view()
unique(df_qual$compound) |> view()
unique(df_qual$sample_id) |> view()
