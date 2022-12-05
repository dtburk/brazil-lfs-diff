
#rstudioapi::openProject() #- write down in the Console to open project

# What this script does ---------------------------------------------------

# Reads in all PNAD and PNAD-C dictionaries and assesses the consistency of
# their structure


# Load packages -----------------------------------------------------------
library(dplyr)
library(purrr)
library(stringr)
library(readxl)
library(tidyr)
library(readr)



# Define file paths -------------------------------------------------------
# setwd("/pkg/ipumsi/country/brazil/surveys/inventory/labor_force_surveys/")
paths <- list(
  pnadc_annual_visit1_dict_dir = "Microdata-PNADC/Annual/Visita_1",
  pnadc_annual_visit5_dict_dir = "Microdata-PNADC/Annual/Visita_5",
  pnadc_quarterly_dir = "Microdata-PNADC/Quarter/Dicionario_e_input_20220224",
  pnad_dict_dir1 = "Microdata-PNAD", # year-specific dirs within this one
  pnad_dict_dir2 = "Microdata-PNAD/Dicion rios e input"
)


# Compile list of all dictionary file paths -------------------------------
all_dictionary_file_paths <- list(

  pnadc_annual_visit1 = tibble(
    series = "PNAD-C",
    visit_number = 1,
    visit_period = "annual",
    dict_path = list.files(
      paths$pnadc_annual_visit1_dict_dir,
      pattern = "^dicionario_",
      full.names = TRUE
    )
  ) %>%
    mutate(
      years = str_match(basename(dict_path), "microdados_(.+)_visita1")[, 2]
    ),

  pnadc_annual_visit5 = tibble(
    series = "PNAD-C",
    visit_number = 5,
    visit_period = "annual",
    dict_path = list.files(
      paths$pnadc_annual_visit5_dict_dir,
      pattern = "^dicionario_",
      full.names = TRUE
    )
  ) %>%
    mutate(
      years = str_match(basename(dict_path), "microdados_(.+)_visita5")[, 2]
    ),

  pnadc_quarterly = tibble(
    series = "PNAD-C",
    visit_period = "quarterly",
    dict_path = list.files(
      paths$pnadc_quarterly_dir,
      pattern = "^dicionario_",
      full.names = TRUE
    ),
    years = "2012-2019"
  ),

  pnad1 = tibble(
    series = "PNAD",
    dict_path = map(
      2001:2012,
      ~list.files(
        file.path(
          paths$pnad_dict_dir1,
          .x,
          ifelse(.x %in% c(2001, 2003:2009), "dicion rio", "Dicion rio")
        ),
        full.names = TRUE
      )
    ) %>%
      flatten_chr()
  ) %>%
    mutate(
      rectype = case_when(
        str_detect(basename(dict_path), "domic¡lios") ~ "H",
        str_detect(basename(dict_path), "pessoas") ~ "P"
      ),
      years = str_extract(basename(dict_path), "[0-9]{4}")
    ),

  pnad2 = tibble(
    series = "PNAD",
    dict_path = list.files(
      paths$pnad_dict_dir2,
      pattern = "^Dicion rio",
      full.names = TRUE
    )
  ) %>%
    mutate(
      rectype = case_when(
        str_detect(basename(dict_path), "domic¡lios") ~ "H",
        str_detect(basename(dict_path), "pessoas") ~ "P"
      ),
      years = str_extract(basename(dict_path), "[0-9]{4}")
    )

)

all_dictionary_file_paths <- bind_rows(all_dictionary_file_paths)


# Read in all dictionaries ------------------------------------------------
all_dictionaries_df <- pmap_dfr(
  all_dictionary_file_paths,
  function(series, visit_number, visit_period, dict_path, years, rectype) {
    suppressMessages(
      read_excel(dict_path, skip = 1, col_types = "text")
    ) %>%
      mutate(
        series = series,
        visit_number = visit_number,
        visit_period = visit_period,
        dict_path = dict_path,
        years = years,
        rectype = rectype
      )
  }
)



# Merge columns with name variations --------------------------------------

# bind_rows() just adds columns when there are column names that don't appear
# in other dictionaries, so column-name variations show up as separate columns

# Posição inicial is the same as Posição Inicial
all_dictionaries_df <- all_dictionaries_df %>%
  mutate(
    `Posição inicial` = if_else(
      is.na(`Posição inicial`) & !is.na(`Posição Inicial`),
      `Posição Inicial`,
      `Posição inicial`
    )
  ) %>%
  select(-`Posição Inicial`)

# Combine the two Código de variável columns
all_dictionaries_df <- all_dictionaries_df %>%
  mutate(
    `Código\nda\nvariável` = if_else(
      is.na(`Código\nda\nvariável`) & !is.na(`Código de variável`),
      `Código de variável`,
      `Código\nda\nvariável`
    )
  ) %>%
  select(-`Código de variável`) %>%
  rename(`Código de variável` = `Código\nda\nvariável`)


# Structure consistency checks -----------------

# Look at first row of every dictionary to make sure we can safely delete it
all_dictionaries_df %>%
  group_by(dict_path) %>%
  slice(1) %>%
  View()
# Yep, looks good to remove the first row of each dictionary
all_dictionaries_df <- all_dictionaries_df %>%
  group_by(dict_path) %>%
  filter(row_number() != 1) %>%
  ungroup()


# Fill variable-level info onto value-level rows ------------------------

# Can we grab the question-group headings and put them in another column?
all_dictionaries_df %>%
  filter(!is.na(`Posição inicial`)) %>%
  filter(is.na(as.numeric(`Posição inicial`))) %>%
  pull(`Posição inicial`) %>%
  unique()
# Those all look like question-group headings

# Confirm that all question-group rows are missing in the "Código de variável"
# column
all_dictionaries_df %>%
  filter(!is.na(`Posição inicial`)) %>%
  filter(is.na(as.numeric(`Posição inicial`))) %>%
  pull(`Código de variável`) %>%
  is.na() %>%
  all()

# So now we can move the question-group info to another column...
all_dictionaries_df <- all_dictionaries_df %>%
  mutate(
    question_group_info = if_else(
      !is.na(`Posição inicial`) & is.na(as.numeric(`Posição inicial`)),
      `Posição inicial`,
      NA_character_
    )
  ) %>%
  relocate(question_group_info)


# ...fill those question_group_info values downward...
all_dictionaries_df <- all_dictionaries_df %>%
  fill(question_group_info)

# ...and get rid of those question_group_info rows
all_dictionaries_df <- all_dictionaries_df %>%
  filter(is.na(`Posição inicial`) | !is.na(as.numeric(`Posição inicial`)))

# And finally, fill all variable-level info downward onto the nested value-level
# rows
all_dictionaries_df <- all_dictionaries_df %>%
  fill(
    c(
      `Posição inicial`, Tamanho, `Código de variável`, Quesito, `...5`, Período
    )
  )


# Rename a couple columns ---------------------------------------------------
all_dictionaries_df <- all_dictionaries_df %>%
  rename(VarLabel = `...5`, ValueLabel = `...7`)


## Karine starts here: ------------------------------------------------------

# Identifying rows with multiple values in a single row -----------------
df1 <- all_dictionaries_df %>% dplyr::add_count(VarLabel,Categorias) %>%
  filter(!is.na(Categorias) & is.na(as.numeric(Categorias))) %>%
  distinct(VarLabel, Categorias,n)



#Create column "Decim" (number of decimal numbers in the category)
df1$Decim <- NA
df1$Decim[df1$Categorias=="6 dígitos e 8 casas decimais"] <- 8
df1$Decim[df1$Categorias=="valor com 10 casas decimais"] <- 10
df1$Decim[df1$Categorias=="valor com 2 casas decimais"] <- 2
df1$Decim[df1$Categorias=="Valor com 10 casas decimais"] <- 10
df1$Decim[df1$Categorias=="Valor com 1 casa decimal"] <- 1
df1$Decim[df1$Categorias=="Valor com 2 casas decimais"] <- 2


# Create column "ipums_categories" to keep the original column "Categorias":
df1$ipums_categories <- df1$Categorias

#changing numeric Categorias to blank in ipums_categories
#checked manually the VarLabels relative to these categories
#(basically, find and replace)
df1$ipums_categories[df1$Categorias=="valor em reais"] <- NA
df1$ipums_categories[df1$Categorias=="Valor"] <- NA
df1$ipums_categories[df1$Categorias=="6 dígitos e 8 casas decimais"] <- NA
df1$ipums_categories[df1$Categorias=="1 a 4"] <- NA
df1$ipums_categories[df1$Categorias=="01 a 14"] <- NA
df1$ipums_categories[df1$Categorias=="001 a 999"] <- NA
df1$ipums_categories[df1$Categorias=="01 a 99"] <- NA
df1$ipums_categories[df1$Categorias=="01 a 30"] <- NA
df1$ipums_categories[df1$Categorias=="01 a 31"] <- NA
df1$ipums_categories[df1$Categorias==
                       "Ano de referência menos 130 até ano de referência"] <- NA
df1$ipums_categories[df1$Categorias==
                       "ano de referência - 130 a ano atual"] <- NA
df1$ipums_categories[df1$Categorias=="000 a 120"] <- NA
df1$ipums_categories[df1$Categorias=="0 a 120"] <- NA
df1$ipums_categories[df1$Categorias=="00 a 15"] <- NA
df1$ipums_categories[df1$Categorias=="01 a 11"] <- NA
df1$ipums_categories[df1$Categorias=="00 a 11"] <- NA
df1$ipums_categories[df1$Categorias=="1 a 5"] <- NA
df1$ipums_categories[df1$Categorias=="11 a 50"] <- NA
df1$ipums_categories[df1$Categorias=="01  a  30"] <- NA
df1$ipums_categories[df1$Categorias=="000.000.000.000 a 999.999.999.998"] <- NA
df1$ipums_categories[df1$Categorias=="999.999.999.999 - Ignorado"] <- NA
df1$ipums_categories[df1$Categorias=="999 – idade ignorada"] <- NA
df1$ipums_categories[df1$Categorias=="1 a 7"] <- NA
df1$ipums_categories[df1$Categorias=="1 a 3"] <- NA
df1$ipums_categories[df1$Categorias=="Não aplicável"] <- NA
df1$ipums_categories[df1$Categorias=="00 a 98"] <- NA
df1$ipums_categories[df1$Categorias=="99 - Ignorado"] <- NA
df1$ipums_categories[df1$Categorias=="00 a 11"] <- NA
df1$ipums_categories[df1$Categorias=="0 a 11"] <- NA
df1$ipums_categories[df1$Categorias=="0 a 11"] <- NA
df1$ipums_categories[df1$Categorias=="999.999.999.999  - Ignorado"] <- NA
df1$ipums_categories[df1$Categorias=="01 a 12"] <- NA
df1$ipums_categories[df1$Categorias=="1 a 120"] <- NA
df1$ipums_categories[df1$Categorias=="001 a 120"] <- NA
df1$ipums_categories[df1$Categorias=="00 a 30"] <- NA
df1$ipums_categories[df1$Categorias=="Pessoa ocupada na semana"] <- NA
df1$ipums_categories[df1$Categorias=="Com registro no quesito 8"] <- NA
df1$ipums_categories[df1$Categorias=="Com registro no  quesito 29"] <- NA
df1$ipums_categories[df1$Categorias=="Pessoa não ocupada na semana"] <- NA
df1$ipums_categories[df1$Categorias=="Com registro no quesito 70"] <- NA
df1$ipums_categories[df1$Categorias=="Com registro no quesito 69"] <- NA
df1$ipums_categories[df1$Categorias=="Em meses"] <- NA
df1$ipums_categories[df1$Categorias=="Em dias"] <- NA
df1$ipums_categories[df1$Categorias=="01 a 15"] <- NA
df1$ipums_categories[df1$Categorias=="01  a  15"] <- NA
df1$ipums_categories[df1$Categorias=="valor com 10 casas decimais"] <- NA
df1$ipums_categories[df1$Categorias=="valor com 2 casas decimais"] <- NA
df1$ipums_categories[df1$Categorias=="999 999 999 999"] <- NA
df1$ipums_categories[df1$Categorias=="1882 a 2002"] <- NA
df1$ipums_categories[df1$Categorias=="0 a 130"] <- NA
df1$ipums_categories[df1$Categorias=="02 a 98"] <- NA
df1$ipums_categories[df1$Categorias=="06 a 10"] <- NA
df1$ipums_categories[df1$Categorias=="1 a 10"] <- NA
df1$ipums_categories[df1$Categorias==
                       "00 – em caso de idade presumida ou estimada"] <- NA
df1$ipums_categories[df1$Categorias==
                       "20 – em caso de idade presumida ou estimada"] <- NA
df1$ipums_categories[df1$Categorias==
                       "0000 a  0098  – em caso de idade presumida ou estimada"] <- NA
df1$ipums_categories[df1$Categorias=="9999 - Ignorado"] <- NA
df1$ipums_categories[df1$Categorias=="0000 a 0098"] <- NA
df1$ipums_categories[df1$Categorias=="Quantidade"] <- NA
df1$ipums_categories[df1$Categorias=="01 a 98"] <- NA
df1$ipums_categories[df1$Categorias=="99 999 999 999"] <- NA
df1$ipums_categories[df1$Categorias=="9 999 999"] <- NA
df1$ipums_categories[df1$Categorias=="1 a 29"] <- NA
df1$ipums_categories[df1$Categorias=="04 a 98"] <- NA
df1$ipums_categories[df1$Categorias=="1 a 30"] <- NA
df1$ipums_categories[df1$Categorias=="Valor com 10 casas decimais"] <- NA
df1$ipums_categories[df1$Categorias=="Valor com 1 casa decimal"] <- NA
df1$ipums_categories[df1$Categorias=="Valor com 2 casas decimais"] <- NA
df1$ipums_categories[df1$Categorias=="1883 a 2004"] <- NA
df1$ipums_categories[df1$Categorias=="01 a 60"] <- NA
df1$ipums_categories[df1$Categorias=="1 a 60"] <- NA
df1$ipums_categories[df1$Categorias=="1895 a 2006"] <- NA
df1$ipums_categories[df1$Categorias=="1894 a 2005"] <- NA
df1$ipums_categories[df1$Categorias=="1893 a 2004"] <- NA
df1$ipums_categories[df1$Categorias=="1884 a 2005"] <- NA
df1$ipums_categories[df1$Categorias=="1885 a 2006"] <- NA
df1$ipums_categories[df1$Categorias=="1900 a 2007"] <- NA
df1$ipums_categories[df1$Categorias=="04 a 30"] <- NA
df1$ipums_categories[df1$Categorias=="1958 a 2007"] <- NA
df1$ipums_categories[df1$Categorias=="1961 a 2007"] <- NA
df1$ipums_categories[df1$Categorias=="15 a 98"] <- NA
df1$ipums_categories[df1$Categorias=="1970 a 2007"] <- NA
df1$ipums_categories[df1$Categorias=="1887 a 2008"] <- NA
df1$ipums_categories[df1$Categorias=="1897 a 2008"] <- NA
df1$ipums_categories[df1$Categorias=="00 a 12"] <- NA
df1$ipums_categories[df1$Categorias=="00 a 10"] <- NA
df1$ipums_categories[df1$Categorias=="01 a 04"] <- NA
df1$ipums_categories[df1$Categorias=="00 a 14"] <- NA
df1$ipums_categories[df1$Categorias=="00 a 52"] <- NA
df1$ipums_categories[df1$Categorias=="1891 a 2011"] <- NA
df1$ipums_categories[df1$Categorias=="1899 a 2010"] <- NA
df1$ipums_categories[df1$Categorias=="1892 a 2012"] <- NA
df1$ipums_categories[df1$Categorias=="1890 a 2010"] <- NA

# Expand categories ----------------------------------------------------

check<- df1%>%filter(!is.na(ipums_categories))

#create n duplicated rows (n = number of categories-1)
## Numero da familia -  1 a 9
nfamilia <- check %>% slice(rep(25, each=9))
nfamilia$ipums_categories <- rownames(nfamilia)

##Código da última Unidade da Federação ou de último país estrangeiro em que morou anteriormente
uf5030 <- check %>% slice(rep(c(24, 26, 27, 46, 47, 54, 55, 78), each=31))
uf5030$ipums_categories <- c("11", "12", "13", "14", "15", "16", "17", "21", "22",
                             "23", "24", "25", "26", "27", "28", "29", "31", "32",
                             "33", "35", "41", "42", "43", "50", "51", "52", "53",
                             "88", "98", "99", NA,"11", "12", "13", "14", "15", "16", "17", "21", "22",
                             "23", "24", "25", "26", "27", "28", "29", "31", "32",
                             "33", "35", "41", "42", "43", "50", "51", "52", "53",
                             "88", "98", "99", NA,"11", "12", "13", "14", "15", "16", "17", "21", "22",
                             "23", "24", "25", "26", "27", "28", "29", "31", "32",
                             "33", "35", "41", "42", "43", "50", "51", "52", "53",
                             "88", "98", "99", NA,"11", "12", "13", "14", "15", "16", "17", "21", "22",
                             "23", "24", "25", "26", "27", "28", "29", "31", "32",
                             "33", "35", "41", "42", "43", "50", "51", "52", "53",
                             "88", "98", "99", NA,"11", "12", "13", "14", "15", "16", "17", "21", "22",
                             "23", "24", "25", "26", "27", "28", "29", "31", "32",
                             "33", "35", "41", "42", "43", "50", "51", "52", "53",
                             "88", "98", "99", NA,"11", "12", "13", "14", "15", "16", "17", "21", "22",
                             "23", "24", "25", "26", "27", "28", "29", "31", "32",
                             "33", "35", "41", "42", "43", "50", "51", "52", "53",
                             "88", "98", "99", NA,"11", "12", "13", "14", "15", "16", "17", "21", "22",
                             "23", "24", "25", "26", "27", "28", "29", "31", "32",
                             "33", "35", "41", "42", "43", "50", "51", "52", "53",
                             "88", "98", "99", NA,"11", "12", "13", "14", "15", "16", "17", "21", "22",
                             "23", "24", "25", "26", "27", "28", "29", "31", "32",
                             "33", "35", "41", "42", "43", "50", "51", "52", "53",
                             "88", "98", "99", NA)

expanded <- rbind(nfamilia, uf5030)

# Merge these datasets to df1, then to all_dictionaries_df, keeping all values
#of df1 and expanded2

data1 <- merge(df1, expanded, by=c("VarLabel", "Categorias", "n",
                                   "ipums_categories", "Decim"), all=T)

data2 <- merge(data1, all_dictionaries_df, by=c("VarLabel", "Categorias"),
               all=T)

# Identifying other rows with multiple categories --------------------------------
data3 <- data2 %>%
  filter(!is.na(ipums_categories) &
           is.na(as.numeric(ipums_categories))) %>%
  distinct(VarLabel, ipums_categories)

# Subsetting rows with undesired values in ipums_categories: ---------------

data4 <- data2 %>%
  filter(VarLabel!= "Número da família" |ipums_categories!="1 a 9")


#create "path" columns to data9 containing the file path to the variables'
# values and values' labels
data4$code_file_path <- NA
data4$code_file_path[data4$ipums_categories==
                       "ver “Relação de Códigos de Atividades” em ANEXO  de Notas Metodológicas"] <-
  "~surveys/inventory/labor_force_surveys/Microdata-PNAD/Metodologia/Anexo III - Composi‡ֶo dos grupamentos de atividade.xls"
data4$ipums_categories[data4$Categorias==
                         "ver “Relação de Códigos de Atividades” em ANEXO  de Notas Metodológicas"] <-
  NA

data4$code_file_path[str_detect(data4$ipums_categories, "quesito 7")] <-
  "~surveys/inventory/labor_force_surveys/Microdata-PNAD/Metodologia/Anexo III - Composi‡ֶo dos grupamentos de atividade.xls"
data4$ipums_categories[str_detect(data4$Categorias, "quesito 7")] <- NA

data4$code_file_path[str_detect(data4$ipums_categories,"Grupamentos de Atividade")] <-
  "~surveys/inventory/labor_force_surveys/Microdata-PNAD/Metodologia/Anexo III - Composi‡ֶo dos grupamentos de atividade.xls"
data4$ipums_categories[str_detect(data4$Categorias,"Grupamentos de Atividade")] <- NA

data4$code_file_path[str_detect(data4$ipums_categories,"Grupamentos Ocupacionais")] <-
  "~surveys/inventory/labor_force_surveys/Microdata-PNAD/Metodologia/Anexo I - Composi‡ֶo dos grupamentos ocupacionais.doc"
data4$ipums_categories[str_detect(data4$Categorias,"Grupamentos Ocupacionais")] <- NA

data4$code_file_path[str_detect(data4$ipums_categories,"Códigos de Ocupação")] <-
  "~surveys/inventory/labor_force_surveys/Microdata-PNAD/Metodologia/Anexo I - Composi‡ֶo dos grupamentos ocupacionais.doc"
data4$ipums_categories[str_detect(data4$Categorias,"Códigos de Ocupação")] <- NA

data4$code_file_path[str_detect(data4$ipums_categories,"ANEXO VIIII")] <-
  "~surveys/inventory/labor_force_surveys/Microdata-PNAD/Metodologia/Anexo VIII - Tipos e modalidades de esporte e atividade f¡sica.xlsx"
data4$ipums_categories[str_detect(data4$Categorias,"ANEXO VIIII")] <- NA

data4$code_file_path[str_detect(data4$ipums_categories, "quesito 6")] <-
  "~surveys/inventory/labor_force_surveys/Microdata-PNAD/Metodologia/Anexo I - Composi‡ֶo dos grupamentos ocupacionais.doc"
data4$ipums_categories[str_detect(data4$Categorias, "quesito 6")] <- NA


data4$code_file_path[data4$VarLabel == "Código da ocupação (cargo ou função)"&
                       data4$ipums_categories == "código"&
                       data4$series == "PNAD-C"] <- "~surveys/inventory/labor_force_surveys/Microdata-PNADC/Annual/Estrutura_Ocupacao_COD.xls"
data4$ipums_categories[data4$VarLabel == "Código da ocupação (cargo ou função)"&
                         data4$ipums_categories == "código"&
                         data4$series == "PNAD-C"] <- NA

data4$code_file_path[data4$VarLabel == "Código da a principal tarefa que ... exerceu"&
                       data4$ipums_categories == "código"&
                       data4$series == "PNAD-C"] <- "~surveys/inventory/labor_force_surveys/Microdata-PNADC/Annual/Estrutura_Atividade_CNAE_Domiciliar_2_0.xls"
data4$ipums_categories[data4$VarLabel == "Código da a principal tarefa que ... exerceu"&
                         data4$ipums_categories == "código"&
                         data4$series == "PNAD-C"] <- NA

data4$code_file_path[data4$VarLabel == "Código da principal atividade da empresa, organização ou instituição para a (através da) qual ... exerceu essa tarefa"&
                       data4$ipums_categories == "código"&
                       data4$series == "PNAD-C"] <- "~surveys/inventory/labor_force_surveys/Microdata-PNADC/Annual/Estrutura_Atividade_CNAE_Domiciliar_2_0.xls"
data4$ipums_categories[data4$VarLabel == "Código da principal atividade da empresa, organização ou instituição para a (através da) qual ... exerceu essa tarefa"&
                         data4$ipums_categories == "código"&
                         data4$series == "PNAD-C"] <- NA

data4$code_file_path[data4$VarLabel == "Código da principal atividade desse negócio/empresa"&
                       data4$ipums_categories == "código"&
                       data4$series == "PNAD-C"] <- "~surveys/inventory/labor_force_surveys/Microdata-PNADC/Annual/Estrutura_Atividade_CNAE_Domiciliar_2_0.xls"
data4$ipums_categories[data4$VarLabel == "Código da principal atividade desse negócio/empresa"&
                         data4$ipums_categories == "código"&
                         data4$series == "PNAD-C"] <- NA

data4$code_file_path[data4$VarLabel == "Código da principal atividade que exerceu na semana de referência"&
                       data4$ipums_categories == "código"&
                       data4$series == "PNAD-C"] <- "~surveys/inventory/labor_force_surveys/Microdata-PNADC/Annual/Estrutura_Atividade_CNAE_Domiciliar_2_0.xls"
data4$ipums_categories[data4$VarLabel == "Código da principal atividade que exerceu na semana de referência"&
                         data4$ipums_categories == "código"&
                         data4$series == "PNAD-C"] <- NA


data4$code_file_path[data4$Categorias == "001 a 623"] <-"~surveys/inventory/labor_force_surveys/Microdata-PNAD/2007 2/Metodologia/Anexo VII - Rela‡Æo de c¢digos dos cursos t‚cnicos.xls"
data4$ipums_categories[data4$Categorias == "001 a 623"] <- NA

data4$code_file_path[data4$Categorias == "000 a 098"] <-  "~surveys/inventory/labor_force_surveys/Microdata-PNAD/2007 2/Metodologia/Anexo VIII - Rela‡ֶo de c¢digos dos cursos de gradua‡ֶo tecnol¢gica.xls"
data4$ipums_categories[data4$Categorias == "000 a 098"] <- NA

#changing values of ipums_categories to NA
data4$ipums_categories[str_detect(data4$VarLabel, "Entrevista")] <- NA
data4$ipums_categories[str_detect(data4$VarLabel, "entrevista")] <- NA
data4$ipums_categories[str_detect(data4$Categorias, "V5030")] <- NA
data4$ipums_categories[str_detect(data4$Categorias, "V0530")] <- NA
data4$ipums_categories[str_detect(data4$Categorias, "as 2 primeiras posições são o código da Unidade da Federação")] <- NA
data4$ipums_categories[str_detect(data4$Categorias, "As 2 primeiras posições são o código da Unidade da Federação")] <- NA
data4$ipums_categories[str_detect(data4$Categorias, "Só aplicável")] <- NA
data4$ipums_categories[str_detect(data4$Categorias, "Só aplicável")] <- NA
data4$ipums_categories[str_detect(data4$Categorias, "Data ordenada na forma")] <- NA
data4$ipums_categories[str_detect(data4$Categorias, "V  1141")] <- NA
data4$ipums_categories[str_detect(data4$Categorias, "V 1141")] <- NA
data4$ipums_categories[str_detect(data4$Categorias, "Número da unidade domiciliar")] <- NA
data4$ipums_categories[str_detect(data4$Categorias, "Igual ao peso da pessoa de referência da família")] <- NA
data4$ipums_categories[str_detect(data4$Categorias, "Igual ao peso da pessoa de referência do domicílio")] <- NA



# Check - Identifying rows with multiple categories ---------------------------
datax <- data4 %>%
  filter(!is.na(ipums_categories) &
           is.na(as.numeric(ipums_categories))) %>%
  distinct(VarLabel, ipums_categories, years, series, code_file_path)


# Subset files according to colunms: one file for code_values paths and another
#without it-------------------------------------------------------------------
table(data4$code_file_path)
values_path <- subset(data4, complete.cases(code_file_path))
ipums_values <- data4 %>% select(-code_file_path)


# Creating data path column-------------------------------------------------
microdata_structure_notes <- read_excel("microdata_structure_notes.xlsx",
                                        sheet = "Structure_summary", col_types = c("text", "text", "text", "text",
                                                                                   "numeric", "text", "numeric",
                                                                                   "numeric", "text", "text",
                                                                                   "text", "text", "text", "skip",
                                                                                   "skip", "skip", "skip"))

microdata_structure_notes <-  microdata_structure_notes %>% dplyr::rename(c(
  "series"="Collection name -",
  "data_path"="directory",
  "data.years"="year"))
microdata_structure_notes <-  microdata_structure_notes %>% filter(!series=="PME")
#microdata_structure_notes <-  microdata_structure_notes %>% select(-"M/Q/A",
#                                                                   -"month",
#                                                                   -"quarter")

# Expanding values in "years" that include more than a single year ------------
table(data4$years)

years.2012_2014 <- subset(data4, years=="2012_a_2014")
years.2012_2014 <- years.2012_2014[rep(c(1:nrow(years.2012_2014)), 3),]
years.2012_2014$dic.years <- rep(c(2012, 2013, 2014), 1165)

years.2012_2019 <- subset(data4, years=="2012-2019")
years.2012_2019 <- years.2012_2019[rep(c(1:nrow(years.2012_2019)), 8),]
years.2012_2019$dic.years <- rep(c(2012, 2013, 2014, 2015, 2016, 2017, 2018,
                                   2019), 1166)

years_dic <- rbind(years.2012_2014, years.2012_2019)

# Merging years_dic with data4 ---------------------------------------------
table(microdata_structure_notes$series)
table(data4$series)
microdata_structure_notes$series[microdata_structure_notes$series=="PNADC"] <-
  "PNAD-C"
table(microdata_structure_notes$series)
data5 <- merge(data4, years_dic, by=c("VarLabel", "Categorias", "n" ,
                                      "ipums_categories", "Decim",
                                      "question_group_info", "Posição inicial",
                                      "Tamanho", "Código de variável","Quesito",
                                      "ValueLabel", "Período", "series",
                                      "visit_number", "visit_period","dict_path",
                                      "years", "rectype","code_file_path"),
               all.x=T)



str(data5$dic.years)
str(data5$years)
data5$dic.years <- as.character(data5$dic.years)
data5$dic.years <- coalesce(data5$dic.years, data5$years)


#Merge data5 with microdata_structure_notes (data paths are here)-------------

#we have to include visit_period for PNAD-C
#renaming visit_period so it matches with the correct Folder name in the data path
data5$visit_period[data5$visit_period=="quarterly"] <- "Quarter"
data5$visit_period[data5$visit_period=="annual"] <- "Annual"

#renaming key variables:
names(data5)
data5 <- data5 %>% rename("data.years"="dic.years")


# Is data5 ready to merge with microdata_structure_notes? ----
data5 %>%
  filter(is.na(visit_period)) %>%
  distinct(series, data.years, rectype) %>%
  arrange(series, data.years, rectype)

data5 %>%
  filter(series == "PNAD") %>%
  pull(data.years) %>%
  unique() %>%
  sort()

microdata_structure_notes %>%
  filter(is.na(visit_period))

# Looks like all the PNAD surveys have visit_period "Annual" in
# microdata_structure_notes -- let's recode to that value in data5
data5 <- data5 %>%
  mutate(visit_period = if_else(series == "PNAD", "Annual", visit_period))

data6 <- data5 %>%
  mutate(data.years = as.numeric(data.years)) %>%
  left_join(
    microdata_structure_notes,
    by = c("series", "data.years", "visit_period", "visit_number", "rectype")
  )

# data6 has 80,131 rows, which is 27,984 more rows than data5. This is because
# microdata_structure_notes had an entry for each quarter for the quarterly
# PNAD-C, and there were 9,328 quarterly PNAD-C rows in data5.
# 9,328 * 3 == 27,984
# (multiplied by 3 because we end up with 4 copies of each quarterly PNAD-C row
# whereas we started out with just 1 copy of each)


#Save files------------------------------------------------------------------
#write.csv(values_path, "codes_paths_PNADandPNADC.csv")
#write.csv(ipums_values, "ipums_valuescode_PNADandPNADC.csv")
#write.csv(data6, "allinfo_ipumsvalues_dic&datapaths.csv")


#Read in microdata --------------------------------------------------------

names(data6)

all_data_files <- data6 %>%
  distinct(
    series, data.years, rectype, visit_number, visit_period, quarter, data_path,
    dat_name, dat_format
  )
# This yields 70 data files, which I think is right

# Check that all data files exist
all_data_files <- all_data_files %>%
  mutate(
    dat_name = str_replace(dat_name, "\\[YYYY\\]", as.character(data.years)),
    dat_name = str_replace(dat_name, "\\[N\\]", as.character(visit_number)),
    dat_name = str_replace(dat_name, "\\[0Q\\]", paste0("0", quarter)),
    data_path = str_remove(data_path, "/[0-9]{4}$"), # remove year from end of path
    data_path = case_when(
      # for 2015 PNAD the data files are in the top level folder
      series == "PNAD" & data.years == "2015" ~ data_path,
      # all other PNAD
      series == "PNAD" & data.years != "2015" ~
        file.path(data_path, data.years, "dados"),
      # PNAD-C annual
      series == "PNAD-C" & visit_period == "Annual" ~
        file.path(data_path, "Annual", paste0("Visita_", visit_number)),
      # PNAD-C quarterly
      series == "PNAD-C" & visit_period == "Quarter" ~
        file.path(data_path, "Quarter")
    ),
    file_path = file.path(data_path, dat_name),
    file_path = str_replace(file_path, "/Volumes", "/pkg"),
    # replace .TXT with .txt if file doesn't exist
    file_path = if_else(
      str_detect(file_path, "TXT$") & !file.exists(file_path),
      str_replace(file_path, "TXT$", "txt"),
      file_path
    ),
    # replace "dados" with "Dados" if file doesn't exist
    file_path = if_else(
      !file.exists(file_path),
      str_replace(file_path, "dados", "Dados"),
      file_path
    ),
    file_exists = file.exists(file_path)
  )

all_data_files_exist <- all_data_files %>%
  pull(file_exists) %>%
  all()

stopifnot(all_data_files_exist)

all_data_files <- all_data_files %>%
  select(
    series, data.years, visit_period, visit_number, quarter, rectype, file_path
  )

for (i in 1:nrow(all_data_files)) {

  print(i)

  data_file_info <- all_data_files %>%
    slice(i)

  data_file_path <- data_file_info$file_path

  var_info <- data6 %>%
    inner_join(
      data_file_info,
      by = c(
        "series", "data.years", "visit_period", "visit_number", "quarter",
        "rectype"
      ),
      #included visit_period
    ) %>%
    # We added this next line because some DOM files had rectype == "P"
    #filter(str_detect(dat_name, "^DOM")) %>%
    #can delete this once we figure out rectype issue
    distinct(`Código de variável`, `Posição inicial`, Tamanho, Decim) %>%
    mutate(
      `Posição inicial` = as.numeric(`Posição inicial`),
      Tamanho = as.numeric(str_remove(Tamanho, "\\..+"))
    )

  col_positions <- fwf_positions(
    start = var_info$`Posição inicial`,
    end = var_info$`Posição inicial` + var_info$Tamanho - 1,
    col_names = var_info$`Código de variável`
  )

  data <- read_fwf(
    file = data_file_path,
    col_positions = col_positions,
    col_types = cols(.default = col_character())
  )

  rm(data)
}
