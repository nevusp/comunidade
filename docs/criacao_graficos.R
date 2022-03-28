library(tidyverse)
library(echarts4r)


library(googledrive)
drive_auth("jjesusfilho@gmail.com")
drive_download(as_id("https://docs.google.com/spreadsheets/d/1YOblwpmIbYx36mCaW-r4UzK_Jw-NWBmsnRX3osdXr5U/edit?usp=sharing"),
                     "data/selecao_variaveis.xlsx", overwrite = TRUE)

selecao_variaveis <- readxl::read_excel("data/selecao_variaveis.xlsx",
                                col_names = FALSE) %>%
                     setNames("x") %>%
                     na.omit()


vars <- JurisMiner::busca_fuzzy(selecao_variaveis$x, names(ondas))


selecao_variaveis$vars <- vars


## Hábito
n2 <- attributes(habito$mseguro)$labels %>%
  subset(. %in% unique(habito$mseguro)) %>%
  names()


habito <- ondas %>%
         janitor::clean_names() %>%
         select(onda, mseguro) %>%
         na.omit() %>%
        as_tibble()



n2 <- attributes(habito$mseguro)$labels %>%
      subset(. %in% unique(habito$mseguro)) %>%
      names()

habito <- habito %>%
          mutate(onda = factor(onda, labels = c('2015',"2017","2018"))) %>%
          mutate(mseguro  = factor(mseguro, labels = n2))

habito <- habito %>%
        mutate(mseguro = case_when(
          mseguro == "Sim" ~ "Sim",
          mseguro == "Não" ~  "Não",
          TRUE ~ "Não sabe/não respondeu"
        ))

p1 <- c("#F5ECC1","#EEDCA7","#E6CC8E","#DFBD74","#D8AD5A","#D09D41","#C98D27","#B8832B","#A77930","#966F34","#846438","#735A3D","#625041")
p2 <- c("#00368A","#0E4090","#1D4A96","#2B549C","#3A5EA2","#4868A8","#5772AE","#657BB4","#7385BA","#828FC0","#9099C6","#9FA3CC","#ADADD2")


phabito <- c("#00368A","#F5ECC1","#625041")


paleta_divergente <- purrr::partial(colorspace::diverging_hcl, palette = "Blue-Red")

e_habito <- habito %>%
  count(onda, mseguro) %>%
  group_by(mseguro) %>%
  mutate(percentual = (n*100/sum(n, na.rm = T)) %>% round(1) %>% paste0("%")) %>%
  pivot_wider(names_from = mseguro, values_from = c("n","percentual")) %>%
  set_names(~str_remove(.x,"^n_")) %>%
  na.omit() %>%
  #mutate(relator = str_wrap(orgao_julgador, 10) %>% ToTitleCasePT() %>% factor(levels = nomes)) %>%
  ungroup() %>%
  e_charts(onda, reorder = T) %>%
  e_bar(Não, bind = percentual_Não, stack= "grp", emphasis = list(focus = "series")) %>%
  e_bar(`Não sabe/não respondeu`, bind = `percentual_Não sabe/não respondeu`, stack= "grp", emphasis = list(focus = "series")) %>%
  e_bar(Sim, bind = percentual_Sim, stack= "grp", emphasis = list(focus = "series")) %>%
  #e_x_axis(show= TRUE,  axisLabel = list(fontSize = 8)) %>%
  e_tooltip() %>%
  e_legend(FALSE) %>%
  e_title("De modo geral, essas mudanças de hábitos no seu dia a dia, fizeram com o que o(a) sr(a) se sentisse mais seguro(a)?") %>%
  e_flip_coords() %>%
  e_color(paleta_divergente(n = 3))

e_habito



## Concordância com as instituições



cp_conc <- ondas %>%
  select(starts_with("CPconc")) %>%
  select(-CPconc)

cp_conc <- cp_conc %>%
  map_dfc(~{

    n <- attributes(.x)$labels %>%
      subset(. %in% unique(.x)) %>%
      names()

    .x <- factor(.x, labels = n)
    .x
  })



cp_conc <- cp_conc %>%
  add_column(onda = ondas$onda) %>%
  pivot_longer(cols = -onda, names_to = "concordancia",
               values_to = "valor") %>%
  mutate(valor = as.character(valor))


cp_conc <- cp_conc %>%
  drop_na() %>%
  droplevels() %>%
  mutate(valor =  case_when(
    str_detect(valor, "(?i)(sabe|respondeu|Nem)") ~ "Não concorda nem discorda",
    TRUE ~ valor
  ))




afirmacoes <- c("A democracia pode ter seus problemas, mas é o melhor sistema de governo.",
  "O governo, em uma democracia, busca o bem estar das pessoas.",
  "A democracia cria condições para que as pessoas possam prosperar por seu próprio esforço.",
  "Os jovens devem ser ensinados a questionar e criticar o que dizem as autoridades.",
  "Precisamos de líderes com pulso firme para restabelecer a ordem.",
  "O que nosso país mais precisa é obediência e disciplina.",
  "O país estaria melhor se fizéssemos uma limpeza eliminando os criminosos.",
  "Até os criminosos devem ter seus direitos respeitados.", "Precisamos de um governo menos tolerante que tome ações mais duras contra o crime."
)

dd <- tibble(concordancia = unique(cp_conc$concordancia), afirmacoes)


cp_conc <- cp_conc %>%
        right_join(dd)


cp_onda3 <- cp_conc %>%
      filter(onda == 3) %>%
      select(-onda)


e_instituicoes<- cp_onda3 %>%
  count(afirmacoes, valor) %>%
  group_by(afirmacoes) %>%
  mutate(percentual = (n*100/sum(n, na.rm = T)) %>% round(1) %>% paste0("%")) %>%
  pivot_wider(names_from = valor, values_from = c("n","percentual")) %>%
  set_names(~str_remove(.x,"^n_")) %>%
  na.omit() %>%
  mutate(afirmacoes = str_wrap(afirmacoes, 30)) %>%
  ungroup() %>%
  e_charts(afirmacoes, reorder = T)

nomes <- c("Discorda totalmente", "Discorda em parte",  "Não concorda nem discorda",
           "Concorda em parte", "Concorda totalmente")

for (i in nomes){
  e_instituicoes <-  e_instituicoes %>%
    e_bar_(i, bind = paste0("percentual_",i), stack= "grp", emphasis = list(focus = "series"))

}


e_instituicoes <- e_instituicoes %>%
  e_tooltip() %>%
  e_legend(FALSE) %>%
  e_title("Você concorda ou discorda de cada uma das frases abaixo") %>%
  e_flip_coords() %>%
  e_color(paleta_divergente(n = 5)) %>%
  e_grid(left = "30%")


e_instituicoes

## Confiança nas pessoas



c_conf <- ondas %>%
  select(starts_with("Cconfiar"))

c_conf <- c_conf %>%
  map_dfc(~{

    n <- attributes(.x)$labels %>%
      subset(. %in% unique(.x)) %>%
      names()

    .x <- factor(.x, labels = n)
    .x
  })



c_conf <- c_conf %>%
  add_column(onda = ondas$onda) %>%
  pivot_longer(cols = -onda, names_to = "var",
               values_to = "valor") %>%
  mutate(valor = as.character(valor))


enunciado <- dicionario %>%
          filter(variaveis_banco_painel %in% unique(c_conf$var)) %>%
          select(var = variaveis_banco_painel, enunciado =enunciado_da_questao)

enunciado <- enunciado %>%
      mutate(enunciado = str_remove(enunciado, ".+?em ")) %>%
      mutate(enunciado = str_remove(enunciado, ":.*")) %>%
     distinct()

c_conf <- c_conf %>%
      right_join(enunciado)

opcoes <- c("Confia pouco", "Confia", "Não confia", "Confia muito", "Nao sabe",
            "Não se aplica", "Não respondeu")

df_opcoes <- tibble(opcoes)

df_opcoes <- df_opcoes %>%
         mutate(novas = case_when(
           str_detect(opcoes, "(?i)(sabe|aplica|respondeu)") ~ "Nao sabe/nao respondeu",
           TRUE ~ opcoes
         ))

c_conf <- c_conf %>%
        right_join(df_opcoes, by = c("valor" = "opcoes"))

c_conf <- c_conf %>%
       mutate(valor = novas,
              opcoes = NULL,
              novas = NULL)


c_conf3 <- c_conf %>%
  filter(onda == 3) %>%
  select(-onda)


e_confianca<- c_conf3 %>%
  count(enunciado, valor) %>%
  group_by(enunciado) %>%
  mutate(percentual = (n*100/sum(n, na.rm = T)) %>% round(1) %>% paste0("%")) %>%
  pivot_wider(names_from = valor, values_from = c("n","percentual")) %>%
  set_names(~str_remove(.x,"^n_")) %>%
  na.omit() %>%
  #mutate(enunciado = str_wrap(enunciado, 30)) %>%
  ungroup() %>%
  e_charts(enunciado, reorder = T)

nomes <- c( "Não confia","Confia pouco","Nao sabe/nao respondeu", "Confia", "Confia muito")

for (i in nomes){
  e_confianca <-  e_confianca %>%
    e_bar_(i, bind = paste0("percentual_",i), stack= "grp", emphasis = list(focus = "series"))

}


e_confianca <- e_confianca %>%
  e_tooltip() %>%
  e_legend(FALSE) %>%
  e_title("Gostaria que o senhor disse se confia ou não confia em:") %>%
  e_flip_coords() %>%
  e_color(paleta_divergente(n = 5)) %>%
  e_grid(left = "30%")


e_confianca
