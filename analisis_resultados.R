
# paquetes ----------------------------------------------------------------

library(tidyverse)

# cargar datos ------------------------------------------------------------

load("resultados/resultados_unificados.RData")

# análisis ----------------------------------------------------------------

## Cantidad total de corridas experimentales ----

### Comparado contra EV

resultados_unificados %>% 
  dplyr::mutate(CS = if_else(CS == "I", "CONVEXSIM - I",
                             "CONVEXSIM - S")) %>% 
  ggplot2::ggplot(aes(x = p, y = tn_EV, group = p)) +
  geom_boxplot(color = "#023047", fill = "white") +
  stat_summary(fun = mean, geom = "point", size = 2, 
               color = "#023047", fill = "#fb8500", shape = 23) + 
  stat_summary(aes(group = 1), geom = "line", color = "#fb8500",
               fun = mean, linetype = "dotdash") +
  scale_x_continuous(breaks = 3:10) +
  facet_grid(~CS) +
  labs(x = "Cantidad de componentes (p)",
       y = "Tasa") +
  theme_bw(base_size = 12) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 12))


resultados_unificados %>% 
  dplyr::mutate(CS = if_else(CS == "I", "CONVEXSIM - I",
                             "CONVEXSIM - S"), 
                n = stringr::str_c("n = ", n)) %>% 
  ggplot2::ggplot(aes(x = p, y = tn_EV, group = p)) +
  geom_boxplot(color = "#023047", fill = "white") +
  stat_summary(fun = mean, geom = "point", size = 2, 
               color = "#023047", fill = "#fb8500", shape = 23) + 
  stat_summary(aes(group = 1), geom = "line", color = "#fb8500",
               fun = mean, linetype = "dotdash") +
  scale_x_continuous(breaks = 3:10) +
  facet_grid(~n) +
  labs(x = "Cantidad de componentes (p)",
       y = "Tasa") +
  theme_bw(base_size = 12) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 12))

### Comparado contra SR

resultados_unificados %>% 
  dplyr::mutate(CS = if_else(CS == "I", "CONVEXSIM - I",
                             "CONVEXSIM - S"), 
                m = case_when(m == 1 ~ "m = 1",
                              m == 2 ~ "m = 2",
                              m == 3 ~ "m = 3")) %>% 
  ggplot2::ggplot(aes(x = p, y = tn_SR, group = p)) +
  geom_boxplot(color = "#023047", fill = "white") +
  stat_summary(fun = mean, geom = "point", size = 2, 
               color = "#023047", fill = "#fb8500", shape = 23) + 
  stat_summary(aes(group = 1), geom = "line", color = "#fb8500",
               fun = mean, linetype = "dotdash") +
  scale_x_continuous(breaks = 3:10) +
  facet_grid(m~CS) +
  labs(x = "Cantidad de componentes (p)",
       y = "Tasa") +
  theme_bw(base_size = 12) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 12) )

### En términos absolutos

resultados_unificados %>% 
  dplyr::mutate(CS = if_else(CS == "I", "CONVEXSIM - I",
                             "CONVEXSIM - S"),
                n_SR = n_conv/(1-tn_SR)) %>% 
  tidyr::pivot_longer(c(n_ev, n_conv, n_SR), 
                      names_to = "Tipo", 
                      values_to = "Cantidad") %>% 
  dplyr::mutate(Tipo = case_when(Tipo == "n_conv" ~ "Vértices extremos \n(CONVEXSIM)",
                                 Tipo == "n_ev" ~ "Vértices \nextremos",
                                 Tipo == "n_SR" ~ "Símplex \nreticular"),
                n = case_when(n == 1 ~ "Réplicas = 1",
                              n == 2 ~ "Réplicas = 2",
                              n == 3 ~ "Réplicas = 3",
                              n == 4 ~ "Réplicas = 4")) %>% 
  dplyr::group_by(Tipo, p, n) %>% 
  dplyr::summarise(mCantidad = mean(Cantidad)) %>% 
  ggplot2::ggplot(aes(x = factor(p), y = mCantidad, fill = Tipo)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("#023047", "#fb8500", "#ffb703")) +
  labs(x = "Cantidad de componentes (p)",
       y = "Cantidad promedio \nde corridas") +
  facet_wrap(vars(n), scales = "free", nrow = 2) +
  theme_bw(base_size = 12) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 12), 
        legend.position = "right")


# corridas en función de los componentes ----------------------------------

resultados_unificados %>% 
  dplyr::mutate(CS = if_else(CS == "I", "CONVEXSIM - I",
                             "CONVEXSIM - S"), 
                p = forcats::fct_reorder(stringr::str_c("p = ", p), p)) %>% 
  ggplot2::ggplot(aes(x = n_b_i, y = tn_EV, group = n_b_i)) +
  geom_boxplot(color = "#023047", fill = "white") +
  stat_summary(fun = mean, geom = "point", size = 2, 
               color = "#023047", fill = "#fb8500", shape = 23) + 
  stat_summary(aes(group = 1), geom = "line", color = "#fb8500",
               fun = mean, linetype = "dotdash") +
  scale_x_continuous(breaks = 1:10) +
  facet_wrap(vars(p), nrow = 2, scales = "free_x") +
  labs(x = "Cantidad de restricciones de frontera superior",
       y = "Tasa") +
  theme_bw(base_size = 12) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 12))



cor(resultados_unificados$tam_rest, resultados_unificados$tn_EV, 
    method = "spearman")

cor(resultados_unificados$tam_rest, resultados_unificados$tn_EV, 
    method = "pearson")


resultados_unificados %>% 
  dplyr::mutate(CS = if_else(CS == "I", "CONVEXSIM - I",
                             "CONVEXSIM - S")) %>% 
  dplyr::filter(m == 3) %>% 
  ggplot2::ggplot(aes(x = p, y = tn_SR, group = p)) +
  geom_boxplot(color = "#023047", fill = "white") +
  stat_summary(fun = mean, geom = "point", size = 2, 
               color = "#023047", fill = "#fb8500", shape = 23) + 
  stat_summary(aes(group = 1), geom = "line", color = "#fb8500",
               fun = mean, linetype = "dotdash") +
  scale_x_continuous(breaks = 1:10) +
  facet_wrap(vars(CS), nrow = 1) +
  labs(x = "Cantidad de componentes (p)",
       y = "Tasa") +
  theme_bw(base_size = 12) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 12))



# Eficacia ----------------------------------------------------------------

resultados_unificados_c <- resultados_unificados %>% 
  dplyr::mutate(A = abs(A_opt_SR_Prop - A_opt_CXS_Pseudo), 
                D = abs(D_opt_SR_Prop - D_opt_CXS_Pseudo), 
                G = abs(G_opt_SR_Prop - G_opt_CXS_Pseudo), 
                V = abs(V_opt_SR_Prop - V_opt_CXS_Pseudo)) %>% 
  dplyr::filter(V == 0, 
                A < 0.1)

optimalidad_prop <- resultados_unificados_c %>% 
  dplyr::mutate(A = 
                  case_when(A_opt_SR_Prop == pmin(A_opt_SR_Prop, 
                                                  A_opt_EV_Prop,
                                                  A_opt_CXS_Prop, 
                                                  na.rm = TRUE) ~ "Símplex \nreticular", 
                            A_opt_EV_Prop == pmin(A_opt_SR_Prop, 
                                                  A_opt_EV_Prop,
                                                  A_opt_CXS_Prop, 
                                                  na.rm = TRUE) ~ "Vértices \nextremos",
                            A_opt_CXS_Prop == pmin(A_opt_SR_Prop, 
                                                   A_opt_EV_Prop,
                                                   A_opt_CXS_Prop, 
                                                   na.rm = TRUE) ~ "Vértices extremos \n(CONVEXSIM)",
                            .default = "Otro"), 
                D = 
                  case_when(D_opt_SR_Prop == pmin(D_opt_SR_Prop, 
                                                  D_opt_EV_Prop,
                                                  D_opt_CXS_Prop, 
                                                  na.rm = TRUE) ~ "Símplex \nreticular", 
                            D_opt_EV_Prop == pmin(D_opt_SR_Prop, 
                                                  D_opt_EV_Prop,
                                                  D_opt_CXS_Prop, 
                                                  na.rm = TRUE) ~ "Vértices \nextremos",
                            D_opt_CXS_Prop == pmin(D_opt_SR_Prop, 
                                                   D_opt_EV_Prop,
                                                   D_opt_CXS_Prop, 
                                                   na.rm = TRUE) ~ "Vértices extremos \n(CONVEXSIM)",
                            .default = "Otro"), 
                G = 
                  case_when(G_opt_SR_Prop == pmin(G_opt_SR_Prop, 
                                                  G_opt_EV_Prop,
                                                  G_opt_CXS_Prop, 
                                                  na.rm = TRUE) ~ "Símplex \nreticular", 
                            G_opt_EV_Prop == pmin(G_opt_SR_Prop, 
                                                  G_opt_EV_Prop,
                                                  G_opt_CXS_Prop, 
                                                  na.rm = TRUE) ~ "Vértices \nextremos",
                            G_opt_CXS_Prop == pmin(G_opt_SR_Prop, 
                                                   G_opt_EV_Prop,
                                                   G_opt_CXS_Prop, 
                                                   na.rm = TRUE) ~ "Vértices extremos \n(CONVEXSIM)",
                            .default = "Otro"), 
                V = 
                  case_when(V_opt_SR_Prop == pmin(V_opt_SR_Prop, 
                                                  V_opt_EV_Prop,
                                                  V_opt_CXS_Prop, 
                                                  na.rm = TRUE) ~ "Símplex \nreticular", 
                            V_opt_EV_Prop == pmin(V_opt_SR_Prop, 
                                                  V_opt_EV_Prop,
                                                  V_opt_CXS_Prop, 
                                                  na.rm = TRUE) ~ "Vértices \nextremos",
                            V_opt_CXS_Prop == pmin(V_opt_SR_Prop, 
                                                   V_opt_EV_Prop,
                                                   V_opt_CXS_Prop, 
                                                   na.rm = TRUE) ~ "Vértices extremos \n(CONVEXSIM)",
                            .default = "Otro"))

## Optimalidad proporciones

optimalidad_prop %>% 
  dplyr::mutate(p = forcats::fct_reorder(stringr::str_c("p = ", p), p)) %>% 
  tidyr::pivot_longer(c('A', "D", "G", "V"),
                      names_to = "Ganador",
                      values_to = "Diseño") %>% 
  dplyr::group_by(p, Ganador, Diseño) %>% 
  dplyr::reframe(Valores = n()) %>% 
  ggplot2::ggplot(aes(x = Ganador, y =  Valores, fill = Diseño)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("#023047", "#fb8500", "#ffb703")) +
  facet_wrap(vars(p), scales = "free") +
  labs(x = "Criterio de optimalidad",
       y = "Cantidad") +
  theme_bw(base_size = 12) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 12), 
        legend.position = c(0.85, 0.12))


optimalidad_prop %>% 
  dplyr::mutate(m = forcats::fct_reorder(stringr::str_c("m = ", m), m)) %>% 
  tidyr::pivot_longer(c('A', "D", "G", "V"),
                      names_to = "Ganador",
                      values_to = "Diseño") %>% 
  dplyr::group_by(m, Ganador, Diseño) %>% 
  dplyr::reframe(Valores = n()) %>% 
  ggplot2::ggplot(aes(x = Ganador, y =  Valores, fill = Diseño)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("#023047", "#fb8500", "#ffb703")) +
  facet_wrap(vars(m), scales = "free") +
  labs(x = "Criterio de optimalidad",
       y = "Cantidad") +
  theme_bw(base_size = 12) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 12))

### Pseudocomponentes ----

optimalidad_pseudo <- resultados_unificados_c %>% 
  dplyr::mutate(across(where(is.numeric), \(x) round(x, 2))) %>% 
  dplyr::mutate(A = 
                  case_when(A_opt_CXS_Pseudo == pmin(A_opt_CXS_Pseudo,
                                                     # A_opt_SR_Prop, 
                                                     A_opt_EV_Pseudo, 
                                                     na.rm = TRUE) ~ "Vértices extremos \n(CONVEXSIM)",
                            # A_opt_SR_Prop == pmin(A_opt_CXS_Pseudo,
                            #                       # A_opt_SR_Prop, 
                            #                       A_opt_EV_Pseudo,
                            #                       na.rm = TRUE) ~ "Símplex \nreticular", 
                            A_opt_EV_Pseudo == pmin(A_opt_CXS_Pseudo,
                                                    # A_opt_SR_Prop, 
                                                    A_opt_EV_Pseudo, 
                                                    na.rm = TRUE) ~ "Vértices \nextremos",
                            .default = "Otro"), 
                D = 
                  case_when(D_opt_CXS_Pseudo == pmin(D_opt_CXS_Pseudo,
                                                     # D_opt_SR_Prop, 
                                                     D_opt_EV_Pseudo, 
                                                     na.rm = TRUE) ~ "Vértices extremos \n(CONVEXSIM)",
                            # D_opt_SR_Prop == pmin(D_opt_CXS_Pseudo,
                            #                       D_opt_SR_Prop, 
                            #                       D_opt_EV_Pseudo,
                            #                       na.rm = TRUE) ~ "Símplex \nreticular", 
                            D_opt_EV_Pseudo == pmin(D_opt_CXS_Pseudo,
                                                    # D_opt_SR_Prop, 
                                                    D_opt_EV_Pseudo, 
                                                    na.rm = TRUE) ~ "Vértices \nextremos",
                            .default = "Otro"), 
                G = 
                  case_when(G_opt_CXS_Pseudo == pmin(G_opt_CXS_Pseudo,
                                                     # G_opt_SR_Prop, 
                                                     G_opt_EV_Pseudo, 
                                                     na.rm = TRUE) ~ "Vértices extremos \n(CONVEXSIM)",
                            # G_opt_SR_Prop == pmin(G_opt_CXS_Pseudo,
                            #                       G_opt_SR_Prop, 
                            #                       G_opt_EV_Pseudo,
                            #                       na.rm = TRUE) ~ "Símplex \nreticular", 
                            G_opt_EV_Pseudo == pmin(G_opt_CXS_Pseudo,
                                                    # G_opt_SR_Prop, 
                                                    G_opt_EV_Pseudo, 
                                                    na.rm = TRUE) ~ "Vértices \nextremos",
                            .default = "Otro"),
                V = 
                  case_when(V_opt_CXS_Pseudo == pmin(V_opt_CXS_Pseudo,
                                                     # V_opt_SR_Prop, 
                                                     V_opt_EV_Pseudo, 
                                                     na.rm = TRUE) ~ "Vértices extremos \n(CONVEXSIM)",
                            # V_opt_SR_Prop == pmin(V_opt_CXS_Pseudo,
                            #                       V_opt_SR_Prop, 
                            #                       V_opt_EV_Pseudo,
                            #                       na.rm = TRUE) ~ "Símplex \nreticular", 
                            V_opt_EV_Pseudo == pmin(V_opt_CXS_Pseudo,
                                                    # V_opt_SR_Prop, 
                                                    V_opt_EV_Pseudo, 
                                                    na.rm = TRUE) ~ "Vértices \nextremos",
                            .default = "Otro"))


optimalidad_pseudo %>% 
  dplyr::mutate(p = forcats::fct_reorder(stringr::str_c("p = ", p), p)) %>% 
  tidyr::pivot_longer(c('A', "D", "G", "V"),
                      names_to = "Ganador",
                      values_to = "Diseño") %>% 
  dplyr::group_by(p, Ganador, Diseño) %>% 
  dplyr::reframe(Valores = n()) %>% 
  dplyr::filter(Diseño != "Otro") %>% 
  ggplot2::ggplot(aes(x = Ganador, y =  Valores, fill = Diseño)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("#023047", "#fb8500", "#ffb703", "red")) +
  facet_wrap(vars(p), scales = "free") +
  labs(x = "Criterio de optimalidad",
       y = "Cantidad") +
  theme_bw(base_size = 12) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 12), 
        legend.position = c(0.85, 0.12))


optimalidad_pseudo %>% 
  dplyr::mutate(m = forcats::fct_reorder(stringr::str_c("m = ", m), m)) %>% 
  tidyr::pivot_longer(c('A', "D", "G", "V"),
                      names_to = "Ganador",
                      values_to = "Diseño") %>% 
  dplyr::group_by(m, Ganador, Diseño) %>% 
  dplyr::reframe(Valores = n()) %>% 
  dplyr::filter(Diseño != "Otro") %>%
  ggplot2::ggplot(aes(x = Ganador, y =  Valores, fill = Diseño)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("#023047", "#fb8500", "#ffb703")) +
  facet_wrap(vars(m), scales = "free") +
  labs(x = "Criterio de optimalidad",
       y = "Cantidad") +
  theme_bw(base_size = 12) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 12))

### Correlaciones para proporciones

matriz <- resultados_unificados_c %>% 
  dplyr::select(dE_SR, dE_EV, R, n,
                dplyr::matches("_opt_")) %>%
  dplyr::select(dE_SR, dE_EV, R, n,
                matches("_Prop")) %>% 
  cor() 

correlaciones <- matriz %>% 
  as.table() %>% 
  as.data.frame()

correlaciones %>% 
  dplyr::filter(Var1 %in% c("dE_SR", "dE_EV", "R", "n"), 
                !Var2 %in% c("dE_SR", "dE_EV", "R", "n")) %>% 
  ggplot2::ggplot(aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = round(Freq, 2)), color = "black", size = 3) +
  scale_fill_gradient2(low = "#ffb703", mid = "white", high = "#023047", 
                       midpoint = 0, limits = c(-1, 1)) +
  scale_x_discrete(labels = c("A_opt_SR_Prop" = "A\n(SR)",
                              "D_opt_SR_Prop" = "D\n(SR)", 
                              "G_opt_SR_Prop" = "G\n(SR)", 
                              "V_opt_SR_Prop" = "V\n(SR)", 
                              "A_opt_EV_Prop" = "A\n(VE)",
                              "D_opt_EV_Prop" = "D\n(VE)", 
                              "G_opt_EV_Prop" = "G\n(VE)", 
                              "V_opt_EV_Prop" = "V\n(VE)", 
                              "A_opt_CXS_Prop" = "A\n(CONVEXSIM)",
                              "D_opt_CXS_Prop" = "D\n(CONVEXSIM)", 
                              "G_opt_CXS_Prop" = "G\n(CONVEXSIM)", 
                              "V_opt_CXS_Prop" = "V\n(CONVEXSIM)")) +
  scale_y_discrete(labels = c("n" = "Réplicas", 
                              "R" = "Rango \nPromedio",
                              "dE_EV" = "Distancia \necludiana \n(VE)",
                              "dE_SR" = "Distancia \necludiana \n(SR)")) +
  labs(x = "Criterios de optimalidad",
       y = "", 
       fill = "Correlación", 
       caption = "SR: Símplex reticular \nVE: Vértices extremos") +
  theme_minimal(base_size = 12) +
  theme(line = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.caption = element_text(hjust = 0))


### Correlaciones para pseudocomponentes

matriz <- resultados_unificados_c %>% 
  dplyr::select(dE_SR, dE_EV, R, n,
                dplyr::matches("_opt_")) %>%
  dplyr::select(dE_SR, dE_EV, R, n,
                matches("_Pseudo")) %>% 
  cor() 

correlaciones <- matriz %>% 
  as.table() %>% 
  as.data.frame()

correlaciones %>% 
  dplyr::filter(Var1 %in% c("dE_SR", "dE_EV", "R", "n"), 
                !Var2 %in% c("dE_SR", "dE_EV", "R", "n")) %>% 
  ggplot2::ggplot(aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = round(Freq, 2)), color = "black", size = 3) +
  scale_fill_gradient2(low = "#ffb703", mid = "white", high = "#023047", 
                       midpoint = 0, limits = c(-1, 1)) +
  scale_x_discrete(labels = c("A_opt_SR_Pseudo" = "A\n(SR)",
                              "D_opt_SR_Pseudo" = "D\n(SR)", 
                              "G_opt_SR_Pseudo" = "G\n(SR)", 
                              "V_opt_SR_Pseudo" = "V\n(SR)", 
                              "A_opt_EV_Pseudo" = "A\n(VE)",
                              "D_opt_EV_Pseudo" = "D\n(VE)", 
                              "G_opt_EV_Pseudo" = "G\n(VE)", 
                              "V_opt_EV_Pseudo" = "V\n(VE)", 
                              "A_opt_CXS_Pseudo" = "A\n(CONVEXSIM)",
                              "D_opt_CXS_Pseudo" = "D\n(CONVEXSIM)", 
                              "G_opt_CXS_Pseudo" = "G\n(CONVEXSIM)", 
                              "V_opt_CXS_Pseudo" = "V\n(CONVEXSIM)")) +
  scale_y_discrete(labels = c("n" = "Réplicas", 
                              "R" = "Rango \nPromedio",
                              "dE_EV" = "Distancia \necludiana \n(VE)",
                              "dE_SR" = "Distancia \necludiana \n(SR)")) +
  labs(x = "Criterios de optimalidad",
       y = "", 
       fill = "Correlación", 
       caption = "SR: Símplex reticular \nVE: Vértices extremos") +
  theme_minimal(base_size = 12) +
  theme(line = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.caption = element_text(hjust = 0))

# eficiencia --------------------------------------------------------------

eficiencia_prop <- resultados_unificados_c %>% 
  dplyr::mutate(A = 
                  case_when(A_eff_SR_Prop == pmax(A_eff_SR_Prop, 
                                                  A_eff_EV_Prop,
                                                  A_eff_CXS_Prop, 
                                                  na.rm = TRUE) ~ "Símplex \nreticular", 
                            A_eff_EV_Prop == pmax(A_eff_SR_Prop, 
                                                  A_eff_EV_Prop,
                                                  A_eff_CXS_Prop, 
                                                  na.rm = TRUE) ~ "Vértices \nextremos",
                            A_eff_CXS_Prop == pmax(A_eff_SR_Prop, 
                                                   A_eff_EV_Prop,
                                                   A_eff_CXS_Prop, 
                                                   na.rm = TRUE) ~ "Vértices extremos \n(CONVEXSIM)",
                            .default = "Otro"), 
                D = 
                  case_when(D_eff_SR_Prop == pmax(D_eff_SR_Prop, 
                                                  D_eff_EV_Prop,
                                                  D_eff_CXS_Prop, 
                                                  na.rm = TRUE) ~ "Símplex \nreticular", 
                            D_eff_EV_Prop == pmax(D_eff_SR_Prop, 
                                                  D_eff_EV_Prop,
                                                  D_eff_CXS_Prop, 
                                                  na.rm = TRUE) ~ "Vértices \nextremos",
                            D_eff_CXS_Prop == pmax(D_eff_SR_Prop, 
                                                   D_eff_EV_Prop,
                                                   D_eff_CXS_Prop, 
                                                   na.rm = TRUE) ~ "Vértices extremos \n(CONVEXSIM)",
                            .default = "Otro"), 
                G = 
                  case_when(G_eff_SR_Prop == pmax(G_eff_SR_Prop, 
                                                  G_eff_EV_Prop,
                                                  G_eff_CXS_Prop, 
                                                  na.rm = TRUE) ~ "Símplex \nreticular", 
                            G_eff_EV_Prop == pmax(G_eff_SR_Prop, 
                                                  G_eff_EV_Prop,
                                                  G_eff_CXS_Prop, 
                                                  na.rm = TRUE) ~ "Vértices \nextremos",
                            G_eff_CXS_Prop == pmax(G_eff_SR_Prop, 
                                                   G_eff_EV_Prop,
                                                   G_eff_CXS_Prop, 
                                                   na.rm = TRUE) ~ "Vértices extremos \n(CONVEXSIM)",
                            .default = "Otro"))


eficiencia_pseudo <- resultados_unificados_c %>% 
  dplyr::mutate(across(where(is.numeric), \(x) round(x, 2))) %>% 
  dplyr::mutate(A = 
                  case_when(#A_eff_SR_Prop == pmax(A_eff_SR_Prop, 
                                                  # A_eff_EV_Pseudo,
                                                  # A_eff_CXS_Pseudo, 
                                                  # na.rm = TRUE) ~ "Símplex \nreticular", 
                            A_eff_EV_Pseudo == pmax(#A_eff_SR_Prop, 
                                                    A_eff_EV_Pseudo,
                                                    A_eff_CXS_Pseudo, 
                                                    na.rm = TRUE) ~ "Vértices \nextremos",
                            A_eff_CXS_Pseudo == pmax(#A_eff_SR_Prop, 
                                                     A_eff_EV_Pseudo,
                                                     A_eff_CXS_Pseudo, 
                                                     na.rm = TRUE) ~ "Vértices extremos \n(CONVEXSIM)",
                            .default = "Otro"), 
                D = 
                  case_when(#D_eff_SR_Prop == pmax(D_eff_SR_Prop, 
                                                  # D_eff_EV_Pseudo,
                                                  # D_eff_CXS_Pseudo, 
                                                  # na.rm = TRUE) ~ "Símplex \nreticular", 
                            D_eff_EV_Pseudo == pmax(#D_eff_SR_Prop, 
                                                    D_eff_EV_Pseudo,
                                                    D_eff_CXS_Pseudo, 
                                                    na.rm = TRUE) ~ "Vértices \nextremos",
                            D_eff_CXS_Pseudo == pmax(#D_eff_SR_Prop, 
                                                     D_eff_EV_Pseudo,
                                                     D_eff_CXS_Pseudo, 
                                                     na.rm = TRUE) ~ "Vértices extremos \n(CONVEXSIM)",
                            .default = "Otro"), 
                G = 
                  case_when(#G_eff_SR_Prop == pmax(G_eff_SR_Prop,
                  #                                 G_eff_EV_Pseudo,
                  #                                 G_eff_CXS_Pseudo, 
                  #                                 na.rm = TRUE) ~ "Símplex \nreticular", 
                            G_eff_EV_Pseudo == pmax(#G_eff_SR_Prop, 
                                                    G_eff_EV_Pseudo,
                                                    G_eff_CXS_Pseudo, 
                                                    na.rm = TRUE) ~ "Vértices \nextremos",
                            G_eff_CXS_Pseudo == pmax(#G_eff_SR_Prop, 
                                                     G_eff_EV_Pseudo,
                                                     G_eff_CXS_Pseudo, 
                                                     na.rm = TRUE) ~ "Vértices extremos \n(CONVEXSIM)",
                            .default = "Otro"))

eficiencia_prop %>% 
  dplyr::mutate(p = forcats::fct_reorder(stringr::str_c("p = ", p), p)) %>% 
  tidyr::pivot_longer(c('A', "D", "G"),
                      names_to = "Ganador",
                      values_to = "Diseño") %>% 
  dplyr::group_by(p, Ganador, Diseño) %>% 
  dplyr::reframe(Valores = n()) %>% 
  dplyr::filter(Diseño != "Otro") %>% 
  ggplot2::ggplot(aes(x = Ganador, y =  Valores, fill = Diseño)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("#023047", "#fb8500", "#ffb703", "red")) +
  facet_wrap(vars(p), scales = "free") +
  labs(x = "Criterio de eficiencia",
       y = "Cantidad") +
  theme_bw(base_size = 12) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 12), 
        legend.position = c(0.85, 0.12))


eficiencia_prop %>% 
  dplyr::mutate(m = forcats::fct_reorder(stringr::str_c("m = ", m), m)) %>% 
  tidyr::pivot_longer(c('A', "D", "G"),
                      names_to = "Ganador",
                      values_to = "Diseño") %>% 
  dplyr::group_by(m, Ganador, Diseño) %>% 
  dplyr::reframe(Valores = n()) %>% 
  ggplot2::ggplot(aes(x = Ganador, y =  Valores, fill = Diseño)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("#023047", "#fb8500", "#ffb703")) +
  facet_wrap(vars(m), scales = "free") +
  labs(x = "Criterio de eficiencia",
       y = "Cantidad") +
  theme_bw(base_size = 12) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 12))



eficiencia_pseudo %>% 
  dplyr::mutate(p = forcats::fct_reorder(stringr::str_c("p = ", p), p)) %>% 
  tidyr::pivot_longer(c('A', "D", "G"),
                      names_to = "Ganador",
                      values_to = "Diseño") %>% 
  dplyr::group_by(p, Ganador, Diseño) %>% 
  dplyr::reframe(Valores = n()) %>% 
  dplyr::filter(Diseño != "Otro") %>% 
  ggplot2::ggplot(aes(x = Ganador, y =  Valores, fill = Diseño)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("#023047", "#fb8500", "#ffb703", "red")) +
  facet_wrap(vars(p), scales = "free") +
  labs(x = "Criterio de eficiencia",
       y = "Cantidad") +
  theme_bw(base_size = 12) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 12), 
        legend.position = c(0.85, 0.12))


eficiencia_pseudo %>% 
  dplyr::mutate(m = forcats::fct_reorder(stringr::str_c("m = ", m), m)) %>% 
  tidyr::pivot_longer(c('A', "D", "G"),
                      names_to = "Ganador",
                      values_to = "Diseño") %>% 
  dplyr::group_by(m, Ganador, Diseño) %>% 
  dplyr::reframe(Valores = n()) %>% 
  dplyr::filter(Diseño != "Otro") %>% 
  ggplot2::ggplot(aes(x = Ganador, y =  Valores, fill = Diseño)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("#023047", "#fb8500", "#ffb703", "red")) +
  facet_wrap(vars(m), scales = "free") +
  labs(x = "Criterio de eficiencia",
       y = "Cantidad") +
  theme_bw(base_size = 12) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 12))
