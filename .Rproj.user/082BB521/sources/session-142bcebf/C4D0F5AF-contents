library(tidyverse)
library(ggnewscale)
library(sf)
library(tmap)
library(here)
tmap_mode("view")

# fx --------------------------------------------------------------------------------------------------------------
shp_extract_read <- function(path, shape, crs) {
  temp <- tempfile()
  unzip(path, exdir = temp, junkpaths = TRUE)
  listfiles <- list.files(temp, pattern = shape, full.names = TRUE)
  shp <- st_read(dsn = listfiles, crs = crs)
  unlink(temp, recursive = TRUE)
  return(shp)
}



# subprefeituras --------------------------------------------------------------------------------------------------
shp_subprefeituras <- shp_extract_read(
  "data/shp/SIRGAS_SHP_subprefeitura.zip" %>% here(), 
  "SIRGAS_SHP_subprefeitura_polygon.shp", 
  crs = 31983
)



# zeis ------------------------------------------------------------------------------------------------------------
shp_zeis <- shp_extract_read(
  "data/shp/PDE_4-Zeis1.zip" %>% here(), 
  "sirgas_PDE_4-Zeis1.shp", 
  crs = 31983
) %>% 
  rbind(
    shp_extract_read(
      "data/shp/PDE_4A-Zeis2-3-4-5.zip" %>% here(), 
      "sirgas_PDE_4A-Zeis2,3,4,5.shp", 
      crs = 31983
    )
  ) %>% 
  rename(tx_zonename = tx_zoneame, cd_zonename = cd_zoneame) %>% 
  #remover coluna de NAs que estragava o negócio
  select(-tx_observa) %>% 
  st_make_valid()


mapa_zeis <- tm_shape(shp_subprefeituras) +
  tm_borders() +
  tm_shape(
    shp_zeis %>% filter(cd_zonename %in% c("ZEIS-1", "ZEIS-5"))
  ) +
  tm_fill(col = "cd_zonename", title = "ZEIS", palette = "Set2", n = 4)

saveRDS(mapa_zeis, "data/rds/mapa_zeis.RDS")


# rede ------------------------------------------------------------------------------------------------------------
shp_metro <- shp_extract_read(
  "data/shp/SIRGAS_SHP_linhametro.zip" %>% here(), 
  "SIRGAS_SHP_linhametro_line.shp", 
  crs = 31983
)

shp_cptm <- shp_extract_read(
  "data/shp/SIRGAS_SHP_linhatrem.zip" %>% here(), 
  "SIRGAS_SHP_linhatrem.shp", 
  crs = 31983
)

shp_est <- shp_extract_read(
  "data/shp/SIRGAS_SHP_estacaometro.zip" %>% here(), 
  "SIRGAS_SHP_estacaometro_point.shp", 
  crs = 31983
) %>% 
  transmute(
    empresa = emt_empres,
    linha = emt_linha,
    nome = emt_nome,
    geometry = geometry
  )

shp_est <- shp_extract_read(
  "data/shp/SIRGAS_SHP_estacaotrem.zip" %>% here(), 
  "SIRGAS_SHP_estacaotrem_point.shp", 
  crs = 31983
) %>% 
  transmute(
    empresa = etr_empres,
    linha = etr_linha,
    nome = etr_nome,
    geometry = geometry
  ) %>% 
  rbind(shp_est) %>% 
  distinct()

shp_est <- readxl::read_excel(
  "data/shp/est_datas.xlsx"
) %>% 
  right_join(shp_est) %>% 
  st_as_sf()

shp_est <- shp_est %>% 
  mutate(
    inaugura = cut(
      inaugura,
      breaks = c(1800, 2002, 2023),
      labels = c("Até 2002", "2003-2018")
    )
  )



# inaugura --------------------------------------------------------------------------------------------------------
mapa_inaug <- ggplot() +
  geom_sf(data = shp_subprefeituras, fill = NA, color = "darkgrey") +
  geom_sf(data = shp_metro, aes(color = "Metrô")) +
  geom_sf(data = shp_cptm, aes(color = "CPTM")) +
  scale_color_manual(
    name = "Sistema", 
    breaks = c("Metrô", "CPTM"), values = c("darkseagreen", "bisque")
  ) +
  new_scale_color() +
  geom_sf(data = shp_est, aes(color = inaugura), size = 0.25) +
  coord_sf(crs = 4326, xlim = c(-46.85, -46.35), ylim = c(-23.35, -24.025)) +
  scale_color_brewer(palette = "Set1", name = "Inauguração") +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  labs(
    title = "Rede de trilhos de São Paulo",
    caption = "Fontes: GeoSampa, Metrô, CPTM e Viamobilidade"
  ) +
  theme_void()

saveRDS(mapa_inaug, "data/rds/mapa_inaug.RDS")



# aop -------------------------------------------------------------------------------------------------------------
shp_aop <- readRDS("data/rds/shp_aop.RDS")

mapa_aop <- ggplot() +
  geom_sf(data = shp_aop, aes(fill = CMATT60), color = NA, alpha = .9) +
  scale_fill_viridis_c(option = "inferno", labels = scales::percent) +
  coord_sf(crs = 4326, xlim = c(-46.85, -46.35), ylim = c(-23.35, -24.025)) +
  labs(
    title='Proporção de trabalhos acessíveis', 
    fill="Acessibilidade",
    subtitle='por transporte público em menos de 60 min.', 
    caption = "Fonte: Pereira et al. (2020)"
  ) +
  theme_void()

saveRDS(mapa_aop, "data/rds/mapa_aop.RDS")

mapa_aop2 <- mapa_aop +
  list(
    geom_sf(data = shp_metro, color = "deepskyblue", linewidth = 0.25),
    geom_sf(data = shp_cptm, color = "green", linewidth = 0.125),
    coord_sf(crs = 4326, xlim = c(-46.85, -46.35), ylim = c(-23.35, -24.025))
  )

saveRDS(mapa_aop2, "data/rds/mapa_aop2.RDS")



