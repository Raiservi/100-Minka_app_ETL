#############################################################################


#Descarrega les dades de MInka a traves d API les tracta i les envia a Postgres


#Data: 05/09/2026


#Versio: 1

#################################################################################

library(rminka)
library(sf)
library(DBI)
library(RPostgres)
library(leaflet)
library(tidyverse)
#------------------correccio del link de imtges de minka----------------------


# Actualitzacio link imatges

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = "bvuvabcrsqjxpsfu2hjg",
  host = "bvuvabcrsqjxpsfu2hjg-postgresql.services.clever-cloud.com",
  port = 8400,
  user = "ugnzz7lvvtl57ckg8oxa",
  password = "dfkakXRYIpvVlD6tBxEvxoC91Ec78r"
)

dbExecute(con, "UPDATE public.obs_cat SET url_picture = regexp_replace(url_picture, '/square\\.', '/original.') WHERE url_picture LIKE '%/square.%'")
dbExecute(con, "UPDATE public.obs_cat SET url_picture = regexp_replace(url_picture, 'https://minka-sdg.org', 'https://observe.minka-sdg.org') WHERE url_picture LIKE '%https://minka-sdg.org/attachments%'")

# check que ja no en queda cap square
dbGetQuery(con, "SELECT COUNT(*) FROM public.obs_cat WHERE url_picture LIKE '%/square.%'")
DBI::dbDisconnect(con)
dbGetQuery(con, "SELECT year, COUNT(*) FROM public.obs_cat WHERE url_picture LIKE '%original.jpeg' GROUP BY year ORDER BY year")
DBI::dbDisconnect(con)

##################################################################################3

#Previ a la descarrega, preparacio de l area de descarrega

#limits de Catalunya pel filtre de les obs

catalunya <- readRDS("./Extensio_cat_1.rds")

# 2. Ver los límites totales de toda la capa
lim_catalunya <- st_bbox(catalunya)
# xmin  ymin  xmax  ymax

lim_catalunya <- c(42.4562621,3.3719196, 40.4341866,0.3975516)


#Descarregem les dades d un any nomes de catalunya y amb quality_grade:research


#=======================Observacions 2017========================================

obs2017 <- rminka::mnk_obs(year = 2017,quality = "research", bounds = lim_catalunya, limit_download= FALSE,)

capa_obs2017 <-  st_as_sf(obs2017, coords = c("longitude","latitude"),crs =  "WGS84" )

names(capa_obs2017)

#remobrem l Id per evitar conflictes a Posgres i canviem el CRS

capa_obs2017_25831 <- capa_obs2017 %>%
  rename(id_minka = id) %>%
  st_transform(25831)

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = "bvuvabcrsqjxpsfu2hjg",
  host = "bvuvabcrsqjxpsfu2hjg-postgresql.services.clever-cloud.com",
  port = 8400,
  user = "ugnzz7lvvtl57ckg8oxa",
  password = "dfkakXRYIpvVlD6tBxEvxoC91Ec78r"
)

st_write(capa_obs2017_25831, con, Id(schema="public", table="obs_cat"), delete_layer = TRUE)

dbListTables(con)
dbGetQuery(con, "SELECT COUNT(*) FROM public.obs_cat;")

dbDisconnect(con)





#Ho comprobem

st_crs(capa_obs2017_25831)

#Ho dibuixem per asegurarrnos
leaflet()%>%
  
  addProviderTiles(providers$Esri.WorldGrayCanvas, group = "CARTO",
                   options = providerTileOptions(crossOrigin = TRUE)) %>%
  
  addMarkers(data = capa_obs2017)

#=======================Observacions 2018========================================

obs2018 <- rminka::mnk_obs(year = 2018,quality = "research", bounds = lim_catalunya, limit_download= FALSE,)


capa_obs2018 <-  st_as_sf(obs2018, coords = c("longitude","latitude"),crs =  "WGS84" )

names(capa_obs2018)

#remobrem l Id per evitar conflictes a Posgres i canviem el CRS

capa_obs2018_25831 <- capa_obs2018 %>%
  rename(id_minka = id) %>%
  st_transform(25831)

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = "bvuvabcrsqjxpsfu2hjg",
  host = "bvuvabcrsqjxpsfu2hjg-postgresql.services.clever-cloud.com",
  port = 8400,
  user = "ugnzz7lvvtl57ckg8oxa",
  password = "dfkakXRYIpvVlD6tBxEvxoC91Ec78r"
)

st_write(capa_obs2018_25831, con, Id(schema="public", table="obs_cat"), append = TRUE)

dbListTables(con)
dbGetQuery(con, "SELECT COUNT(*) FROM public.obs_cat;")

dbDisconnect(con)

#=======================Observacions 2019========================================

obs2019 <- rminka::mnk_obs(year = 2019,quality = "research", bounds = lim_catalunya, limit_download= FALSE,)


capa_obs2019 <-  st_as_sf(obs2019, coords = c("longitude","latitude"),crs =  "WGS84" )

names(capa_obs2019)

#remobrem l Id per evitar conflictes a Posgres i canviem el CRS

capa_obs2019_25831 <- capa_obs2019 %>%
  rename(id_minka = id) %>%
  st_transform(25831)

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = "bvuvabcrsqjxpsfu2hjg",
  host = "bvuvabcrsqjxpsfu2hjg-postgresql.services.clever-cloud.com",
  port = 8400,
  user = "ugnzz7lvvtl57ckg8oxa",
  password = "dfkakXRYIpvVlD6tBxEvxoC91Ec78r"
)

st_write(capa_obs2019_25831, con, Id(schema="public", table="obs_cat"), append = TRUE)

dbListTables(con)
dbGetQuery(con, "SELECT COUNT(*) FROM public.obs_cat;")

dbDisconnect(con)

#==========================Dades 2020=============================================


obs2020 <- rminka::mnk_obs(year = 2020,quality = "research", bounds = lim_catalunya, limit_download= FALSE,)


capa_obs2020 <-  st_as_sf(obs2020, coords = c("longitude","latitude"),crs =  "WGS84" )


#remobrem l Id per evitar conflictes a Posgres i canviem el CRS

capa_obs2020_25831 <- capa_obs2020 %>%
  rename(id_minka = id) %>%
  st_transform(25831)

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = "bvuvabcrsqjxpsfu2hjg",
  host = "bvuvabcrsqjxpsfu2hjg-postgresql.services.clever-cloud.com",
  port = 8400,
  user = "ugnzz7lvvtl57ckg8oxa",
  password = "dfkakXRYIpvVlD6tBxEvxoC91Ec78r"
)

st_write(capa_obs2020_25831, con, Id(schema="public", table="obs_cat"), append = TRUE)

dbListTables(con)
dbGetQuery(con, "SELECT COUNT(*) FROM public.obs_cat;")

dbDisconnect(con)

#=======================Observacions 2021========================================

obs2021 <- rminka::mnk_obs(year = 2021,quality = "research", bounds = lim_catalunya, limit_download= FALSE,)


capa_obs2021 <-  st_as_sf(obs2021, coords = c("longitude","latitude"),crs =  "WGS84" )


#remobrem l Id per evitar conflictes a Posgres i canviem el CRS

capa_obs2021_25831 <- capa_obs2021 %>%
  rename(id_minka = id) %>%
  st_transform(25831)

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = "bvuvabcrsqjxpsfu2hjg",
  host = "bvuvabcrsqjxpsfu2hjg-postgresql.services.clever-cloud.com",
  port = 8400,
  user = "ugnzz7lvvtl57ckg8oxa",
  password = "dfkakXRYIpvVlD6tBxEvxoC91Ec78r"
)

st_write(capa_obs2021_25831, con, Id(schema="public", table="obs_cat"), append = TRUE)

dbListTables(con)
dbGetQuery(con, "SELECT COUNT(*) FROM public.obs_cat;")

dbDisconnect(con)

#=======================Observacions 2022========================================

obs2022 <- rminka::mnk_obs(year = 2022,quality = "research", bounds = lim_catalunya, limit_download= FALSE,)


capa_obs2022 <-  st_as_sf(obs2022, coords = c("longitude","latitude"),crs =  "WGS84" )


#remobrem l Id per evitar conflictes a Posgres i canviem el CRS

capa_obs2022_25831 <- capa_obs2022 %>%
  rename(id_minka = id) %>%
  st_transform(25831)

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = "bvuvabcrsqjxpsfu2hjg",
  host = "bvuvabcrsqjxpsfu2hjg-postgresql.services.clever-cloud.com",
  port = 8400,
  user = "ugnzz7lvvtl57ckg8oxa",
  password = "dfkakXRYIpvVlD6tBxEvxoC91Ec78r"
)

st_write(capa_obs2022_25831, con, Id(schema="public", table="obs_cat"), append = TRUE)

dbListTables(con)

dbGetQuery(con, "SELECT COUNT(*) FROM public.obs_cat WHERE year = '2022';")

dbGetQuery(con, "SELECT COUNT(*) FROM public.obs_cat;")

dbDisconnect(con)

#=======================Observacions 2023========================================


obs2023 <- rminka::mnk_obs(year = 2023,quality = "research", bounds = lim_catalunya, limit_download= FALSE,)


capa_obs2023 <-  st_as_sf(obs2023, coords = c("longitude","latitude"),crs =  "WGS84" )


#remobrem l Id per evitar conflictes a Posgres i canviem el CRS

capa_obs2023_25831 <- capa_obs2023 %>%
  rename(id_minka = id) %>%
  st_transform(25831)

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = "bvuvabcrsqjxpsfu2hjg",
  host = "bvuvabcrsqjxpsfu2hjg-postgresql.services.clever-cloud.com",
  port = 8400,
  user = "ugnzz7lvvtl57ckg8oxa",
  password = "dfkakXRYIpvVlD6tBxEvxoC91Ec78r"
)

st_write(capa_obs2023_25831, con, Id(schema="public", table="obs_cat"), append = TRUE)

dbListTables(con)

dbGetQuery(con, "SELECT COUNT(*) FROM public.obs_cat;")

dbGetQuery(con, "SELECT COUNT(*) FROM public.obs_cat WHERE year = '2023';")

dbGetQuery(con, "SELECT year, COUNT(*) FROM public.obs_cat GROUP BY year ORDER BY year;")

dbDisconnect(con)


#=======================Observacions 2024========================================


obs2024 <- rminka::mnk_obs(year = 2024,quality = "research", bounds = lim_catalunya, limit_download= FALSE,)


capa_obs2024 <-  st_as_sf(obs2024, coords = c("longitude","latitude"),crs =  "WGS84" )


#remobrem l Id per evitar conflictes a Posgres i canviem el CRS

capa_obs2024_25831 <- capa_obs2024 %>%
  rename(id_minka = id) %>%
  st_transform(25831)

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = "bvuvabcrsqjxpsfu2hjg",
  host = "bvuvabcrsqjxpsfu2hjg-postgresql.services.clever-cloud.com",
  port = 8400,
  user = "ugnzz7lvvtl57ckg8oxa",
  password = "dfkakXRYIpvVlD6tBxEvxoC91Ec78r"
)

st_write(capa_obs2024_25831, con, Id(schema="public", table="obs_cat"), append = TRUE)

dbListTables(con)
dbGetQuery(con, "SELECT COUNT(*) FROM public.obs_cat WHERE year ='2024';")
dbGetQuery(con, "SELECT year, COUNT(*) FROM public.obs_cat GROUP BY year ORDER BY year;")
dbGetQuery(con, "SELECT COUNT(*) FROM public.obs_cat;")
dbDisconnect(con)

#=======================Observacions 2025========================================


obs2025 <- rminka::mnk_obs(year = 2025, quality = "research", bounds = lim_catalunya, limit_download= FALSE,)


capa_obs2025 <-  st_as_sf(obs2025, coords = c("longitude","latitude"),crs =  "WGS84" )


#remobrem l Id per evitar conflictes a Posgres i canviem el CRS

capa_obs2025_25831 <- capa_obs2025 %>%
  rename(id_minka = id) %>%
  st_transform(25831)

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = "bvuvabcrsqjxpsfu2hjg",
  host = "bvuvabcrsqjxpsfu2hjg-postgresql.services.clever-cloud.com",
  port = 8400,
  user = "ugnzz7lvvtl57ckg8oxa",
  password = "dfkakXRYIpvVlD6tBxEvxoC91Ec78r"
)

st_write(capa_obs2025_25831, con, Id(schema="public", table="obs_cat"), append = TRUE)

dbListTables(con)
dbGetQuery(con, "SELECT COUNT(*) FROM public.obs_cat WHERE year = 2025;")
dbGetQuery(con, "SELECT year, COUNT(*) FROM public.obs_cat GROUP BY year ORDER BY year;")
dbGetQuery(con, "SELECT  COUNT(*) FROM public.obs_cat;")
dbDisconnect(con)

#=================================================================================
#=======================Observacions 2026========================================


obs2025_gen <- rminka::mnk_obs(year = 2026, month= 1, quality = "research", bounds = lim_catalunya, limit_download= FALSE,)

View(obs2025_gen)

obs2025_feb <- rminka::mnk_obs(year = 2026, month= 2, quality = "research", bounds = lim_catalunya, limit_download= FALSE,)

obs2025_mar <- rminka::mnk_obs(year = 2026, month= 3, quality = "research", bounds = lim_catalunya, limit_download= FALSE,)

obs2025_abr <- rminka::mnk_obs(year = 2026, month= 4, quality = "research", bounds = lim_catalunya, limit_download= FALSE,)

obs2025_mai <- rminka::mnk_obs(year = 2026, month= 5, quality = "research", bounds = lim_catalunya, limit_download= FALSE,)

obs2025_jun <- rminka::mnk_obs(year = 2026, month= 6, quality = "research", bounds = lim_catalunya, limit_download= FALSE,)

obs2025_jul <- rminka::mnk_obs(year = 2026, month= 7, quality = "research", bounds = lim_catalunya, limit_download= FALSE,)

obs2025_ago<- rminka::mnk_obs(year = 2026, month= 8, quality = "research", bounds = lim_catalunya, limit_download= FALSE,)

obs2026 <- rbind(obs2025_gen,obs2025_feb)
obs2026 <- rbind(obs2026,obs2025_mar)
obs2026 <- rbind(obs2026,obs2025_abr)
obs2026 <- rbind(obs2026,obs2025_mai)
obs2026 <- rbind(obs2026,obs2025_jun)
obs2026 <- rbind(obs2026,obs2025_jul)
obs2026 <- rbind(obs2026,obs2025_ago)

capa_obs2026 <-  st_as_sf(obs2026, coords = c("longitude","latitude"),crs =  "WGS84" )


#remobrem l Id per evitar conflictes a Posgres i canviem el CRS

capa_new <- capa_obs2026 %>%
  rename(id_minka = id) %>%
  st_transform(25831)%>%
  rename(geom = geometry) %>%
  st_set_geometry("geom") %>%
  st_make_valid()


# 2. transforma
capa_new <- st_as_sf(obs2026, coords=c("longitude","latitude"), crs="WGS84") %>%
  rename(id_minka = id) %>%
  st_transform(25831) %>%
  rename(geom = geometry) %>%
  st_set_geometry("geom")

cat("Total descargados:", nrow(capa_new), "\n")
cat("No válidos:", sum(!st_is_valid(capa_new)), "\n")
cat("Vacíos:", sum(st_is_empty(capa_new)), "\n")
cat("Duplicados id_minka en el df:", sum(duplicated(capa_new$id_minka)), "\n")

# repara automáticamente
capa_new <- capa_new %>%
  filter(!st_is_empty(geom)) %>%
  st_make_valid()



###############################################################################
#############################################################################

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = "bvuvabcrsqjxpsfu2hjg",
  host = "bvuvabcrsqjxpsfu2hjg-postgresql.services.clever-cloud.com",
  port = 8400,
  user = "ugnzz7lvvtl57ckg8oxa",
  password = "dfkakXRYIpvVlD6tBxEvxoC91Ec78r"
)

#Obetnim el timestamp de l ultima sincro

last_sync <- dbGetQuery(con, "SELECT MAX(updated_at) as last FROM public.obs_cat")$last

#Apliquem aquet timestamp a la darrera update de les dades amb conexio a l API

actualitzacio <- rminka::mnk_obs(updated_since = last_sync, 
                                 quality="research", 
                                 bounds=lim_catalunya, 
                                 limit_download=FALSE)

actualitzacio <- rminka::mnk_obs(
  updated_since = "2026-09-01", # o el MAX(updated_at) de la teva BBDD
  quality = "research", 
  bounds = lim_catalunya, 
  limit_download = FALSE
)

#Ho pasem a capa geografica

capa_new <- actualitzacio %>%
  rename(id_minka = id) %>%
  st_as_sf(coords=c("longitude","latitude"), crs=4326) %>%
  st_transform(25831) %>%
  rename(geom = geometry) %>% st_set_geometry("geom") %>%
  filter(!st_is_empty(geom)) %>% st_make_valid()

#reparacio del link de les imatges.......

capa_new$url_picture <- gsub("/square\\.", "/original.", capa_new$url_picture)
capa_new$url_picture <- gsub("https://minka-sdg.org", "https://observe.minka-sdg.org", capa_new$url_picture)

#Estadistiques previes

cat("Total descargados:", nrow(capa_new), "\n")
cat("No válidos:", sum(!st_is_valid(capa_new)), "\n")
cat("Vacíos:", sum(st_is_empty(capa_new)), "\n")
cat("Duplicados id_minka en el df:", sum(duplicated(capa_new$id_minka)), "\n")

# repara automáticamente

capa_new <- capa_new %>%
  filter(!st_is_empty(geom)) %>%
  st_make_valid()

# --- CHECK 2: conteig avans en BBDD ---
before <- dbGetQuery(con, "SELECT COUNT(*) as total, COUNT(*) FILTER (WHERE year=2026) as y2026 FROM public.obs_cat")
print(before)

# 3. staging - sobreescribim sempre

st_write(capa_new, con, Id(schema="public", table="obs_cat_staging"), delete_layer = TRUE)

# 4. UPSERT automatic

dbExecute(con, "
INSERT INTO public.obs_cat (id_minka, observed_on, year, month, week, day, hour, created_at, updated_at, positional_accuracy, geoprivacy, obscured, uri, url_picture, quality_grade, taxon_id, taxon_name, taxon_rank, taxon_min_ancestry, taxon_endemic, taxon_threatened, taxon_introduced, taxon_native, user_id, user_login, geom, utm10_code, utm1_code, codi_hic, cod_corine, habitat_id, profunditat, substrat_gid)
SELECT id_minka, observed_on, year, month, week, day, hour, created_at, updated_at, positional_accuracy, geoprivacy, obscured, uri, url_picture, quality_grade, taxon_id, taxon_name, taxon_rank, taxon_min_ancestry, taxon_endemic, taxon_threatened, taxon_introduced, taxon_native, user_id, user_login, geom, NULL,NULL,NULL,NULL,NULL,NULL,NULL
FROM public.obs_cat_staging
ON CONFLICT (id_minka) DO UPDATE SET
  observed_on = EXCLUDED.observed_on,
  year = EXCLUDED.year, month = EXCLUDED.month, week = EXCLUDED.week, day = EXCLUDED.day, hour = EXCLUDED.hour,
  created_at = EXCLUDED.created_at, updated_at = EXCLUDED.updated_at,
  positional_accuracy = EXCLUDED.positional_accuracy, geoprivacy = EXCLUDED.geoprivacy, obscured = EXCLUDED.obscured,
  uri = EXCLUDED.uri, url_picture = EXCLUDED.url_picture, quality_grade = EXCLUDED.quality_grade,
  taxon_id = EXCLUDED.taxon_id, taxon_name = EXCLUDED.taxon_name, taxon_rank = EXCLUDED.taxon_rank, taxon_min_ancestry = EXCLUDED.taxon_min_ancestry,
  taxon_endemic = EXCLUDED.taxon_endemic, taxon_threatened = EXCLUDED.taxon_threatened, taxon_introduced = EXCLUDED.taxon_introduced, taxon_native = EXCLUDED.taxon_native,
  user_id = EXCLUDED.user_id, user_login = EXCLUDED.user_login,
  geom = EXCLUDED.geom,
  utm10_code = NULL, utm1_code = NULL, codi_hic = NULL, cod_corine = NULL, habitat_id = NULL, profunditat = NULL, substrat_gid = NULL
")

#Comprobacio que hem pujat observacions

dbGetQuery(con, "
SELECT 
  COUNT(*) as staging,
  COUNT(*) FILTER (WHERE geom IS NULL) as geom_null,
  COUNT(*) FILTER (WHERE NOT ST_IsValid(geom)) as invalid
FROM public.obs_cat_staging
")

# 5. rRecaluem els creuaments per les noves dades o les actualitzades amb les capes mediambientals

dbExecute(con, "UPDATE public.obs_cat o SET utm10_code = u.coord_10k FROM public.utm10_litoral u WHERE o.id_minka IN (SELECT id_minka FROM obs_cat_staging) AND ST_Intersects(o.geom, u.geom)")
dbExecute(con, "UPDATE public.obs_cat o SET utm1_code = u.cod1x1 FROM public.utm1_litoral u WHERE o.id_minka IN (SELECT id_minka FROM obs_cat_staging) AND ST_Intersects(o.geom, u.geom)")
dbExecute(con, "UPDATE public.obs_cat o SET habitat_id = h.id, cod_corine = h.cod_corine, codi_hic = h.codi_hic FROM public.habitats_simple h WHERE o.id_minka IN (SELECT id_minka FROM obs_cat_staging) AND ST_Intersects(o.geom, h.geom)")
dbExecute(con, "UPDATE public.obs_cat o SET substrat_gid = s.gid FROM public.substrat_fons_2m s WHERE o.id_minka IN (SELECT id_minka FROM obs_cat_staging) AND ST_Intersects(o.geom, s.geom)")
dbExecute(con, "UPDATE public.obs_cat o SET profunditat = (SELECT ST_Value(b.rast, o.geom) FROM public.batimetria_5m_mar b WHERE ST_Intersects(b.rast, o.geom) LIMIT 1) WHERE o.id_minka IN (SELECT id_minka FROM obs_cat_staging) AND o.profunditat IS NULL")
dbExecute(con, "DROP TABLE public.obs_cat_staging")

#Comprobacio despres del creuamnet de les capes

after <- dbGetQuery(con, "
SELECT 
  COUNT(*) as total,
  COUNT(*) FILTER (WHERE year=2026) as y2026,
  COUNT(*) FILTER (WHERE year=2026 AND utm10_code IS NULL) as sin_utm10,
  COUNT(*) FILTER (WHERE year=2026 AND profunditat IS NULL) as sin_bati,
  COUNT(*) FILTER (WHERE year=2026 AND substrat_gid IS NULL) as sin_substrat
FROM public.obs_cat;
")
print(after)

# geometries trencades despres del creuament

dbGetQuery(con, "SELECT id_minka FROM public.obs_cat WHERE NOT ST_IsValid(geom) LIMIT 10")

DBI::dbGetQuery(con, "SELECT year, COUNT(*) FROM public.obs_cat GROUP BY year ORDER BY year DESC")

DBI::dbDisconnect(con)



