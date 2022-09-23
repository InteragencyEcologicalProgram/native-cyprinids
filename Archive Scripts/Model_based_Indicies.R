# ***Model based Indicies ---------------------------------------------------------

# Going to fit mixed effects model to predict/estimate regional abundance


# SAPM --------------------------------------------------------------------

head(SAPM_Index)
str(SAPM_Index)

# SPLT --------------------------------------------------------------------

head(SPLT_Index)

SPLT_Index_SJ <- 
  SPLT_Index %>% 
  filter(Region == "San_Joaquin")
head(SPLT_Index_SJ)

SPLT_Index_Sac <- 
  SPLT_Index %>% 
  filter(Region == "Sacramento")

SPLT_Index_Delta <- 
  SPLT_Index %>% 
  filter(Region == "Delta")
str(SPLT_Index_Delta)


SAPM_Abun_glmer <- 
  glmer(round(Adjusted_Count) ~ offset(Volume_Z) + Julian_Z + 
          Julian_Sq_Z  + Year + (1|subarea), 
        data = SAPM_Index, family = poisson(link = "log"))

SAPM_Index_Abr <- 
  SAPM_Index %>% 
  filter(Region != "San_Joaquin")

SAPM_Abun_glmer <- 
  glmer(round(Adjusted_Count) ~ offset(Volume_Z) + Julian_Z + 
          Julian_Sq_Z  + Year + Region + Year*Region + (1|subarea), 
        data = SAPM_Index_Abr, family = poisson(link = "log"))


SPLT_Abun_glmer <- 
  glmer(round(Adjusted_Count) ~ offset(Volume_Z) + Julian_Z + 
          Julian_Sq_Z  + Year + Region + Year*Region + (1|subarea), 
        data = SPLT_Index, family = poisson(link = "log"))

SASU_Abun_glmer <- 
  glmer(round(Adjusted_Count) ~ offset(Volume_Z) + Julian_Z + 
          Julian_Sq_Z  + Year + Region + Year*Region + (1|subarea), 
        data = SASU_Index, family = poisson(link = "log"))

SPLT_Abun_SJ_glmer <- 
  glmer(round(Adjusted_Count) ~ offset(Volume_Z) + Julian_Z + 
          Julian_Sq_Z  + Year + (1|subarea), 
        data = SPLT_Index_SJ, family = poisson(link = "log"))

SPLT_Abun_SAC_glmer <- 
  glmer(round(Adjusted_Count) ~ offset(Volume_Z) + Julian_Z + 
          Julian_Sq_Z  + Year + (1|subarea), 
        data = SPLT_Index_Sac, family = poisson(link = "log"))

SPLT_Abun_Delta_glmer <- 
  glmer(round(Adjusted_Count) ~ offset(Volume_Z) + Julian_Z + 
          Julian_Sq_Z  + Year + (1|subarea), 
        data = SPLT_Index_Delta, family = poisson(link = "log"))




