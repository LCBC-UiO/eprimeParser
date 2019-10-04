library(dplyr)

eprime_tasks <- list(
  "n-back" = list(cols = c("Running",
                           "Pic",
                           "CR",
                           "StimTest.ACC",
                           "StimTest.RT",
                           "StimTest.RESP",
                           "StimTest.CRESP",
                           "Stim.ACC",
                           "Stim.RT",
                           "Stim.RESP",
                           "Stim.CRESP"),
                  nlines = 6113),
  "Attention" = list(cols = c("Running",
                              "WarningType",
                              "FlankerType",
                              "TargetType1",
                              "TargerDirection",
                              "Sample",
                              "PracSlideTarget.ACC",
                              "PracSlideTarget.RT",
                              "PracSlideTarget.RESP",
                              "PracSlideTarget.CRESP",
                              "TargetType",
                              "TargetDirection",
                              "SlideTarget.ACC",
                              "SlideTarget.RT",
                              "SlideTarget.RESP",
                              "SlideTarget.CRESP"),
                     nlines = 13289),
  "Antisaccade" = list(cols = c("Running",
                                "Arrow",
                                "CR",
                                "Eprime.LevelName",
                                "Sample",
                                "Target1.ACC",
                                "Target1.RT",
                                "Target1.RESP",
                                "Target1.CRESP",
                                "TrialList",
                                "Target.ACC",
                                "Target.RT",
                                "Target.RESP",
                                "Target.CRESP"),
                       nlines = 13000),
  "Letter_memory" = list(cols = c("Running", 
                                  "Eprime.LevelName",
                                  "TestList",
                                  "TestL1", "TestL2", "TestL3", "TestL4", "TestL5", "TestL6", "TestL7",
                                  "Cycle", 
                                  "Sample", 
                                  "L01", "L02", "L03", "L04", "L05", "L06","L07","L08","L09","L10","L11"),
                         nlines = 640)
)



projects <- tibble(
  Project_Name = c(rep("MemP",2),rep("NCP",3), rep("MemC", 2), "ACon",
                   rep("NDev", 3), "Loci", 
                   "MoBa", rep("S2C", 2)),
  name = c(c("HUK", "MemP"), c("NCP", "nevrokogplas", "neurocogplas"), c("MemC", "memconstruct"), "ACon",
           c("NDev", "nevrodev", "neurodev"), "Loci",
           "MoBa", c("S2C", "SetToChange"))
) %>% 
  mutate(Project_Number = case_when(
    Project_Name == "NDev" ~ 10,
    Project_Name == "MemP" ~ 11,
    Project_Name == "NCP" ~ 12,
    Project_Name == "MoBa" ~ 13,
    Project_Name == "Loci" ~ 14,
    Project_Name == "MemC" ~ 15,
    Project_Name == "ACon" ~ 16,
    Project_Name == "S2C" ~ 17,
  )) %>% 
  arrange(Project_Number)


usethis::use_data(eprime_tasks, projects, overwrite = TRUE, internal = TRUE)
