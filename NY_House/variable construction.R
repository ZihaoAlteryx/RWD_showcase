data = read.csv("cleaned_data_v3.csv")
library(dplyr)
library(stringr)


# Create 3 sizes
data = data %>%
  mutate(lotarea = LTFRONT * LTDEPTH) %>%
  mutate(bldarea = BLDFRONT * BLDDEPTH) %>%
  mutate(bldvol = bldarea * STORIES)%>%
  mutate(ZIP3 = substr(ZIP,1,3))

# 9 variables
data1 = data %>%
  mutate(FV_lotarea = FULLVAL/lotarea) %>%
  mutate(FV_bldarea = FULLVAL/bldarea) %>%
  mutate(FV_bldvol = FULLVAL/bldvol) %>%
  mutate(AL_lotarea = AVLAND/lotarea) %>%
  mutate(AL_bldarea = AVLAND/bldarea) %>%
  mutate(AL_bldvol = AVLAND/bldvol) %>%
  mutate(AT_lotarea = AVTOT/lotarea) %>%
  mutate(AT_bldarea = AVTOT/bldarea) %>%
  mutate(AT_bldvol = AVTOT/bldvol)

# Group by ZIP 5, ZIP 3, BOROUGH, TAXCLASS
data2 = data1%>%
  group_by(ZIP) %>%
  summarize(avg_FV_lotarea = mean(FV_lotarea),
            avg_FV_bldarea = mean(FV_bldarea),
            avg_FV_bldvol = mean(FV_bldvol),
            avg_AL_lotarea = mean(AL_lotarea),
            avg_AL_bldarea = mean(AL_bldarea),
            avg_AL_bldvol = mean(AL_bldvol),
            avg_AT_lotarea = mean(AT_lotarea),
            avg_AT_bldarea = mean(AT_bldarea),
            avg_AT_bldvol = mean(AT_bldvol))
data3 = left_join(data1,data2,by = c("ZIP" = "ZIP"))
data4 = data1%>%
  group_by(ZIP3) %>%
  summarize(avg_FV_lotarea3 = mean(FV_lotarea),
            avg_FV_bldarea3 = mean(FV_bldarea),
            avg_FV_bldvol3 = mean(FV_bldvol),
            avg_AL_lotarea3 = mean(AL_lotarea),
            avg_AL_bldarea3 = mean(AL_bldarea),
            avg_AL_bldvol3 = mean(AL_bldvol),
            avg_AT_lotarea3 = mean(AT_lotarea),
            avg_AT_bldarea3 = mean(AT_bldarea),
            avg_AT_bldvol3 = mean(AT_bldvol))
data3 = left_join(data3,data4,by = c("ZIP3" = "ZIP3"))
data5 = data1%>%
  group_by(TAXCLASS) %>%
  summarize(avg_FV_lotareaT = mean(FV_lotarea),
            avg_FV_bldareaT = mean(FV_bldarea),
            avg_FV_bldvolT = mean(FV_bldvol),
            avg_AL_lotareaT = mean(AL_lotarea),
            avg_AL_bldareaT = mean(AL_bldarea),
            avg_AL_bldvolT = mean(AL_bldvol),
            avg_AT_lotareaT = mean(AT_lotarea),
            avg_AT_bldareaT = mean(AT_bldarea),
            avg_AT_bldvolT = mean(AT_bldvol))
data3 = left_join(data3,data5,by = c("TAXCLASS" = "TAXCLASS"))
data6 = data1%>%
  group_by(BOROUGH) %>%
  summarize(avg_FV_lotareaB = mean(FV_lotarea),
            avg_FV_bldareaB = mean(FV_bldarea),
            avg_FV_bldvolB = mean(FV_bldvol),
            avg_AL_lotareaB = mean(AL_lotarea),
            avg_AL_bldareaB = mean(AL_bldarea),
            avg_AL_bldvolB = mean(AL_bldvol),
            avg_AT_lotareaB = mean(AT_lotarea),
            avg_AT_bldareaB = mean(AT_bldarea),
            avg_AT_bldvolB = mean(AT_bldvol))
data3 = left_join(data3,data6,by = c("BOROUGH" = "BOROUGH"))

data7 = data1 %>%
  group_by(STORIES,ZIP3) %>%
  summarize(avg_FV_lotareaSZ = mean(FV_lotarea),
            avg_FV_bldareaSZ = mean(FV_bldarea),
            avg_FV_bldvolSZ = mean(FV_bldvol),
            avg_AL_lotareaSZ = mean(AL_lotarea),
            avg_AL_bldareaSZ = mean(AL_bldarea),
            avg_AL_bldvolSZ = mean(AL_bldvol),
            avg_AT_lotareaSZ = mean(AT_lotarea),
            avg_AT_bldareaSZ = mean(AT_bldarea),
            avg_AT_bldvolSZ = mean(AT_bldvol))
data3 = left_join(data3,data7,by = c("STORIES" = "STORIES", "ZIP3" = "ZIP3"))

data8 = data1 %>%
  group_by(STORIES,TAXCLASS) %>%
  summarize(avg_FV_lotareaST = mean(FV_lotarea),
            avg_FV_bldareaST = mean(FV_bldarea),
            avg_FV_bldvolST = mean(FV_bldvol),
            avg_AL_lotareaST = mean(AL_lotarea),
            avg_AL_bldareaST = mean(AL_bldarea),
            avg_AL_bldvolST = mean(AL_bldvol),
            avg_AT_lotareaST = mean(AT_lotarea),
            avg_AT_bldareaST = mean(AT_bldarea),
            avg_AT_bldvolST = mean(AT_bldvol))
data3 = left_join(data3,data8,by = c("STORIES" = "STORIES", "TAXCLASS" = "TAXCLASS"))

data9 = data1 %>%
  group_by(ZIP3,TAXCLASS) %>%
  summarize(avg_FV_lotareaZT = mean(FV_lotarea),
            avg_FV_bldareaZT = mean(FV_bldarea),
            avg_FV_bldvolZT = mean(FV_bldvol),
            avg_AL_lotareaZT = mean(AL_lotarea),
            avg_AL_bldareaZT = mean(AL_bldarea),
            avg_AL_bldvolZT = mean(AL_bldvol),
            avg_AT_lotareaZT = mean(AT_lotarea),
            avg_AT_bldareaZT = mean(AT_bldarea),
            avg_AT_bldvolZT = mean(AT_bldvol))
data3 = left_join(data3,data9,by = c("ZIP3" = "ZIP3", "TAXCLASS" = "TAXCLASS"))

data = data3

data = data %>%
  mutate(r_FV_lotarea5 = FV_lotarea/avg_FV_lotarea,
         r_FV_bldarea5 = FV_bldarea/avg_FV_bldarea,
         r_FV_bldvol5 = FV_bldvol/avg_FV_bldvol,
         r_AL_lotarea5 = AL_lotarea/avg_AL_lotarea,
         r_AL_bldarea5 = AL_bldarea/avg_AL_bldarea,
         r_AL_bldvol5 = AL_bldvol/avg_AL_bldvol,
         r_AT_lotarea5 = AT_lotarea/avg_AT_lotarea,
         r_AT_bldarea5 = AT_bldarea/avg_AT_bldarea,
         r_AT_bldvol5 = AT_bldvol/avg_AT_bldvol
)

data = data %>%
  mutate(r_FV_lotarea3 = FV_lotarea/avg_FV_lotarea3,
         r_FV_bldarea3 = FV_bldarea/avg_FV_bldarea3,
         r_FV_bldvol3 = FV_bldvol/avg_FV_bldvol3,
         r_AL_lotarea3 = AL_lotarea/avg_AL_lotarea3,
         r_AL_bldarea3 = AL_bldarea/avg_AL_bldarea3,
         r_AL_bldvol3 = AL_bldvol/avg_AL_bldvol3,
         r_AT_lotarea3 = AT_lotarea/avg_AT_lotarea3,
         r_AT_bldarea3 = AT_bldarea/avg_AT_bldarea3,
         r_AT_bldvol3 = AT_bldvol/avg_AT_bldvol3
  )

data = data %>%
  mutate(r_FV_lotareaT = FV_lotarea/avg_FV_lotareaT,
         r_FV_bldareaT = FV_bldarea/avg_FV_bldareaT,
         r_FV_bldvolT = FV_bldvol/avg_FV_bldvolT,
         r_AL_lotareaT = AL_lotarea/avg_AL_lotareaT,
         r_AL_bldareaT = AL_bldarea/avg_AL_bldareaT,
         r_AL_bldvolT = AL_bldvol/avg_AL_bldvolT,
         r_AT_lotareaT = AT_lotarea/avg_AT_lotareaT,
         r_AT_bldareaT = AT_bldarea/avg_AT_bldareaT,
         r_AT_bldvolT = AT_bldvol/avg_AT_bldvolT
  )
  
data = data %>%
  mutate(r_FV_lotareaB = FV_lotarea/avg_FV_lotareaB,
         r_FV_bldareaB = FV_bldarea/avg_FV_bldareaB,
         r_FV_bldvolB = FV_bldvol/avg_FV_bldvolB,
         r_AL_lotareaB = AL_lotarea/avg_AL_lotareaB,
         r_AL_bldareaB = AL_bldarea/avg_AL_bldareaB,
         r_AL_bldvolB = AL_bldvol/avg_AL_bldvolB,
         r_AT_lotareaB = AT_lotarea/avg_AT_lotareaB,
         r_AT_bldareaB = AT_bldarea/avg_AT_bldareaB,
         r_AT_bldvolB = AT_bldvol/avg_AT_bldvolB
  )

data = data %>%
  mutate(r_FV_lotareaS3 = FV_lotarea/avg_FV_lotareaSZ,
         r_FV_bldareaS3 = FV_bldarea/avg_FV_bldareaSZ,
         r_FV_bldvolS3 = FV_bldvol/avg_FV_bldvolSZ,
         r_AL_lotareaS3 = AL_lotarea/avg_AL_lotareaSZ,
         r_AL_bldareaS3 = AL_bldarea/avg_AL_bldareaSZ,
         r_AL_bldvolS3 = AL_bldvol/avg_AL_bldvolSZ,
         r_AT_lotareaS3 = AT_lotarea/avg_AT_lotareaSZ,
         r_AT_bldareaS3 = AT_bldarea/avg_AT_bldareaSZ,
         r_AT_bldvolS3 = AT_bldvol/avg_AT_bldvolSZ
  )

data = data %>%
  mutate(r_FV_lotareaST = FV_lotarea/avg_FV_lotareaST,
         r_FV_bldareaST = FV_bldarea/avg_FV_bldareaST,
         r_FV_bldvolST = FV_bldvol/avg_FV_bldvolST,
         r_AL_lotareaST = AL_lotarea/avg_AL_lotareaST,
         r_AL_bldareaST = AL_bldarea/avg_AL_bldareaST,
         r_AL_bldvolST = AL_bldvol/avg_AL_bldvolST,
         r_AT_lotareaST = AT_lotarea/avg_AT_lotareaST,
         r_AT_bldareaST = AT_bldarea/avg_AT_bldareaST,
         r_AT_bldvolST = AT_bldvol/avg_AT_bldvolST
  )

data = data %>%
  mutate(r_FV_lotarea3T = FV_lotarea/avg_FV_lotareaZT,
         r_FV_bldarea3T = FV_bldarea/avg_FV_bldareaZT,
         r_FV_bldvol3T = FV_bldvol/avg_FV_bldvolZT,
         r_AL_lotarea3T = AL_lotarea/avg_AL_lotareaZT,
         r_AL_bldarea3T = AL_bldarea/avg_AL_bldareaZT,
         r_AL_bldvol3T = AL_bldvol/avg_AL_bldvolZT,
         r_AT_lotarea3T = AT_lotarea/avg_AT_lotareaZT,
         r_AT_bldarea3T = AT_bldarea/avg_AT_bldareaZT,
         r_AT_bldvol3T = AT_bldvol/avg_AT_bldvolZT
  )

data = data %>%
  mutate(r_FV_lotareaAL = FV_lotarea/mean(FV_lotarea),
         r_FV_bldareaAL = FV_bldarea/mean(FV_bldarea),
         r_FV_bldvolAL = FV_bldvol/mean(FV_bldvol),
         r_AL_lotareaAL = AL_lotarea/mean(AL_lotarea),
         r_AL_bldareaAL = AL_bldarea/mean(AL_bldarea),
         r_AL_bldvolAL = AL_bldvol/mean(AL_bldvol),
         r_AT_lotareaAL = AT_lotarea/mean(AT_lotarea),
         r_AT_bldareaAL = AT_bldarea/mean(AT_bldarea),
         r_AT_bldvolAL= AT_bldvol/mean(AT_bldvol)
  )

data100 = data[,-c(30:92)]

write.csv(data100,"data_72_variables_constructed.csv")
