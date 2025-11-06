
ITA <- read.table("student_ita.txt")

## school ID

head(as.character(ITA$CNTSCHID))

## student id

head(as.character(ITA$CNTSTUID))

## grade

table(ITA$ST001D01T)

# gender, male =0, female =1

table(ITA$ST004D01T)
gender =rep(0, dim(ITA)[1])
for(i in 1: dim(ITA)[1]){
  
  if(ITA$ST004D01T[i]==1) gender[i]=1
  else if(ITA$ST004D01T[i]==2) gender[i]=0
}
table(gender)

## immigrant status, 0 is native, 1 and 2 are first ad second generation respectively

table(ITA$IMMIG)
immig = rep(NA, dim(ITA)[1])

for(i in 1: dim(ITA)[1]){
  if(is.na(ITA$IMMIG[i])==F){
    if(ITA$IMMIG[i]==1) immig[i]= 0
    else if(ITA$IMMIG[i]==2) immig[i]= 1
    else if(ITA$IMMIG[i]==3) immig[i]= 2
  }
}
table(immig)



## breakfast (not present in Italian data)

## video_games

head(ITA$ST078Q06NA)
#IC001Q05TA: video games available  to use at home
table(ITA$IC001Q05TA,useNA = "always")
video_games = ifelse(ITA$IC001Q05TA==1,1,ifelse(ITA$IC001Q05TA==2 | ITA$IC001Q05TA==3, 0, NA))
table(video_games, useNA = "always")

#internet 
#IC008Q08TA: digital devices outside school
table(ITA$IC008Q08TA,useNA = "always")
internet = ifelse(ITA$IC008Q08TA %in% c(1,2,3),0,ifelse(ITA$IC008Q08TA %in% c(4,5), 1, NA))
table(internet, useNA = "always")





## discriminate and school climate
summary(ITA$DISCRIM)

## sense of belonging to school
summary(ITA$BELONG)

## perception of competitiveness in the school
summary(ITA$PERCOMP)

## cooperation and teamwork disposition
summary(ITA$PERCOOP)


## SUPPORT
## support of teacher
summary(ITA$TEACHSUP)

## emosups, parents emotional support
summary(ITA$EMOSUPS)

## SELF PERCEPTION

## cognitive adaptability flexibility
summary(ITA$COGFLEX)

## general fear of failure
summary(ITA$GFOFAIL)

## compete
summary(ITA$COMPETE)

## sense of life
summary(ITA$EUDMO)

## STUDY

## mmins (learning time in minutes per maths per week)
head(ITA$MMINS)

## tmins (learning time in minutes per all subjects per week)
head(ITA$TMINS)

## EDUCATION AT HOME AND ESCS

## highest level of schooling of mum

table(ITA$MISCED,useNA = "always")
misced=ITA$MISCED
table(misced,useNA = "always")


table(ITA$FISCED, useNA = "always")
fisced=ITA$FISCED


table(ITA$HISCED,useNA = "always")
hisced=ITA$HISCED
table(hisced)

## cultural possession at home
summary(ITA$CULTPOSS)

## home educational resources
summary(ITA$HEDRES)

## ESCS
summary(ITA$ESCS)


## ORIENTATION isced 0

table(ITA$ISCEDO,useNA = "always")

iscedo = rep(NA, dim(ITA)[1])
for(i in 1: dim(ITA)[1]){
  if(is.na(ITA$ISCEDO[i])==F){
    if(ITA$ISCEDO[i]==1) iscedo[i]="general"
    if(ITA$ISCEDO[i]==3) iscedo[i]="vocational"
  }
}

table(iscedo)

## PV 1,2,..10
## the correlation across each pair is around 0.87
## we keep pv1maths
pv1maths = (ITA$PV1MATH - mean(ITA$PV1MATH))/sd(ITA$PV1MATH)
hist(pv1maths)


student_ita = data.frame(as.character(ITA$CNTSCHID), as.character(ITA$CNTSTUID), ITA$ST001D01T, gender,immig,
                         video_games, internet, ITA$MMINS, ITA$TMINS, 
                         ITA$DISCRIM, ITA$BELONG, ITA$PERCOMP, ITA$PERCOOP,
                         ITA$TEACHSUP, ITA$EMOSUPS,
                         ITA$COGFLEX, ITA$GFOFAIL, ITA$COMPETE, ITA$EUDMO,
                         misced, ITA$CULTPOSS, ITA$HEDRES, ITA$ESCS,
                         iscedo,
                         pv1maths)


names(student_ita) = c("school_id", "student_id", "grade", "gender","immig",
                       "video_games","internet","mmins","tmins",
                       "sc_DISCRIM", "sc_BELONG", "sc_PERCOMP", "sc_PERCOOP",
                       "TEACHSUP", "EMOSUPS",
                       "COGFLEX", "GFOFAIL", "COMPETE", "EUDMO",
                       "misced","cultural_possession","home_educ_resources","ESCS", 
                       "iscedo",
                       "math_PISA_score")

dim(na.omit(student_ita))
colSums(is.na(student_ita))


#############################################################################

## save character and dummy as factor
student_ita$gender        <- as.factor(student_ita$gender) # male =0, female =1
student_ita$immig         <- as.factor(student_ita$immig)
student_ita$video_games   <- as.factor(student_ita$video_games)
student_ita$internet      <- as.factor(student_ita$internet)
student_ita$school_id     <- as.factor(student_ita$school_id)
student_ita$misced        <- as.numeric(student_ita$misced)
student_ita$iscedo        <- as.factor(student_ita$iscedo)
student_ita$iscedo        <- relevel(student_ita$iscedo, ref='general')

## create early/late enrollment standing on the grade 
student_ita$late          <- as.factor(ifelse(student_ita$grade < 10,1,0)) #as.factor
student_ita$early         <- as.factor(ifelse(student_ita$grade > 10,1,0)) #as.factor
student_ita$grade         <- NULL

## check for NA
colSums(is.na(student_ita)) ## NA quite distributed across the sample
student_ita_nona            <- na.omit(student_ita)  
student_ita_nona$school_id  <- as.character(student_ita_nona$school_id)


table1 <- 
  student_ita_nona %>%
  tbl_summary(include = names(student_ita_nona)[-1])
table1
corrplot::corrplot(cor(student_ita_nona[,c(8:21,23)]))


# build 2 dataset iscedo= "general" e iscedo ="vocational" come 2  studi separati

data_gen <- student_ita_nona[student_ita_nona$iscedo=="general",]
data_gen$iscedo <- NULL
dim(data_gen)

## data_gen is the dataset used in the analysis of the paper

