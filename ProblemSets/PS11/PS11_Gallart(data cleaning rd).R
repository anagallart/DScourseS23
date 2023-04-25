# i know i should use API to get the data into R, but i just haven't
# the data file is too big to upload to github, so download from 
# https://www.census.gov/data/datasets/time-series/demo/cps/cps-supp_cps-repwgt/cps-food-security.2021.html#list-tab-216513607
# to device to use this right now.... i know submitting R code wasn't required for PS11, but I included it to show some of what i've done

# read in csv from repository/ projecrt directory

cpsFS<-read.csv("dec21pub.csv")
dim(cpsFS) # 127489    507
head(cpsFS, n=1)


names(cpsFS)
cpsFS <- cpsFS[cpsFS$PRPERTYP == 2,] #only entries of civilian adults (over 15)
cpsFS <- cpsFS[cpsFS$HRINTSTA == 1,] # only those intwrviewed for CPS
cpsFS <- cpsFS[cpsFS$HRSUPINT == 1,] #only those interviewed with the supplemental Food Security questions

# bought food at supermarket or grocery store last week?
cpsFS <- cpsFS[cpsFS$HES1A%in% c(1, 2), ] # only those who answered yes or no

# bought food elsewhere (stores)?
cpsFS <- cpsFS[cpsFS$HES1B%in% c(1, 2), ] # only those who answered yes or no

# bought food at restaurant, fast food, caf, vending machine?
cpsFS <- cpsFS[cpsFS$HES1C%in% c(1, 2), ] # only those who answered yes or no

# bought any other kind of food?
cpsFS <- cpsFS[cpsFS$HES1D%in% c(1, 2), ] # only those who answered yes or no


## total amount spent on food LAST WEEK 
      #(derived by adding HES2O, HES4O, HES6O, and HES7O) 
        # and subtracting positive values in (HES3O and HES5O )
cpsFS <- cpsFS[cpsFS$HETS8O >= 0 ,]
# typical amount spent on food from all places
cpsFS <- cpsFS[cpsFS$HETS8OU >= 0 ,]

# to buy just enough food to meet needs, would you need to spend more (1) or less (2) or same(3)
cpsFS <- cpsFS[cpsFS$HES8B%in% c(1, 2,3), ]

# in the last year did you ever run short of money 
# and try to make your food or food money go further?
cpsFS <- cpsFS[cpsFS$HES9%in% c(1, 2), ]

# in the last year, did anyone in HH get SNAP or food stamps?
cpsFS <- cpsFS[cpsFS$HESP1%in% c(1, 2), ]

# describe the food eaten in HH (enough of what we want (1), 
                    #     enough but not always what we want (2),
                    #     sometimes not enough  (3),
                    #     often not enough (4))
cpsFS <- cpsFS[cpsFS$HESS1%in% c(1, 2,3,4), ]


### Different statements (1 Often true, 2 Sometimes true, 3 Never true)
# worried food would run out before got money to buy more (in last 12 months)
cpsFS <- cpsFS[cpsFS$HESS2%in% c(1, 2, 3), ]
# worried food would run out before got money to buy more (in last  30 days)
# cpsFS <- cpsFS[cpsFS$HESSM2%in% c(1, 2, 3), ]

# dim(cpsFS) #18758   507

# food we bought didnt last and didnt have money for more (in last 12 months)
cpsFS <- cpsFS[cpsFS$HESS3%in% c(1, 2, 3), ]
# food we bought didnt last and didnt have money for more (in last 30 days)
# cpsFS <- cpsFS[cpsFS$HESSM3%in% c(1, 2, 3), ]

#couldnt afford to eat balanced meals (in last 12 months)
cpsFS <- cpsFS[cpsFS$HESS4%in% c(1, 2, 3), ]
#couldnt afford to eat balanced meals (in last 30 days)
#cpsFS <- cpsFS[cpsFS$HESSM4%in% c(1, 2, 3), ]

# ever cut size of meals or skip meals bc not enough money for food? (last 12 months)
cpsFS <- cpsFS[cpsFS$HESH2%in% c(1, 2), ]
# how often? (months) HESHF2
# last 30 days? = HESHM2
# how many days = HETSHMF2

# Last 12 months, ever eat less than  felt you should bc not enough money for food?
cpsFS <- cpsFS[cpsFS$HESH3%in% c(1, 2), ]
# how often? (months) HESHF3
# last 30 days? = HESHM3
# how many days = HETSHMF3

# Last 12 moths were you ever hungry but didnt eat bc not enough money for food?
cpsFS <- cpsFS[cpsFS$HESH4%in% c(1, 2), ]
# how often? (months) HESHF4
# last 30 days? = HESHM4
# how many days = HETSHMF4

# Last 12 months did you lose weight bv not enouigh food?
cpsFS <- cpsFS[cpsFS$HESH5%in% c(1, 2), ]
# last 30 days? = HESHM5

# from the initial filter, we are only looking at civilian adults (15+ years old: PRPERTYP == 2) 
# who were interviewd for CPS (HRINTSTA == 1)
# and answered supplemental FS questions (HRSUPINT == 1)
cpsFS <- cpsFS[, c("PESEX", "HETENURE", "HEHOUSUT",
                   "HEFAMINC", "HRHTYPE", "PEMARITL", "GEREG", "PRTAGE",
                   "PEEDUCA", "PTDTRACE", "PRCITSHP", "PEMLR", 
                   "PUWK", "PUDIS","PUABSOT", "PEABSRSN", "PEMJOT",
                   "PEHRFTPT", "PEJHRSN", "PREMPNOT", "PRUNTYPE",
                   # Food security questions
                   "HES1A", "HES1B", "HES1C", "HES1D", "HETS8O", 
                   "HETS8OU", "HES8B", "HES9", "HESP1", "HESS1",
                   "HESS2","HESS3", "HESS4","HESH2", "HESH3", 
                   "HESH4", "HESH5")]
 
dim(cpsFS) # 8060   38
######################################
# look at variables and condense possible variables
######################################

### HEHOUSUT
table(cpsFS$HEHOUSUT) # looking at this i think HETENURE would be a better variable to use
# cpsFS <- cpsFS[cpsFS$HEHOUSUT%in% c(1, 5,6),] # going to merge 5 and 6 responses into mobile home

table(cpsFS$PEMARITL) # this is better than HRHTYPE variable, same type of imfo

table(cpsFS$PEEDUCA) 

# responses 16-26 combine into multi-racial
# responses 06-15 combine into biracial


