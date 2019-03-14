





# Comparing different ways to reshape. base::reshape on a data.table vs. dcast on a data.table
"To melt or cast data.tables, it is not necessary to load reshape2 anymore. If you have load reshape2, do so before loading data.table to prevent unwanted masking."

xxx = Sys.time()
antibiotic_trans = dcast(tempDT,
  admission_id + bcx_drawn_day + day ~ counter, value.var = 'lag_ab')
Sys.time() - xxx
#Time difference of 1.094 secs
setnames(antibiotic_trans, as.character(1:nab), paste('lag_ab', as.character(1:nab), sep = ''))

# This took a VERY long time to run. Probably several orders of magnitude more.
if(FALSE){
xxx = Sys.time()
antibiotic_trans = reshape(tempDT,
  direction = 'wide',
  v.names = 'lag_ab',
  idvar = 'groupId',
  #idvar = c('admission_id', 'bcx_drawn_day', 'day'),
  timevar = 'counter',
  sep = '')
Sys.time() - xxx



## MWE to try indicating first row in group

dat = data.table(color = sample(Cs(red, green, blue), 20, replace = TRUE), grp = sample(Cs(A, B), 20, replace = TRUE), x = sample(1:7, 20, replace = TRUE), 
  y = sample(1:7, 20, replace = TRUE), key = c('color', 'grp'))

dat[ , firstInd := 0]
# dat[ , firstInd := 1, mult = 'first']


# Returns all of dat
dat[ , .SD]


# Returns first row of data
dat[.SD[1]]



# Returns first row within each group of color
dat2 = dat[ , .SD[1:5], by = color]
dat2[ , .N, by = .(color)]


# Returns first row within each group of color-grp combination
dat[ , .SD[1], by = .(color, grp)]


# Returns all the indices
dat[, .I]


# Returns the index of the first row within each group of color-grp combination.
dat[ , .I[1], by = .(color, grp)]


# Make a variable indicating that it is the first row of each color-grp combination
dat[ , firstInd := 0]
dat[, firstInd := c(1L, firstInd[-1]), by = .(color, grp)]   ## must have the "L" after 1, as 1L

# Alternate!
dat[dat[, .I[1], by = .(color, grp)]$V1, firstInd := 1]



.SD, .BY, .N, .I and .GRP are read only symbols for use in j. .N can be used in i as well.
What does .BY do?


# Add a group index 
dat[ , groupIndex := .GRP, by = .(color, grp)]



dt[ , rownums := .I]

.BY


.I
.I is an integer vector equal to seq_len(nrow(x)). While grouping, it holds for each item in
the group, it’s row location in x. This is useful to subset in j; e.g. DT[, .I[which.max(somecol)], by=grp].






.SDcols
Specifies the columns of x to be included in the special symbol .SD which stands
for Subset of data.table. May be character column names or numeric positions.
This is useful for speed when applying a function through a subset of (possible
very many) columns; e.g., DT[, lapply(.SD, sum), by="x,y", .SDcols=301:350].
For convenient interactive use, the form startcol:endcol is also allowed (as
in by), e.g., DT[, lapply(.SD, sum), by=x:y, .SDcols=a:f]


---+ An Operation Across Rows
Check if a variable is equal to any of the other variables.
antibiotic_qad_all5[ , rownums := .I]
antibiotic_qad_all5[antibiotic != 'VANCOMYCIN', find_ab2 := antibiotic %in% .SD, .SDcols = paste('lag2_ab', 1:nab, sep = ""), by = rownums]


n = 7
set.seed(3)
dt = data.table(
  x = 1:n,
  var1 = sample(1:8, n, replace = TRUE),
  var2 = sample(1:8, n, replace = TRUE),
  var3 = sample(1:8, n, replace = TRUE),
  var4 = sample(1:8, n, replace = TRUE),
  var5 = sample(1:8, n, replace = TRUE)
  )
lets = letters[1:n]
set.seed(3)
dt = data.table(
  x = lets,
  var1 = sample(lets, n, replace = TRUE),
  var2 = sample(lets, n, replace = TRUE),
  var3 = sample(lets, n, replace = TRUE),
  var4 = sample(lets, n, replace = TRUE),
  var5 = sample(lets, n, replace = TRUE)
  )
dt
dt[4, var3 := NA]


# Works!
xxx = Sys.time()
dt[ , rowSums(x == .SD) > 0, .SDcols = var1:var5]
Sys.time() - xxx
Time difference of 0.05899978 secs


xxx = Sys.time()
dt[ , rowSums(x == .SD, na.rm = TRUE) > 0, .SDcols = var1:var5]
Sys.time() - xxx
Time difference of 0.03999996 secs











dt[ , rowSums(x %in% .SD) > 0, .SDcols = var1:var5]
Error in rowSums(x %in% .SD) : 
  'x' must be an array of at least two dimensions




# Does not work.
dt[ , x %in% .SD, .SDcols = var1:var5]
#[1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE

# Does not work. Gives a matrix.
dt[ , apply(.SD, FUN = function(z)(x == z), MARGIN = 1), .SDcols = var1:var5]
# Does not work. Gives a matrix.
dt[ , apply(.SD, FUN = function(z)(x %in% z), MARGIN = 1), .SDcols = var1:var5]


# Does not work:
dt[ , sum(x == .SD), .SDcols = var1:var5]

# Key to solving this problem elegantly is ###by.###
xxx = Sys.time()
dt[ , any(x == .SD), .SDcols = var1:var5, by = x]
xxx - Sys.time()
Time difference of -0.04399991 secs

   x    V1
1: 1  TRUE
2: 2  TRUE
3: 3 FALSE
4: 4 FALSE
5: 5  TRUE
6: 6 FALSE
7: 7 FALSE

# This is awesome, but it only works if there is a unique key.
xxx = Sys.time()
dt[ , x %in% .SD, .SDcols = var1:var5, by = x]
xxx - Sys.time()
   x    V1
1: 1  TRUE
2: 2  TRUE
3: 3 FALSE
4: 4 FALSE
5: 5  TRUE
6: 6 FALSE
7: 7 FALSE
Time difference of -0.04200006 secs


set.seed(3)
dt = data.table(
  x = c(3, 4, 6, 4, 2, 1, 7),
  var1 = sample(1:8, n, replace = TRUE),
  var2 = sample(1:8, n, replace = TRUE),
  var3 = sample(1:8, n, replace = TRUE),
  var4 = sample(1:8, n, replace = TRUE),
  var5 = sample(1:8, n, replace = TRUE)
  )
dt

xxx = Sys.time()
dt[ , rownums := .I]
dt[ , x %in% .SD, .SDcols = var1:var5, by = rownums]
xxx - Sys.time()
Not sure how to avoid making the rownums variable.
Time difference of -0.09800005 secs

Using rowSums actually works faster.


---+ by =

Investigate what this does. Will DT have the new variable? Will only rows 1:5 have the value populated? I think yes and yes, and the effect of subsetting to the first 5 rows is that only the rows with values of V1 occurring in rows 1:5 will get a value calculated for V4.sum.
DT[1:5,.(V4.Sum = sum(V4)),by=V1] Calculates the sum of V4, for every group in
V1, after subsetting on the first five rows.
##!! But this does not have any assignment, right?

---++ 

questions = c(
    'Code_30_Alert_in_ED'
  , 'Identifies_30_Day_Readmits_in_ED'
  , 'Notifies_PCP'
  , 'Monitors_Teach_Back'
  , 'Schedules_Fu_Appt'
  , "Fu_Calls"
  , 'Ensures_Med_Reconciliation'
  , 'Assesses_Med_Coverage'
  , 'Multi_Disc_Readmission_Team'
  , 'Reviews_Readmit_Cases'
  , 'Uses_RaR_Daily'
  , 'Provides_Interpreters'
  , "One_Person_in_Charge_of_Readmission"
  )

tier1 = c( 
    'Schedules_Fu_Appt'
  , 'Fu_Calls'
  , 'Notifies_PCP'
  , 'Ensures_Med_Reconciliation')
tier2 = c(tier1, 'Identifies_30_Day_Readmits_in_ED', 'Uses_RaR_Daily')
tier3 = c(tier2, 'Multi_Disc_Readmission_Team', 'One_Person_in_Charge_of_Readmission', 'Reviews_Readmit_Cases')


surveyDat5[ , at_least_tier1 := c('No', "Yes")[as.numeric(Schedules_Fu_Appt == "Yes" & Fu_Calls == "Yes" & Notifies_PCP == "Yes" & Ensures_Med_Reconciliation == "Yes") + 1]]
surveyDat5[ , at_least_tier2 := c('No', "Yes")[as.numeric(at_least_tier1 == "Yes" & Identifies_30_Day_Readmits_in_ED == "Yes" & Uses_RaR_Daily == "Yes") + 1]]
surveyDat5[ , at_least_tier3 := c('No', "Yes")[as.numeric(at_least_tier2 == "Yes" & Multi_Disc_Readmission_Team == 'Yes' & One_Person_in_Charge_of_Readmission == "Yes" & Reviews_Readmit_Cases == 'Yes') + 1]]
tier_names = Cs(at_least_tier1, at_least_tier2, at_least_tier3)

#questions %in% names(surveyDat4)
interventions_long = melt(surveyDat5[ , c('facility_cd', questions, tier_names), with = FALSE], 
     id.vars = 'facility_cd',
     measure.vars = c(questions, tier_names),
     variable.name = 'Question',
     value.name = 'value')


interventions_long[ , value2 := as.numeric(value == "Yes")]
interventions_long[!(value %in% c("Yes", "No") | is.na(value)) , value2 := 'Error']
# Calculate the number of interventions each facility is using

##!! Summing one variable with a by variable ***but only for some rows.***
interventions_long[ , number_of_interventions := sum(value2[Question %in% questions]), by = facility_cd]

interventions_long[ , facility_condition_adjusted_rate := surveyDat5$facility_condition_adjusted_rate[match(facility_cd, surveyDat5$facility_cd)]]
interventions_long[ , facility_size := facility_level_rates$N[match(facility_cd, facility_level_rates$facility_cd)]]

# For a specific intervention, compare readmission rates between hospitals that do and don't use them.
compare_rates_by_use = interventions_long[ , 
                                          .(.N, 
                                            avg_facility_condition_adjusted_rate = mean(facility_condition_adjusted_rate, na.rm = TRUE)), by = .(Question, value)]

## Count the number of rows satisfying some condition, using a by variable
dev_dat[ , num_encounters_at_origin := sum(as.numeric(encounter_period == 'Feature calculation')), by = patient_id]



---++ Assigning variables when using by

Be careful when trying to subset in i and assign a new variable, and trying to use by. I need to look at this more carefully.



# Example of assigning variables with by
# This way doesn't use by, and it doesn't propagate the new variable to each by group.
identify_four_qads[qad1Flag == 1 & (willHaveAnAntibioticTomorrow | willHaveAnAntibioticInTwoDays), 
  hasASecondQad := 1]  



# This does use by, and it does propagate to the other variables.
## Key synapse: in the last example, we used subsetting in i, whereas in the next, using by, we used it in the RHS of the variable assignment.
identify_four_qads[ , 

  hasASecondQad := 
    as.numeric(
    any(qad1Flag == 1) 
    & 
    any((willHaveAnAntibioticTomorrow & qad1Flag == 1) | (willHaveAnAntibioticInTwoDays & qad1Flag == 1))), 
    
  by = .(admission_id, bcx_drawn_day)] 

prev_encounters_dat[ , time_since_last_ip := as.integer(min(time_prev_encounter_to_index_encounter[ip_encounters == 1])), by = patient_id]
Was getting an error about lhs not matching type of rhs. Adding as.integer fixed it, but it doesn't make sense to me.

---+ Sorting

Note that setkey still requires and will always sort only in ascending order, and is different from setorder in that it additionally sets the sorted attribute.

 x[order(.)] is now optimised internally to use data.table's fast order by default.


-------------

rbindlist()
antibiotic_qad_all1 = rbindlist(list(antibiotic_qad5, antibiotic_qad_van4), use.names = TRUE)
concatenates

make new dt with specific columns and create some aggregations
mechvent_fnl = unique(mechvent_proc4[ ,
  .(admission_id, patient_id, bcx_drawn_day, mechvent_min_day = min(day), mechvent = max(mechvent)),
  by = .(admission_id, patient_id, bcx_drawn_day)])
  
  
  
mwe


  
  
  
  
  
  
  
  help(':=') '['

  
  
new
2017-12-20
  



## Perform a function on all (or multiple columns)


# MWE
n = 13
xx = data.table(aa = as.character(sample(1:n, n)),
                bb = as.character(sample(1:n, n)),
                cc = as.character(sample(1:n, n)))
# Set all the columns to numeric
xx[ , names(xx) := lapply(.SD, as.numeric)]





## Set all the blank values to 0...
repMiss =  function(x) ifelse(is.na(x), 0, x)

cols = Cs(vasopressor_new, vaso_cons, mechvent, lactate, cr_comm, cr_hosp,
  egfr_comm, egfr_hosp, tbili_comm, tbili_hosp, plt_comm, plt_hosp)
cols44 = paste(colz, '44', sep = "")  
  
  !!!!! Problem is with using a variable name inside of the [] to refer to a column.
ehr_organ_dys12[ , 
  c('tbili_commX', 'tbili_hospX', 'plt_commX') := lapply(.SD, FUN = repMiss), .SDcols = colz, with = FALSE]

ehr_organ_dys12[ , 
  eval(colz) := lapply(.SD, FUN = repMiss), .SDcols = colz]
  
testDat = ehr_organ_dys12[ , 
  lapply(.SD, FUN = repMiss), .SDcols = cols]

ehr_organ_dys12[ , 
  print(cols44) := lapply(.SD, FUN = repMiss), .SDcols = colz]



## DT excluding specified column / Remove one column:  
ehr_organ_dys[ , -'qad_first']




Using variables in j
I think you need two dots if you''re not using :=, and parentheses if you are using :=.
ks_mod_subset = main_w_prev_enc[ , ..ks_mod_vars]
main_w_prev_enc[ , (labs_vitals_vars) := lapply(.SD, as.numeric), .SDcols = labs_vitals_vars]





