# Filterführung Check
table(as_factor(dat$F23), as_factor(dat$F24_1), useNA="ifany")
table(as_factor(dat$F23), as_factor(dat$F24_2), useNA="ifany")
table(as_factor(dat$F23), as_factor(dat$F24_3), useNA="ifany")
table(as_factor(dat$F23), as_factor(dat$F24_99), useNA="ifany")
table(as_factor(dat$F23), as_factor(dat$F25), useNA="ifany")