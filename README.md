# Y8Setting-BirthMonth-Analysis
computeAgeCorrelation.R is a script used to test the hypothesis that there is positive correlation between birth date and Year 8 Maths ability set for pupils at "Z School".

At "Z school", a real school with a pseudonym for data protection, pupils are placed into an ability set when they enter Year 8 (at the age of 12). There are 4 ability sets: 1, 2, 3 and 4, with 1 being the highest (and largest) and 4 being the lowest (and smallest). The school sets pupils by the pace of their learning and the amount of classroom support they typically require. Setting is done in the pupils' interest - that they are challenged at a level that suits them. 

Valid data was used from 2004-05 to present day. There was data available going back to 2000-01 but before 2004-05 a different setting structure was used and it wasn't fair to include it. For the same reason, boys in 2013-14 and 2006-07, and girls in 2004-05 were excluded; a different setting structure was in place due to unusual cohorts. 

From archived setting spreadsheets, each pupil's Name, beginning-of-year-8 maths set and, where available, their UPN - numerical Unique Pupil Index - were loaded into memory, then present and archived records used to try to match them up in order to extract their date of birth, with UPN taking precedence where available and a name-match used otherwise. A total of 1114 pupils were matched in total, giving 1114 pairs of values: the first being the number of days into the academic year that pupil was born, and the second being their Maths set at the beginning of Year 8.

**Hypothesis Test with Spearman's Rank Correlation Coefficient at 5% level of significance**  
H0: rho = 0. There is no correlation between the number of days into the academic year that a pupil at "Z School" is born and their Maths set at the beginning of Year 8.  
H1: rho > 0. There is positive correlation between the number of days into the academic year that a pupil at "Z School" is born and their Maths set at the beginning of Year 8.  

When considering the entire matched cohort of n = 1114 pupils it was found that Spearman's rho = 0.0535, i.e. weak positive correlation.  
Because of the large n and large number of tied ranks, it was not possible to calculate an *exact* p-value to determine if this was significant correlation. Therefore to test for significance, 100000 similarly structured datasets (with the correct totals of pupils in each set) were randomly generated assuming a uniform distribution of birth dates across the year. Just 3.709% of these datasets had rho > 0.0535, from which it is concluded that rho = 0.0535 *is significant* at the 5% level; that is, there *is* evidence that there is positive correlation between the number of days into the academic year that a pupil at "Z School" is born and their Maths set at the beginning of Year 8.  

**Conclusion**  
A pupil with a birthday nearer the beginning of the academic year is more likely to get into a higher Maths set in Year 8 at "Z School".