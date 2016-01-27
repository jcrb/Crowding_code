#Codebook
###A generic method for evaluating crowding in an emergency department

When a patient arrives in the defined study period she is included in the study until departure or the end of the study period. The following variables must be obtained for each patient arriving in the study period.

####Variable name; Variable description; Values; Value labels


- dte; the date that the observation were made; 0-9; yyyy-mm-dd

- tm.intrvl; the time interval that the observation were made; 0-9; hh:mm - hh:mm

- visit.id; string of ordinal data containing unique id of each patient visit - the id follows the patient from arrival to departure; 0-9; integers

- record.id; string of ordinal data containing unique id of each observation - the id shifts for each observation thus the same patient may get several different record.ids; 0-9; integers

- age; numerical string holding information of the age in years since birth of the patient; 0-9; integers
 
- sex; dicotomous character string holding information of the sex of the patient; "male"/"female"; "male"/"female"

- arrival.dte; the date of the first registration of the patient (ie. earliest observation for the particular visit.id); 0-9; yyyy-mm-dd

- arrival.tm; the time interval of the first registration of the patient (ie. earliest observation for the particular visit.id); 0-9; hh:mm - hh:mm

- depart.dte; the date of the last registration of the patient (ie. last observation for the particular visit.id); 0-9; yyyy-mm-dd

- depart.tm; the time interval of the last registration of the patient (ie. last observation for the particular visit.id); 0-9; hh:mm - hh:mm

- first.location; character string containing department code of first department registered for that specific visit; A-Z 0-9 including all special characters; label depends on coding system

- last.location; character string containing department code of last department registered for that specific visit; A-Z 0-9 including all special characters; label depends on coding system

- triage.score; numerical string containing the triage score given at arrival to the ED with 1 being most acute and 5 being least acute; 1-5; five-point ordinal scale

- diagnosis; character string containing the final diagnosis code assigned to the specific patient (i.e. one code per visit.id); A-Z 0-9 including all special characters; depends on diagnosis system
