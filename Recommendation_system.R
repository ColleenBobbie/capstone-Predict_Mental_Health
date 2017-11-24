# MENTAL CRISES

#This is for a random forest with Depth 3. While not having a great accuracy, it could provide some insight into 
#how patients are behaving, and be a relatively simple tool for doctors when assessing patients' mental health.

#note: this tree has a 81% accuracy, so keep that in mind!
#If this were a funded project, would actually get a programmer to sit down and write out a tree that's not limited
#to a depth of 3.

#---
#LOAD FUNCTION
#---

health_funt=function(concentrate, coverage, checkup, Dentist, arthritis, blind, diabetes){
  if (concentrate ==1){
    if(coverage<2){
      return("Your patient may be at risk of developing a mental health crises.")
    }
    if (coverage >=2){
      if(arthritis < 2)
      {return("Your patient may be at risk of developing a mental health crises.")
      }
    }
  }
  if (concentrate >=2){
    if (dentist<1){
      return("Your patient is likely not at risk of developing a mental health crises.")
    }
    if(dentist>=1){
      if(diabetes == 2){
        return("Your patient may be at risk of developing a mental health crises.")
      }
      if(diabetes==1){
        return("Your patient is likely not at risk of developing a mental health crises.")
      }
    }
  }
}

#---
#FILL IN PATIENT's INFORMATION:
  #run each line one at a time as it will prompt you to ask questions to your patients
#---
library(utils)
concentrate=as.numeric(menu(c("yes", "no"), title="Do you have difficulty concentrating or remembering?"))
coverage=as.numeric(menu(c("yes", "no"), title="Hvae you been without health care coverage in the past 12 months?"))
blind = as.numeric(menu(c("yes", "no"), title= "Are you blind or do you have serious difficulty seeing, even when wearing glasses?"))
arthritis = as.numeric(menu(c("yes", "no"), title="(Ever told) you have some form of arthritis, rheumatoid arthritis, gout, lupus, or fibromyalgia?"))
Dentist=as.numeric(menu(c("within last year", "1-2 years", "2-5 years", "> 5 years"), title="When did you last visit a dentist or a dental clinic?"))
checkup=as.numeric(menu(c("within last year", "1-2 years", "2-5 years", "> 5 years"), title="When was the last time you had a routine checkup?"))
diabetes=as.numeric(menu(c("yes", "no"), title="Have you ever been told you have diabetes?"))


#--
#RUN LIKELIHOOD SCENARIO
#--
health_funt(concentrate, coverage, Dentist, checkup, diabetes, arthritis, blind)
                 