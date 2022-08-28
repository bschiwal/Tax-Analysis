### This project serves to provide a basic analysis of the proposed ND tax rate changes 

require(tidyverse)
### Build Rate Table 


##New Rate Schedule###
sglnewmin<-c(0,54725)
sglnewmax<-c(54725,9999999999)
sglnewrate<-c(0,.015)
mfjnewmin<-c(0,95600)
mfjnewmax<-format(c(95600,9999999999), scientific = FALSE)
mfjnewrate<-c(0,.015)

###Current Rate Schedule####
sglcurmin<-c(0,40525,98100,204675,445000)
sglcurmax<-c(40525,98100,204675,445000,9999999999)
sglcurrate<-c(0.011,.0204,0.0227,0.0264,.0290)
mfjcurmin<-c(0,67700,163550,249150,445000)
mfjcurmax<-c(67700,163550,249150,445000,9999999999)
mfjcurrate<-c(0.011,.0204,0.0227,0.0264,.0290)

###Combine into Data Frames###
sglnew<-data_frame(xmin=sglnewmin,xmax=sglnewmax,xrate=sglnewrate)
mfjnew<-data_frame(xmin=mfjnewmin,xmax=mfjnewmax,xrate=mfjnewrate)
sglcur<-data_frame(xmin=sglcurmin,xmax=sglcurmax,xrate=sglcurrate)
mfjcur<-data_frame(xmin=mfjcurmin,xmax=mfjcurmax,xrate=mfjcurrate)
rm(sglnewmax,sglnewmin,sglnewrate,mfjnewmax,mfjnewmin,mfjnewrate,sglcurmax,sglcurmin,sglcurrate,mfjcurmax,mfjcurmin,mfjcurrate)



income<-seq(0,10000000,25000)
#income<-50000

tax<- function(inc, brackets, rates) {
  nbrackets<- length(brackets)
  if(inc<=brackets[1])
    return(0)
  
  i<-2
  cumtax<-0
  while(i <=nbrackets){
    if(inc>brackets[i-1]&& inc <brackets[i])
      return(cumtax + rates[i-1]*(inc-brackets[i-1]))
    else
      cumtax<-cumtax+rates[i-1]*(brackets[i]-brackets[i-1])
    i<- i+1
  }
  
  cumtax +rates[nbrackets]*(inc - brackets[nbrackets])
}


mfjtaxcur<-sapply(income,tax,brackets=mfjcur$xmin, rates=mfjcur$xrate)
sgltaxcur<-sapply(income,tax,brackets=sglcur$xmin, rates=sglcur$xrate)
mfjtaxnew<-sapply(income,tax,brackets=mfjnew$xmin, rates=sglnew$xrate)
sgltaxnew<-sapply(income,tax,brackets=sglnew$xmin, rates=sglnew$xrate)

tottax<-data.frame(income=income,
                   filing_type = "Single",
                   current=sgltaxcur,
                   proposed=sgltaxnew)%>%
  union(data.frame(income=income,
                   filing_type="Married",
                   current=mfjtaxcur,
                   proposed=mfjtaxnew))%>%
  mutate(change = proposed-current,
         current_eff = round(current/income,4) ,
         proposed_eff = round(proposed/income,4),
         change_eff = proposed_eff-current_eff
  )
         


rm(income,mfjtaxcur,mfjtaxnew,sgltaxcur,sgltaxnew,mfjcur,mfjnew,sglcur,sglnew)


ggplot(tottax,aes(x=(income/1000),y=change_eff,color=filing_type))+
  geom_smooth(se=FALSE)+
  scale_y_continuous(labels= scales::percent, name="Change in Effective Rate", limits=c(-.015,-.01))+
  scale_x_continuous(name="Taxable Income (Thousands)", limits= c(0,10000), breaks=seq(0,10000,1000))+
  labs(color="Filing Type",
       subtitle="Under Governor's Proposed Plan",
       title="Change in Effective Rate by Income Level",
       caption = "Made by @BSchiwal")+
  theme(plot.title=element_text(
          hjust=.5,
          family="Calibri", face="bold",
          size=rel(1.2)),
        plot.subtitle = element_text(
          hjust=.5,
          family="Calibri",
          size= rel(.8)
        ))


ggplot(tottax,aes(x=(income),y=change_eff,color=filing_type))+
  geom_smooth(se=FALSE)+
  scale_y_continuous(labels= scales::percent, name="Change in Effective Rate", limits=c(-.015,-.01))+
  scale_x_continuous(labels= scales::comma, name="Taxable Income", limits= c(0,2500000), breaks=seq(0,2500000,250000))+
  labs(color="Filing Type",
       subtitle="Under Governor's Proposed Plan",
       title="Change in Effective Rate by Income Level",
       caption = "Made by @BSchiwal")+
  theme(plot.title=element_text(
    hjust=.5,
    #family="Calibri", 
    face="bold",
    size=rel(1.2)),
    plot.subtitle = element_text(
      hjust=.5,
      #family="Calibri",
      size= rel(.8)
    ))
dev.print(file="ChangeInEffectiveTaxRate.png",device=png, height=600,width=800)
dev.off()


