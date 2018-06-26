
install.packages("mrgsolve")
library(mrgsolve)
library(dplyr)
library(ggplot2)

# one-compartment with first order absorption and first order elimination
code <- '
$PARAM  @annotated
CL   : 0.0375 : L/hr
VC   : 10.0   : L
KA   : 0.0145 : 1/hr
ALAG : 0.705  : hr


$SET delta=0.1,end=-1

$CMT DEPOT CENT


$ODE
dxdt_DEPOT = -KA*DEPOT;
dxdt_CENT = KA*DEPOT - CL*(CENT/(VC));

$TABLE double CP = CENT/(VC);

$CAPTURE CP
'

mod <- mread("efpeg0", tempdir(),code)

mod

# For dose titration 2-2-4-4-6-6-6-6-6-6-6-6

data <- expand.ev(ID=1,amt=c(2,4,6), ii=7*24, addl=1) %>% 
  as_tibble() %>% 
  mutate(time = case_when(
    amt == 4 ~ 168*2,
    amt == 6 ~ 168*4,
    TRUE     ~ time
  ))
data$ID <-1
data$addl[data$amt==6] <- 8
out <- mod %>% data_set(data) %>% carry.out(dose) %>% Req(CP) %>% mrgsim(end=168*12)

(P0 <- out %>% as.data.frame %>% 
  ggplot(aes(x=time/24/7)) + 
  geom_line(aes(y=CP), col="firebrick",lwd=1) )

# For dose scheme of 6-6-6-6-6-6-6-6-6-6-6-6

data0 <- expand.ev(amt=c(6), ii=7*24, addl=11) %>% mutate(dose=amt)
data0

out1 <- mod %>% data_set(data0) %>% carry.out(dose) %>% 
  Req(CP) %>% mrgsim(end=168*12) %>% as.data.frame

(P1 <- P0 + geom_line(data=out1,aes(x=time/24/7, y=CP), col="darkgreen",lwd=1) +
    xlab("Time (weeks)") +
    ggtitle("Linear elimination") +
    scale_x_continuous(breaks = c(0:13)) +
    geom_vline(xintercept=7,linetype=2) +
    geom_vline(xintercept=8,linetype=2) )


# one-compartment with first order absorption and MM elimination
code <- '
$PARAM  @annotated
VMAX : 0.099  : mg/hr
KM   : 1.730  : mg/L (1730 ng/mL)
VC   : 12.7   : L
KA   : 0.0145 : 1/hr
ALAG : 0.705  : hr


$SET delta=0.1,end=-1

$CMT DEPOT CENT


$ODE
dxdt_DEPOT = -KA*DEPOT;
dxdt_CENT = KA*DEPOT - VMAX*(CENT/(VC))/(KM+(CENT/(VC)));

$TABLE double CP = CENT/(VC);

$CAPTURE CP
'

mod <- mread("efpeg1", tempdir(),code)
out <- mod %>% data_set(data) %>% carry.out(dose) %>% Req(CP) %>% mrgsim(end=168*12)
out1 <- mod %>% data_set(data0) %>% carry.out(dose) %>% 
  Req(CP) %>% mrgsim(end=168*12) %>% as.data.frame
(P0 <- out %>% as.data.frame %>% 
    ggplot(aes(x=time/24/7)) + 
    geom_line(aes(y=CP), col="firebrick",lwd=1) )

(P2 <- P0 + geom_line(data=out1,aes(x=time/24/7, y=CP), col="darkgreen",lwd=1) +
    xlab("Time (weeks)") +
    ggtitle("MM elimination") +
    scale_x_continuous(breaks = c(0:13)) +
    geom_vline(xintercept=7,linetype=2) +
    geom_vline(xintercept=8,linetype=2) )

gridExtra::grid.arrange(P1,P2,nrow=1)
