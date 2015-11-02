##############################
#Bild G眉tefunktion           #
#Abbildung 4.2 G眉tefunktion  #
##############################
WertMu0<-2.3      #Erwartungswert in der Nullhypothese
WertSigma<-1.672	#Bekannte Standardabweichung
WertN<-36   	#Stichprobengr??e
Alpha1<-0.05	#Signifikanzniveau
U1<-qt(1-Alpha1/2,WertN-1) 	#Quantil
Lambda<- function(mu) sqrt(WertN)*(mu-WertMu0)/WertSigma #Erwartungswert von T

#Definition der Funktion G
Fehler1<- function(mu) 1-pt(U1,WertN-1,Lambda(mu))+pt(-U1,WertN-1,Lambda(mu))

#Zeichnung von G #Achsenbeschriftung mit griechischen Buchstaben, Info ?plotmath !!
curve(Fehler1,1,3.5,ylab=expression(paste("G眉tefunktion:  ",G(mu))) , xlab=expression(mu))

#Wie oben mit einem zweiten Signifikanzniveau

Alpha2<-0.1
U2<-qt(1-Alpha2/2,WertN-1)
Fehler2<- function(mu) 1-( pt(U2,WertN-1,Lambda(mu))-pt(-U2,WertN-1,Lambda(mu)))

curve(Fehler2,1,3.5, add=TRUE, lty=2)	#F?ge Zweite Funktion 

text(1.50,0.6, expression( alpha==0.05))	#Beschriftung obere Funktion
text(2.1,0.4, expression( alpha==0.1))	#Beschriftung untere Funktion
segments(0,0.05,2.3,0.05)			#y=0.95
segments(0,0.10,2.3,0.10,lty=3)		#y=0.9
text(1.25,0.08, expression(paste("G(2,3)=",alpha)))	#Beschriftung der beiden oberen
title("G眉tefunktion")

