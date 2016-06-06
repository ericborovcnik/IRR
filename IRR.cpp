///*
// * Berechnet den IRR aus einer Sammlung von Cashflows, wobei der Basis-Invest als erster Wert mit umgekehrten Vorzeichen
// * eingebracht wird.
// * @author		Eric Borovcnik <eric.borovcnik@wb-informatik.ch>
// * @copyright	WB Informatik AG (http://www.wb-informatik.ch)
// * @version		2016-06-05	eb	from scratch
// */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <vector>
#include <limits>

#define LOW_RATE -1											//	Start der unteren Bandbreite bei -100%
#define HIGH_RATE 1											//	Start der oberen Bandbreite bei 100%
#define MAX_ITERATION 1000							//	Maximale Anzahl Berechnungsschritte

double delay = 0.5;											//	Cashflow-Verzinsung ab Jahr 1 - 0:Vorschüssig - 0.5:Halbjährlich nachschüssig - 1:Nachschüssig
double precision = 0.00001;							//	Initiale NPV-Toleranz - 1/10^5 der durchschnittlichen absoluten CF-Werte
																				//	Input 10er			->	Toleranz = 0.00001
																				//	Input 1000er		->	Toleranz = 0.001
																				//	Input 100'000er	->	Toleranz = 1.0

/*
 * Berechnet den DCF, wobei der erste Wert die Basis-Investitions im Jahr 0 darstellt und alle Folge-Cashflows
 * sich um DELAY Jahre verz?gern
 * @param std::vector cf								Cashflows
 * @param double diskontsatz						Diskontsatz
 * @return double												DCF-Wert - Ist dieser Wert nahe 0, dann entspricht der Diskontsatz dem IRR
 */
double calculateDCF(std::vector<double> cf, double diskontsatz) {
	int i;																//	Iterator
	double diskontfaktor;									//	Diskontsatz f?r einzelne Diskontierung
	double dcf=0;													//	Discounted Cashflow - Summe aller NCF
	double ncf=0;													//	Net Cashflow - diskontierter Cashflow
	int size=0;														//	Anzahl Cashflows
	size = cf.size();
	for(i=0; i<size; i++) {
		if(i==0) {
			//	Eintrag auf Index 0 ist die Basisinvestition zu Beginn
			diskontfaktor = 1;
		} else {
			//	Folgeperioden werden um die Periode abgezinst.
			//	Die Periode ist durch i = Anzahl Jahre + DELAY = Versatz um 0|0.5|1 bestimmt

			if(diskontsatz == -1) {
				//	Ausnahmeregelung: Ist der Diskontsatz -100%, dann kann der Diskontfaktor nicht bestimmt werden
				//	Stattdessen wird ein diskontfaktor nahe 0 simuliert
				diskontfaktor = 0.00000001;
			} else {
				diskontfaktor = pow((1+diskontsatz), (i - 1 + delay));
			}
		}
		if(diskontfaktor != diskontfaktor) {
			//	Ung?ltiger Diskontfaktor (weil Zins VIIIEEEL zu hoch oder zu tief)
			//	Zins negativ => R?ckgabe positiver fake-DCF: 999'999'999
			//	Zins positiv => R?ckgabe negativer fake-DCF: -999'999'999
			if(diskontsatz<0) {
				dcf = 999999999;
				break;
			} else {
				dcf = -999999999;
				break;
			}
		}
		ncf = cf[i] / diskontfaktor;
		dcf += ncf;
		//printf("calcNPV I:%i CF:%.0f Zins:%.4f fact:%.8f NCF:%.0f DCF:%.0f\n", i, cf[i], diskontsatz, diskontfaktor, ncf, dcf);
	}
	return dcf;
}

/*
 * Ermittelt den internen Zinssatz, der den DCF-Wert auf 0 +/- precision dr?ckt
 * @param std::vector cf								Cashflows
 * @return double												IRR - 0, wenn IRR nicht bestimmt werden kann
 */
double calculateIRR(std::vector<double> cf) {
	int i=0;															//	Maximale Iterationsschritte
	double npv = 0;												//	Net Present Value auf Basis guessRate
	double guessRate = 0;									//	Zu pr?fender Zins
	double lowGuessRate = LOW_RATE;				//	Untere Zinsbarriere
	double highGuessRate = HIGH_RATE;			//	Obere Zinsbarriere
	bool switchCalculation = false; 			//	true, wenn mit fallenden Zinsen der NPV f?lt

	//	Ob mit steigenden oder fallenden Zinsen der NPV steigt oder f?lt, h?gt extrem
	//	vom Zeitpunkt der Investitionen und der R?ckfl?sse ab (Bspw. -10 1 2 3 vs. 1 2 3 -10)
	//	Es wird inital ausgetestet, indem die untere und obere Bandbreite eingesetzt wird.
	//	Ist der NPV(low) > NPV(high), dann steigt der NPV mit fallenden Zinsen (switch = false)
	//	Ist der NPV(low) < NPV(high), dann f?lt der NPV mit fallenden Zinsen (switch = true)
	if(calculateDCF(cf,lowGuessRate) < calculateDCF(cf,highGuessRate)) {
		switchCalculation = true;
	}

	for(i=0; i<MAX_ITERATION; i++) {
		//	Der zu pr?fende Zinssatz wird in der Mitte zwischen unterer und oberer Bandbreite vermutet
		guessRate = (lowGuessRate + highGuessRate) / 2;
		npv = calculateDCF(cf, guessRate);
		//printf("%i: %.4f .. %.4f  IRR:%.12f   =>   NPV:%.2f\n",i,lowGuessRate,highGuessRate,guessRate, npv);

		//	Ist der vorliegende NPV innerhalb der 0-Toleranz, dann ist der IRR gefunden
		if(fabs(npv) < precision) 			break;

		if((npv > 0 && !switchCalculation) || (npv < 0 && switchCalculation)) {
			//	Der NPV ist positiv => der Zins ist zu tief und muss angehoben werden.
			//	Das geschieht, indem die untere Bandbreite nach oben geschoben wird.
			//	Stossen wir mit der unteren Grenze an die obere, dann wird die obere Grenze nach oben geschoben
			lowGuessRate = guessRate;
			if(lowGuessRate == highGuessRate) 						highGuessRate *=2;
		} else {
			//	Der NPV ist negativ => der Zins ist zu hoch und muss gesenkt werden.
			//	Das geschieht, indem die obere Bandbreite nach unten geschoben wird.
			//	Stossen wir mit der oberen Grenze an die untere, dann wir die untere  Grenze nach unten geschoben
			//	(Wobei der Verlust sich in IRR ausgedr?ckt ohnehin nur an die Schwelle von -100% ann?ert)
			highGuessRate = guessRate;
			if(lowGuessRate == highGuessRate)							lowGuessRate *=2;
		}
	}
	return guessRate;
}

/**
 * Zeigt die Info-Meldung an
 */
void showInfo() {
	printf("IRR - berechnet den internen Zinsfus einer Kapitalanlage (Internal Rate of Return)\n");
	printf("Syntax:\n");
	printf("IRR -?                             Diese Hilfeseite\n\n");
	printf("IRR CF0 CF1 CF2 ... CFn            Berechnet den IRR zu den Kapitalströmen in den Jahren 0 1 ... n\n");
	printf("Beachten Sie, dass Cash-in- und -outflows stattfinden müssen damit eine IRR-Berechnung möglich ist.\n\n");
	printf("IRR -d DELAY CF0 CF1 CF2 ... CFn   Berechnet den IRR zu den Kapitalströmen in den Jahren 0 1 ... n\n");
	printf("DELAY beschreibt dabei den Diskontierungsversatz für die Jahre 1 bis n.\n");
	printf("DELAY muss einem der folgenden Werte entsrpechen:\n");
	printf(" - 0   : Vorschüssiger Kapitalstrom. Die Umsätze werden zu Beginn der Periode realisiert.\n");
	printf(" - 0.5 : Halbjährlich nachschüssig. Die Umsätze kommen laufend und werden Mitte Jahr diskontiert (Standard)\n");
	printf(" - 1   : Nachschüssig. Die Umsätze werden erst per Ende Jahr realisiert.\n\n");
	printf("Kann der IRR nicht ermittelt werden, dann wird 0.000 ausgewiesen.\n\n");
}

/**
 * Hauptprogramm - berechnet den IRR
 * Syntax:	IRR -? | [-d[0|0.5|1]] CF0 CF1 ... CFn
 */
int main(int argc, char *argv[]) {

	int i=0;															//	Iterator
	int j=1;															//	Hilfs-Iterator
	bool hasPositives = false;						//	Marke ist true, wenn positive Werte vorliegen
	bool hasNegatives = false;						//	Marke ist true, wenn negative Werte vorliegen
	double value=0;												//	Einzelner Cashflow
	std::vector<double> cf;								//	Cashflow-Array in den Jahren 0..(n-1)
	double irr=0;													//	Interner Ertragszinssatz - 0.00 wenn keine Vorzeichenwechsel in den CF vorliegen
	double avg=0;													//	Summe rsp. Durchschnitt der absoluten CF-Gr?ssen

	/*
	 * Prüfe, ob Parameter -? gesetzt ist
	 */
	if(argc >1) {
		if(strcmp(argv[1], "-?") == 0) {
			showInfo();
			return  EXIT_SUCCESS;
		}
		if(strstr(argv[1], "-d") != NULL) {
			if(strcmp(argv[1], "-d0") == 0)			delay = 0;
			if(strcmp(argv[1], "-d1") == 0)			delay = 1;
			j = 2;
		}
	}


	/*
	 * Extrahiere die Parameter in Cashflow-Array
	 */
	for(i=j; i<argc; i++) {
		value = atof(argv[i]);
		if(value<0)			hasNegatives = true;
		if(value>0)			hasPositives = true;
		avg += abs(value);
		cf.push_back(value);
	}
	if(cf.size() > 0) {
		avg = avg / cf.size();
		precision = avg / pow(10,5);
	}

	/*
	 * Pseudo-Loop: Ermittle IRR ?ber Exceptions oder Berechnung
	 */
	do {
		if(!hasPositives)			break;				//	IRR=0, wenn keine positiven Werte vorliegen
		if(!hasNegatives)			break;				//	IRR=0, wenn keine negativen Werte vorliegen
		irr = calculateIRR(cf);
	} while(false);

	/*
	 * Ausgabe IRR und Exit
	 */
	printf("%.4f\n", irr);
	return EXIT_SUCCESS;
}
