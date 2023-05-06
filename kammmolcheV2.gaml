/**
* Name: kammmolche
* Based on the internal empty template. 
* Author: Philipp Franke
* Tags: Invasive Species, conservation, agend based modelling, triturus carnifex
*/


model kammmolche

global {
	// shapefiles für die Umgebung laden
	file shp_bb <- file("./shp/bodenbedeckung.shp");
	geometry shape <- envelope(shp_bb);
	// Parameter
	int anz_km_start parameter:"Anzahl Kammmolche zu Beginn: " category: "Rahmenbedingungen" init:10;
	date starting_date parameter:"Start Datum" category: "Rahmenbedingungen" init:date([2000,1,1,0,0,0.0]);
	float step <- 1 #day;
	float ueberlebensrate_eier parameter: "Überlebensrate Eier: " category:"Biologie" init: 0.5;
	float ueberlebensrate_larve parameter: "Überlebenrate Larven: " category:"Biologie" init: 1.0;
	float ueberlebensrate_juvenil parameter: "Überlebensrate Juvenil: " category: "Biologie" init:1.0;
	float ueberlebensrate_adult parameter: "Überlebenrate Adulte, jährlich: " category:"Biologie" init: 0.5;
	float larvendichte parameter: "Larvendichte Anz./m2" category:"Biologie" init:1.0;
	int zeit_ei parameter: "Entwicklungsdauer Ei (Tage): " category: "Biologie" init:15;
	int zeit_larve parameter: "Entwicklungsdauer Larve (Tage): " category: "Biologie" init: 90;
	int zeit_juvenil parameter: "Entwicklungsdauer Juvenil (Tage): " category: "Biologie" init: 730;
	init{
		create bb from:shp_bb with:[
			typ::read("ART_TXT"),
			anzahl_km::int(read("anz_km")),
			flaeche::float(read("flaeche_m"))
		];
		create adult number:anz_km_start {
			location <- any_location_in(one_of(where (bb,each.anzahl_km!=0)));
			female <- flip (0.5);
			heimisches_gewaesser <- first(overlapping(collect(bb,each),self.location));
			geschlechtsreife <- current_date;
			letzte_fortpflanzung <- current_date;
		}
	}
}

species bb{
	string typ;
	float flaeche;
	rgb color;
	int anzahl_km;
	bool vernetzung;
	bool versteck;
	bool barriere;
	bool brut;
	aspect gis {
		draw shape color: color;
	}
	init{
		switch self.typ{
			match "Gewaesser.stehendes"{color<-#blue;brut<-true;}
			match_one ["Gewaesser.fliessendes",
				"befestigt.Wasserbecken"] {color<-#blue;vernetzung<-true;}
			match_one ["Gebaeude"]{color<-#black;barriere<-true;}
			match_one ["humusiert.Acker_Wiese_Weide", 
				"humusiert.uebrige_humusierte",
				"humusiert.Intensivkultur.Reben",
				"humusiert.Intensivkultur.uebrige_Intensivkultur"]
				{color<-rgb(255,255,229);vernetzung<-true;}
			match "humusiert.Gartenanlage" {color<-#darkgrey;vernetzung<-true;}
			match_one ["bestockt.geschlossener_Wald.Wald",
				"bestockt.uebrige_bestockte",
				"bestockt.geschlossener_Wald.Waldstrasse"]
				{color<-rgb(229,245,224);vernetzung<-true;}
			match_one ["befestigt.Strasse_Weg",
				"befestigt.uebrige_befestigte",
				"befestigt.Trottoir",
				"befestigt.Verkehrsinsel",
				"vegetationslos.uebrige_vegetationslose",
				"befestigt.Bahn"]
				{color<-#grey;vernetzung<-true;}
			default {color<-#red;}
		}
	}
}

species eier {
 	date legedatum;
 	bb heimisches_gewaesser;
 	bool female;
 	rgb color;

 	reflex entwicklung {
 		if (current_date - legedatum)/3600/24 >= zeit_ei {
 			if flip (ueberlebensrate_eier) {
	 			write "eier entwicklung";
				create larve number:1 {
	 				self.color <- myself.color;
	 				self.female <- myself.female;
	 				self.location <- myself.location;
	 				self.schluepfdatum <- current_date;
	 				self.heimisches_gewaesser <- myself.heimisches_gewaesser;
	 				self.flaeche <- myself.heimisches_gewaesser.flaeche;
	 			}
 			}
 			do die;
 		}
 	}
 	aspect gis {
 		draw circle(2) color: #white border:color;
 		}
 }
	
species larve {
	date schluepfdatum;
	bb heimisches_gewaesser;
 	bool female;
 	rgb color;
	float flaeche;
	float dichte;
	float dichtekoeffizient;
	reflex regulierung {
		//Dichte abhängigkeit pro Gewässer
		dichte <- count(larve,each.heimisches_gewaesser=self.heimisches_gewaesser)/flaeche;
		if (dichte>=(larvendichte/2)) {dichtekoeffizient <- (2*dichte/larvendichte)-1;}
		if flip (dichtekoeffizient) {do die;}// linearer zusammenhang, Sterbewahrscheinlichkeit nimmt mit 0.5*max larvendichte linear zu bis max larvendichte
	}
	reflex entwicklung {
		if (current_date - schluepfdatum)/3600/24 >= zeit_larve {
			if flip (ueberlebensrate_larve) { //keine zusätzliche Sterbewahrscheinlichkeit
				create juvenil number:1 {
					self.color <- myself.color;
					self.female <- myself.female;
					self.location <- myself.location;
					self.entwicklungsdatum <- current_date;
					self.heimisches_gewaesser <- myself.heimisches_gewaesser;
				}
				do die;
			}
		}
	}
	aspect gis {
 		draw square(3) color: color border:#white;
 	}
}

species juvenil {
	date entwicklungsdatum;
	bb heimisches_gewaesser;
	bool female;
	rgb color;
	reflex entwicklung {
		if (current_date - entwicklungsdatum)/3600/24 >= zeit_juvenil {
			if flip (ueberlebensrate_juvenil){
				create adult number:1 {
					self.color <- myself.color;
					self.female <- myself.female;
					self.heimisches_gewaesser <- myself.heimisches_gewaesser;
					self.letzte_fortpflanzung <- current_date - 2 #year;
					self.location <- myself.location;
				}
			}
			do die;
		}
	}
	aspect gis {
 		draw square(3) color: color border:#black;
 	}
}

species migrant skills: [moving]{
	juvenil stadium_juv;
	adult stadium_adult;
	point ziel;
	float distanz;
	aspect gis{
		draw cross (3) color:#transparent border:#blue;
	}
	
	action ziel_festlegen {
	// ziel <- punkt in weiter ferne (5km?), zufällige himmelsrichtung?	
	}
	
	action wandern {
		 //innerhalb geeigneter Strukturen?
		do ziel_festlegen;
		do goto target:ziel speed: self.distanz #m/#day;
	}
}

species adult skills: [moving]{
	date geschlechtsreife;
	bb heimisches_gewaesser;
	bool female;
	rgb color;
	date letzte_fortpflanzung;
	bool pause <- false;
	point ziel;
	float distanz;
	
	init {
		color <- female ? #red : #brown;
	}
		
	aspect gis{
		draw circle (3) color:#transparent border:color;
	}
	
	reflex decide {
		switch current_date.week_of_year {
			match_between [0,8] {
				do ueberwintern;
			}
			match_between [9,35] {
				if (current_date - self.letzte_fortpflanzung)/3600/24 >=500 {
					do fortpflanzen;
				}
				else {do verstecken;}
			}
			match_between [36,44] {
				do verstecken;
			}
			match_between [45,51] {
				do ueberwintern;
			}
			match 52 {
				do ueberwintern;
				if current_date.day_of_week= 1 {
					do sterben;
				}
			}
		}
		
	}
	
	// Jede Phase (Forpflanzen, Überwintern, Wandern, Verstecken setzt eine Wanderbewegung voraus.
	// "ziel_festlegen" berechnet anhand des Parameters "zweck" eine Zielkoordinate
	// 	zweck: 	Bedeutung
	// 	1: 		Fortpflanzen
	// 	2: 		überwintern
	// 	3: 		Wandern
	// 	4: 		Verstecken  
	
	action ziel_festlegen (int zweck) {
		distanz <-5+rnd(30.0);
		switch zweck {
			match 1 { // Fortpflanzen, aufsuchen des gespeicherten Brutgewässers
				ziel <- centroid(heimisches_gewaesser);
				//write "fortpfl. ziel: " + ziel;
			}
			match 2 { // überwintern, an Ort verharren
				ziel<-self.location;
			}
			match 3 { // Wandern, zielstrebig in in eine Richtung wandern
				ziel <- any_location_in(circle(20,self.location)) with_precision 1;
				//write "wandern, ziel:" + ziel;
			}
			match 4 { // Verstecken (gleichbedeutend mit lokal herumstreifen)
				ziel <- any_location_in(circle(20,self.location)) with_precision 1;
				//write "verstecken, ziel: " + ziel;
			}
		}
	}
	
	action wandern {
		do ziel_festlegen(3);
		do goto target:ziel speed: self.distanz #m/#day;
	}
	
	action fortpflanzen {
		if overlaps(self.heimisches_gewaesser.shape,self.location){
			do wander bounds: self.heimisches_gewaesser.shape speed: 0.1 #m/#day;
			if count(overlapping(adult,circle(10,self.location)),each.female=false)>=1 and self.female {
				create eier number:300 {
					self.location <- myself.location;
					self.heimisches_gewaesser <- myself.heimisches_gewaesser;
					self.legedatum <- current_date;
 					self.female <- flip (0.5);
 					self.color <- female ? #red : #brown;
 					write "Nachwuchs";
					}
				self.letzte_fortpflanzung <- current_date;
			}
			
		}
		else {
			do ziel_festlegen (1);
			do move heading: towards(self.location,self.ziel) speed:self.distanz #m/#day;
		}
	}
	
	action verstecken {
		do ziel_festlegen(4);
		do move heading:towards(self.location, ziel) speed: 2 #m/#day;
	}
	
	action ueberwintern {
		if flip(0.3){pause <- true;}
		if pause {
			//write "wart ... wart...";
			}
		else {do verstecken;}
	}
		
	action sterben {
		if flip(1-ueberlebensrate_adult){
			do die;
			write "sterb!";
		}
	}
}

experiment test {
	output {
		display Karte {
			species bb aspect:gis;
			species adult aspect:gis;
			//species eier aspect:gis;
			//species larve aspect:gis;
			species juvenil aspect:gis;
			
			}
		display Grafiken {
			chart "Stadien" type:series size:{0.8,1} position:{0,0} y_range:{0,500} {
				datalist [
					//"Eier", 
					"Larven",
					//"Juvenile",
					"Adulte"
				] color:[
					//#silver,
					#green,
					//#red,
					#blue
				] value: [
					//count(eier,each.color!=nil),
					count(larve,each.color!=nil),
					//count(juvenil,each.color!=nil),
					count(adult,each.color!=nil)
				] marker:false;
			}
			chart "Anteil Weibchen" type:histogram size:{0.2,1} position:{0.8,0} style:bar y_range:{0,100} {
				data "Prozent" value:count(adult,each.female=true)*100/(count(adult,each.female!=nil)+0.0001);
			}
		}
		monitor Datum value:current_date;
		monitor KW value:current_date.week_of_year;
		monitor Tag value:current_date.day_of_year;
		monitor Larvendichte value:first(collect(larve,each.dichte));
	}
}