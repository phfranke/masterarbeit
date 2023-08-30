/**
* Name: kammmolche
* Based on the internal empty template. 
* Author: Philipp Franke
* Tags: Invasive Species, conservation, agend based modelling, triturus carnifex
*/


model kammmolche

global {
	map jahreszeiten <- [nil::"init",1::"Zuwanderung",2::"Fortpflanzung",3::"Abwanderung",4::"Winter"];
	int aktuelleJahreszeit <- 1;
	string display_jahreszeit <- "init";
	rgb jahreszeit_color <- #white;
	// Rasterdatei für die Vernetzung laden
	file tif <- grid_file("./shp/vernetzung_raster.tif","EPSG:2056");
	// shapefiles für die Umgebung laden
	file shp_bb <- file("./shp/bodenbedeckung.shp");
	geometry shape <- envelope(shp_bb);
	// Parameter
	int anz_km_start parameter:"Anzahl Kammmolche zu Beginn: " category: "Rahmenbedingungen" init:10;
	date starting_date parameter:"Start Datum" category: "Rahmenbedingungen" init:date([2000,2,28,0,0,0.0]);
	float step <- 1 #day;
	float ueberlebensrate_eier parameter: "Überlebensrate Eier: " category:"Überlebensrate" init: 0.5;
	float ueberlebensrate_larve parameter: "Überlebenrate Larven: " category:"Überlebensrate" init: 0.4;
	float ueberlebensrate_juvenil parameter: "Überlebensrate Juvenil: " category: "Überlebensrate" init:0.4;
	float ueberlebensrate_adult parameter: "Überlebenrate Adulte, jährlich: " category:"Überlebensrate" init: 0.5;
	float max_larvendichte parameter: "Larvendichte Anz./m2" category:"Überlebensrate" init:1.0;
	float wanderAdult parameter: "Rate der Auswanderer Adulte [%]: " category: "Wanderung" init: 1.2;
	float wanderJuvenil parameter: "Rate der Auswanderer Juvenil [%]: " category: "Wanderung" init: 37.3;
	int wanderDauer parameter: "Dauer der Wandertätigkeit [d]: " category: "Wanderung" init: 30; 
	float winterImWasser parameter: "Anteil der adulten, welche im Wasser überwintern: " category: "Wanderung" init:0.95;
	int zeit_ei parameter: "Entwicklungsdauer Ei (Tage): " category: "Entwicklung" init:15;
	int zeit_larve parameter: "Entwicklungsdauer Larve (Tage): " category: "Entwicklung" init: 90;
	int zeit_juvenil parameter: "Entwicklungsdauer Juvenil (Tage): " category: "Entwicklung" init: 730;
	
	init{
		create bb from:shp_bb with:[
			typ::string(read("ART_TXT")),
			anzahl_km::int(read("anz_km")),
			flaeche::float(read("flaeche_m"))
		];
		create kammmolch number:anz_km_start {
			self.location <- any_location_in(one_of(where (bb,each.anzahl_km!=0)));
			self.female <- flip (0.5);
			self.color <- female ? #springgreen : #seagreen;
			self.bcolor <- #maroon;
			self.display_form <- circle(5);
			self.heimisches_gewaesser <- first(overlapping(collect(bb,each),self.location));
			self.geschlechtsreife <- starting_date - (rnd(500)*3600*24);
			self.letzte_fortpflanzung <- starting_date - (rnd(500)*3600*24);
			self.wandern <- false;
		}
	}
	reflex jahreszeit{
		switch current_date.week_of_year {
			match_between [0,8] {
				aktuelleJahreszeit<- 1;
				jahreszeit_color <- #lightcyan;
			}
			match_between [9,35] {
				aktuelleJahreszeit <- 2;
				jahreszeit_color <- #lightskyblue;
			}
			match_between [36,44] {
				aktuelleJahreszeit <- 3;
				jahreszeit_color <- #mediumslateblue;
			}
			match_between [44,53] {
				aktuelleJahreszeit <- 4;
				jahreszeit_color <- #snow;
			}
		}
		display_jahreszeit <- jahreszeiten[aktuelleJahreszeit];
	}
}

species bb{
	string typ;
	float flaeche;
	rgb color;
	int anzahl_km;
	float larvendichte <- 0.0;
	aspect standard {
		draw shape color: color;
	}
	init{
		switch self.typ{
			match "Gewaesser.stehendes"{color<-#blue;}
			match_one ["Gewaesser.fliessendes",
				"befestigt.Wasserbecken"] {color<-#blue;}
			match_one ["Gebaeude"]{color<-#black;}
			match_one ["humusiert.Acker_Wiese_Weide", 
				"humusiert.uebrige_humusierte",
				"humusiert.Intensivkultur.Reben",
				"humusiert.Intensivkultur.uebrige_Intensivkultur"]
				{color<-rgb(255,255,229);}
			match "humusiert.Gartenanlage" {color<-#darkgrey;}
			match_one ["bestockt.geschlossener_Wald.Wald",
				"bestockt.uebrige_bestockte",
				"bestockt.geschlossener_Wald.Waldstrasse"]
				{color<-rgb(229,245,224);}
			match_one ["befestigt.Strasse_Weg",
				"befestigt.uebrige_befestigte",
				"befestigt.Trottoir",
				"befestigt.Verkehrsinsel",
				"vegetationslos.uebrige_vegetationslose",
				"befestigt.Bahn"]
				{color<-#grey;}
			default {color<-#red;}
		}
	}	
}

//grid vernetzung file:tif neighbors:8{
//	init {
//		int col <- 265-int(grid_value);
//		color <- rgb(col,col,255);
//	}
//}

species nachwuchs {
	// Eier und Larven werden nicht als einzelne Agenten geführt sondern nummerisch in einem von der Muttern abstammenden Nachwuchs-Agenten
	bb heimisches_gewaesser;
	float gewaesser_flaeche;
	date eier_legedatum;
	date larve_schluepfdatum <- date(10000101);
	bool larve_bereitsgestorben <- false;		// sorgt dafür, dass dichteabhängige Sterblichkeit nur einmal in der ganzen Phase angewendet wird.
	bool ei; //true wenn Stadium = Ei, false wenn Stadium = Larve
	int anzahl;
	aspect standard {
		draw square(5) color:#white border:#black;
	}
	reflex entwicklung {
		if current_date > (self.eier_legedatum + zeit_ei*24*3600) and ei {
			self.ei <- false;
			self.anzahl <- round(self.anzahl/2);
			self.larve_schluepfdatum <- current_date;
		}
		else if current_date > (self.larve_schluepfdatum + zeit_larve*24*3600) and !ei {
			int anzlarven <- sum(collect(where(nachwuchs,!each.ei and each.heimisches_gewaesser=self.heimisches_gewaesser),each.anzahl));
			float flaeche <- first(where(bb,each=self.heimisches_gewaesser)).flaeche;
			float larvendichte <- anzlarven / flaeche;
			float ueberlebensrate <- ueberlebensrate_larve/(1+larvendichte);
			write 'Anzlarven vorher ;' + anzlarven;
			ask where(nachwuchs, each.heimisches_gewaesser = self.heimisches_gewaesser and !each.larve_bereitsgestorben){
				self.anzahl <- round(self.anzahl * ueberlebensrate);
				self.larve_bereitsgestorben <- true;
			} 
			write 'Anzlarven nachher ;' + sum(collect(where(nachwuchs,!each.ei and each.heimisches_gewaesser=self.heimisches_gewaesser),each.anzahl));  
			create juvenil number: self.anzahl { //round(self.anzahl * ueberlebensrate) {
				self.location <- myself.location;
				self.female <- flip(0.5);
				self.heimisches_gewaesser <- myself.heimisches_gewaesser;
				self.entwicklungsdatum <- current_date;
			}
			do die;
		}
	}
}

species juvenil {
	bb heimisches_gewaesser;
	bool female;
	date entwicklungsdatum;
	bool wandern <- true;
	
	aspect standard {
		draw square(5) color:#red;
	}
	
	reflex entwicklung {
		if current_date > (entwicklungsdatum + zeit_juvenil*3600*24) {
			if flip (ueberlebensrate_juvenil) {
				create kammmolch number:1 {
					self.heimisches_gewaesser <- myself.heimisches_gewaesser;
					self.location <- myself.location;
					self.female <- myself.female;
					self.geschlechtsreife <- current_date;
					self.letzte_fortpflanzung <- current_date-500*3600*24;
				}
			}
			do die;
		}
		else if flip(wanderJuvenil/100) and self.wandern {
			create migrant number:1 {
				self.juvenil <- true;
				self.entwicklungsdatum <- myself.entwicklungsdatum;
				self.start_wanderung <- current_date;
				self.female <- myself.female;
				self.location <- myself.location;
				self.wandern <- true;
				self.ziel <- 900.0;
			}
			write self.name + ' wandert.';
			do die;
		}
		else {
			self.wandern <- false;
		}
	}
}

species migrant skills: [moving]{
	bool juvenil;
	bool wandern;
	date entwicklungsdatum;
	date start_wanderung;
	bool female;
	float ziel;
	bb heimisches_gewaesser;
	
	aspect standard {
		draw square(6) color: #green;
	}
	
	reflex wandern {
		if self.wandern and (current_date - self.start_wanderung) > (wanderDauer*3600*24) {
			self.heimisches_gewaesser <- closest_to(where(bb,each.typ='Gewaesser.stehendes'),self);
			self.ziel <- towards(self.location,heimisches_gewaesser.location); 
			float links_rechts <- 1.0;
			float kurswechsel <- 0.0;
			self.wandern <- false;
			write self.name + " genug gewandert!";
		}
		else if self.ziel = 900.0 {
			self.ziel <- rnd(359.9);
			write self.name + " zufälliges Ziel erstellen";
		}
		else if overlaps(self.location,self.heimisches_gewaesser) and !self.wandern {
			if self.juvenil {
				create juvenil number:1 {
					self.heimisches_gewaesser <- myself.heimisches_gewaesser;
					self.female <- myself.female;
					self.entwicklungsdatum <- myself.entwicklungsdatum;
					self.location <- myself.location;
				}
				write self.name + " jungmolch wird wieder juvenil";
				do die;
			}
			else {
				create kammmolch number: 1 {
					self.heimisches_gewaesser <- myself.heimisches_gewaesser;
					self.female <- myself.female;
					self.geschlechtsreife <- myself.entwicklungsdatum;
					self.location <- myself.location;
				}
				write self.name + " adulter wanderer wird wieder Kammmmolch";
				do die;
			}
		}
		else if wandern {
			float kurswechsel <- rnd(30.0);
			float links_rechts <- flip(0.5) ? 1.0: -1.0;
			self.ziel <- self.ziel + links_rechts*kurswechsel;
//			write self.name + 'linksrechts: ' + links_rechts + ', kurswechsel: '+ kurswechsel + ', ziel: ' + self.ziel;
		}
		self.ziel <- self.ziel > 360 ? self.ziel-360 : self.ziel;
		self.ziel <- self.ziel < 0 ? self.ziel*-1 : self.ziel;
		float tempo <- 5.0 + rnd(20.0);
		do move speed: tempo #m/#day heading: self.ziel;
	}
}

species kammmolch skills: [moving]{
	date geschlechtsreife;
	date start_wanderung;
	date letzte_fortpflanzung;
	bb heimisches_gewaesser;
	bool wandern;
	bool female;
	rgb color;
	rgb bcolor;
	geometry display_form <- circle(10);
	float ziel <- 999.9;
	float distanz;
	float larvendichte;
	float dichtekoeffizient;
	float v_wert;
	
	aspect standard 
	{
		draw circle(10) color: #transparent border:#purple;
	}
	
	reflex jahreszeit {
		switch aktuelleJahreszeit {
			match 1 {
				// Zuwanderung an Gewässer
//				do ziel_festlegen(1);
//				do wandern;
			}
			match 2 {
				// Fortpflanzung
				if letzte_fortpflanzung = nil or (current_date - self.letzte_fortpflanzung)/3600/24 >=500 {
					do fortpflanzen;
				}
			}
			match 3 {
			//	Wandern oder Überdauern
				if flip(wanderAdult/100) and self.wandern{
					write "adult: " +self+ ", wandert";
					create migrant number:1{
						self.start_wanderung <- current_date;
						self.juvenil <- false;
						self.entwicklungsdatum <- myself.geschlechtsreife;
						self.female <- myself.female;
						self.location <- myself.location;
						self.wandern <- true;
						self.ziel <- 900.0;
					}
				}
				else {
					wandern <- false;
				}
			}
			match 4 {
			//	ueberwintern;
				if current_date.day_of_year = 365 {
					// sterben
					if flip(1-ueberlebensrate_adult){do die;}
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
	
//	action ziel_festlegen (int zweck) {
//		distanz <-5+rnd(30.0);
//		switch zweck {
//			match 1 { // zum Brutgewässer zurückkehren
//				ziel <- towards(self.location,self.heimisches_gewaesser);
//				}
//			match 2 { // neues Brutgewässer suchen
//				self.heimisches_gewaesser <- one_of(where(overlapping(bb,circle(100, self.location)),each.typ="Gewaesser.stehendes"));
//				
//				if self.heimisches_gewaesser = nil {
//					geometry naechstesZiel <- one_of(where(overlapping(vernetzung,circle(100, self.location)),each.grid_value>40));
//					self.wandern <- true;
//					}
//				}					
//				ziel <- towards(self.location,self.heimisches_gewaesser);
//
//			match 4 { // Zufälliges Ziel innerhalb geeigneter Vernetzungsstrutkturen suchen
//				geometry naechstesZiel <- last(sort_by(overlapping(vernetzung,circle(100, self.location)),each.grid_value));
//				ziel <- towards(self.location,naechstesZiel.location);
//			}
//		}
//	}
//	
//	action wandern {
//		//innerhalb geeigneter Strukturen?
//		
//		v_wert <- first_with(vernetzung,intersects(each,self)).grid_value;
//		//v_wert <- first(of_species(agents_overlapping(self.location), vernetzung)).grid_value;
//		
//		if ziel = 999.9 {
//			do ziel_festlegen(4);
//		}	
//		if v_wert < 40 and flip(0.2){
//			do ziel_festlegen(4);
//		}		
//		//if start_wanderung!=nil and (current_date - start_wanderung)/3600/24 > 40 {
//		//	self.wandern <- 2;
//		//	self.ziel <- 999.9;
//		//}
//		do move heading: self.ziel speed: 2 #m/#day;
//	}
	
	action fortpflanzen {
		if overlaps(self.heimisches_gewaesser.shape,self.location){
			do wander bounds: self.heimisches_gewaesser.shape speed: 0.1 #m/#day;
			if count(overlapping(kammmolch,circle(10,self.location)),each.female=false)>=1 and self.female {
				create nachwuchs number:1 {
					self.location <- myself.location;
					self.ei <- true;
					self.anzahl <- 300;
					self.heimisches_gewaesser <- myself.heimisches_gewaesser;
					self.eier_legedatum <- current_date; 					
					}
				self.letzte_fortpflanzung <- current_date;
				//write letzte_fortpflanzung;
			}
			
		}
		else if self.wandern{
//			do ziel_festlegen (2);
			self.wandern <- false;
//			do wandern;
		}
		else {
//			do ziel_festlegen (1);
//			do wandern;
		}
	}
	
	action verstecken {
//		do ziel_festlegen(4);
		do move heading:self.ziel speed: 2 #m/#day;
	}
	
	action ueberwintern {
		do verstecken;
	}
		
	action sterben {
		if flip(1-ueberlebensrate_adult){
			do die;
			//write "sterb!";
		}
	}
}

experiment test {

	output {
		display Karte {
			species bb aspect:standard;
			species kammmolch aspect:standard;
			species nachwuchs aspect:standard;
			species migrant aspect:standard;
//			grid vernetzung transparency:0.5;			
			}
		display Grafiken {
			chart "Zeitliche Entwicklung" type:series size:{0.9,1} position:{0,0} y_range:{0,500} {
				datalist [
					"Eier", 
					"Larven",
					"Juvenile",
					"Adulte",
					"Wanderer"
				] color:[
					#silver,
					#green,
					#red,
					#blue,
					#brown
				] value: [
					sum(collect(where(nachwuchs,each.ei),each.anzahl)),
					sum(collect(where(nachwuchs,!each.ei),each.anzahl)),
					count(juvenil,each.female or !each.female),
					count(kammmolch,each.female or !each.female),
					count(migrant,each.female or !each.female)
				] marker:false;
			}
			chart "Stadien" type:histogram size:{0.1,1} position:{0.9,0} style:stack x_serie_labels:""
				{
				datalist [
					"Larve",
					"Juvenil",
					"Adult"
				]
				color: [
					#green,
					#red,
					#blue
				]
				value:[
					count(nachwuchs,!each.ei),
					count(juvenil,each.female or !each.female),
					count(kammmolch, each.female or !each.female)
				];
			}
		}
		monitor Datum value:current_date color:jahreszeit_color;
		monitor Tag value:current_date.day_of_year color:jahreszeit_color;
		monitor Jahreszeit value:display_jahreszeit color:jahreszeit_color;
	}
}