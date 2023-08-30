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
	date starting_date parameter:"Start Datum" category: "Rahmenbedingungen" init:date([2000,1,1,0,0,0.0]);
	float step <- 1 #day;
	float ueberlebensrate_eier parameter: "Überlebensrate Eier: " category:"Überlebensrate" init: 0.5;
	float ueberlebensrate_larve parameter: "Überlebenrate Larven: " category:"Überlebensrate" init: 0.4;
	float ueberlebensrate_juvenil parameter: "Überlebensrate Juvenil: " category: "Überlebensrate" init:0.4;
	float ueberlebensrate_adult parameter: "Überlebenrate Adulte, jährlich: " category:"Überlebensrate" init: 0.5;
	float max_larvendichte parameter: "Larvendichte Anz./m2" category:"Überlebensrate" init:1.0;
	float wanderAdult parameter: "Rate der Auswanderer Adulte [%]: " category: "Wanderung" init: 1.2;
	float wanderJuvenil parameter: "Rate der Auswanderer Juvenil [%]: " category: "Wanderung" init: 37.3;
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
			self.stadium <- "adult";
			self.female <- flip (0.5);
			self.color <- female ? #springgreen : #seagreen;
			self.bcolor <- #maroon;
			self.display_form <- circle(5);
			self.heimisches_gewaesser <- first(overlapping(collect(bb,each),self.location));
			self.gewaesser_flaeche <- heimisches_gewaesser.flaeche;
			self.geschlechtsreife <- starting_date - (rnd(500)*3600*24);
			self.letzte_fortpflanzung <- starting_date - (rnd(500)*3600*24);
			self.wandern <- 0;
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
	aspect gis {
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
	reflex larvendichte {
		if self.typ="Gewaesser.stehendes" {
			self.larvendichte <- count(inside(kammmolch,self),each.stadium="larve")/self.flaeche;
		}
	}
}

grid vernetzung file:tif neighbors:8{
	init {
		int col <- 265-int(grid_value);
		color <- rgb(col,col,255);
	}
}

species kammmolch skills: [moving]{
	date eier_legedatum;
	date larve_schluepfdatum;
	date juvenil_entwicklungsdatum;
	date geschlechtsreife;
	date start_wanderung;
	date letzte_fortpflanzung;
	string stadium; // "ei", "larve", "juvenil", "adult"
	bb heimisches_gewaesser;
	float gewaesser_flaeche;
	int wandern; //0: nicht gesetzt, 1:wandern, 2: nicht wandern
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
		draw geometry(display_form,self.location) color:color border:bcolor;
	}
	
	reflex stadiumfilter {
		switch stadium {
			match "ei" {
				// schlüpfen -> Stadium "Ei" wird zu "Larve"
				if (current_date - eier_legedatum)/3600/24 >= zeit_ei {
 					if flip (ueberlebensrate_eier) {
						self.stadium <- "larve";
						self.larve_schluepfdatum <- current_date;
					}
					else {
						do die;
					}
	 			}
	 		}
			match "larve" {
				// Dichteabhängige Sterberate wird in Species "bb" gerechnet.
				self.dichtekoeffizient <- (first_with(bb,intersects(each,self.location)).larvendichte/max_larvendichte^2)/zeit_larve;
				if flip (self.dichtekoeffizient) {
					do die;
				}
				// Entwicklung von Stadium "Larve" zu "Juvenil" 
				if (current_date - larve_schluepfdatum)/3600/24 >= zeit_larve and self.stadium="larve" {
					if flip (ueberlebensrate_larve) {
						self.stadium <- "juvenil";
						self.juvenil_entwicklungsdatum <- current_date;
						self.color <- female ? #sandybrown : #sienna;
						self.bcolor <- #black;
						self.display_form <- square(5);
						}
					else {
						do die;
					}
				}
			}	
			match "juvenil" {
				switch wandern {
					match 0 {
						if flip(wanderJuvenil/100) {
							wandern <- 1;
							self.start_wanderung <- current_date;
							write "Kammmolch, juvenil " + self + " wandert bald!";
						}
						else {
							wandern <- 2;
						}
					}
					match 1 {
						if aktuelleJahreszeit between (1,4) {
							do wandern;
						}
						else if aktuelleJahreszeit = 1{
							do ziel_festlegen(2);
							do wandern;
						}
					}
					match 2 {
						
					}
				}
				
				// Entwicklung von Stadium "juvenil" zu "Adult"
				if (current_date - juvenil_entwicklungsdatum)/3600/24 >= zeit_juvenil and self.stadium="juvenil" {
					if flip (ueberlebensrate_juvenil){
						self.stadium<-"adult";
						self.geschlechtsreife <- current_date;
						self.wandern <- 0;
						self.display_form <- circle(5);
					}
					else {
						do die;
					}
				}
			}
			
			match "adult" {
				switch aktuelleJahreszeit {
					match 1 {
						// Zuwanderung an Gewässer
						do ziel_festlegen(1);
						do wandern;
					}
					match 2 {
						// Fortpflanzung
						if letzte_fortpflanzung = nil or (current_date - self.letzte_fortpflanzung)/3600/24 >=500 {
							do fortpflanzen;
						}
					}
					match 3 {
					//	Wandern oder Überdauern
						switch wandern {
							match 0 {
								if flip(wanderAdult/100) {
									wandern <- 1;
									self.start_wanderung <- current_date;
									write "adult: " +self+ ", wandert";
								}
								else {
									wandern <- 2;
								}
							}
							match 1 {
								do wandern;
							}
							match 2 {
								
							}
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
			match 1 { // zum Brutgewässer zurückkehren
				ziel <- towards(self.location,self.heimisches_gewaesser);
				}
			match 2 { // neues Brutgewässer suchen
				self.heimisches_gewaesser <- one_of(where(overlapping(bb,circle(100, self.location)),each.typ="Gewaesser.stehendes"));
				
				if self.heimisches_gewaesser = nil {
					geometry naechstesZiel <- one_of(where(overlapping(vernetzung,circle(100, self.location)),each.grid_value>40));
					self.wandern <- 1;
					}
				}					
				ziel <- towards(self.location,self.heimisches_gewaesser);

			match 4 { // Zufälliges Ziel innerhalb geeigneter Vernetzungsstrutkturen suchen
				geometry naechstesZiel <- last(sort_by(overlapping(vernetzung,circle(100, self.location)),each.grid_value));
				ziel <- towards(self.location,naechstesZiel.location);
			}
		}
	}
	
	action wandern {
		//innerhalb geeigneter Strukturen?
		
		v_wert <- first_with(vernetzung,intersects(each,self)).grid_value;
		//v_wert <- first(of_species(agents_overlapping(self.location), vernetzung)).grid_value;
		
		if ziel = 999.9 {
			do ziel_festlegen(4);
		}	
		if v_wert < 40 and flip(0.2){
			do ziel_festlegen(4);
		}		
		//if start_wanderung!=nil and (current_date - start_wanderung)/3600/24 > 40 {
		//	self.wandern <- 2;
		//	self.ziel <- 999.9;
		//}
		do move heading: self.ziel speed: 2 #m/#day;
	}
	
	action fortpflanzen {
		if overlaps(self.heimisches_gewaesser.shape,self.location){
			do wander bounds: self.heimisches_gewaesser.shape speed: 0.1 #m/#day;
			if count(overlapping(kammmolch,circle(10,self.location)),each.female=false)>=1 and self.female {
				create kammmolch number:300 {
					self.display_form <- circle(0);
					self.color <- #transparent;
					self.bcolor <- female ? #black : #white;
					self.location <- myself.location;
					self.heimisches_gewaesser <- myself.heimisches_gewaesser;
					self.gewaesser_flaeche <- myself.gewaesser_flaeche;
					self.eier_legedatum <- current_date;
					self.stadium <- "ei";
 					self.female <- flip (0.5);
 					self.wandern <- 0;
 					
					}
				self.letzte_fortpflanzung <- current_date;
				//write letzte_fortpflanzung;
			}
			
		}
		else if self.wandern=1 and self.stadium="adult"{
			do ziel_festlegen (2);
			self.wandern <- 2;
			do wandern;
		}
		else {
			do ziel_festlegen (1);
			do wandern;
		}
	}
	
	action verstecken {
		do ziel_festlegen(4);
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
			species bb aspect:gis;
			species kammmolch aspect:standard;
			grid vernetzung transparency:0.5;			
			}
		display Grafiken {
			chart "Zeitliche Entwicklung" type:series size:{0.9,1} position:{0,0} y_range:{0,500} {
				datalist [
					"Eier", 
					"Larven",
					"Juvenile",
					"Adulte"
				] color:[
					#silver,
					#green,
					#red,
					#blue
				] value: [
					count(kammmolch,each.stadium="ei"),
					count(kammmolch,each.stadium="larve"),
					count(kammmolch,each.stadium="juvenil"),
					count(kammmolch,each.stadium="adult")
				] marker:false;
			}
			chart "Stadien" type:histogram size:{0.1,1} position:{0.8,0} style:stack x_serie_labels:""
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
					count(kammmolch,each.stadium="larve"),
					count(kammmolch,each.stadium="juvenil"),
					count(kammmolch, each.stadium="adult")
				];
			}
		}
		monitor Datum value:current_date color:jahreszeit_color;
		monitor Tag value:current_date.day_of_year color:jahreszeit_color;
		monitor Jahreszeit value:display_jahreszeit color:jahreszeit_color;
	}
}