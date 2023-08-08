/**
* Name: kammmolche
* Based on the internal empty template. 
* Author: Philipp Franke
* Tags: Invasive Species, conservation, agend based modelling, triturus carnifex
*/


model kammmolche

global {
	// Definition und Darstellung Jahreszeiten
	map jahreszeiten <- [nil::"init",1::"Zuwanderung",2::"Fortpflanzung",3::"Abwanderung",4::"Winter"];
	int aktuelleJahreszeit <- 1;
	string display_jahreszeit <- "init";
	rgb jahreszeit_color <- #white;
	// shapefiles für die Umgebung laden
	file shp_bb <- file("./shp/bodenbedeckung.shp");
	geometry shape <- envelope(shp_bb);
	// Parameter
	int anz_km_start parameter:"Anzahl Kammmolche zu Beginn: " category: "Rahmenbedingungen" init:50;
	date starting_date parameter:"Start Datum" category: "Rahmenbedingungen" init:date([2000,2,28,0,0,0.0]);
	float step <- 1 #day;
	float ueberlebensrate_eier parameter: "Überlebensrate Eier: " category:"Überlebensrate" init: 0.5;
	float ueberlebensrate_larve parameter: "Überlebenrate Larven: " category:"Überlebensrate" init: 0.4;
	float ueberlebensrate_juvenil parameter: "Überlebensrate Juvenil: " category: "Überlebensrate" init:0.4;
	float ueberlebensrate_adult parameter: "Überlebenrate Adulte, jährlich: " category:"Überlebensrate" init: 0.5;
	float max_larvendichte parameter: "Larvendichte Anz./m2" category:"Überlebensrate" init:1.0;
	float dichtekoeffizient parameter: "Koeffizient Dichteabhängige Sterblichkeit: " category: "Überlebensrate" init:0.1;
	float wanderAdult parameter: "Rate der Auswanderer Adulte [%]: " category: "Wanderung" init: 1.2;
	float wanderJuvenil parameter: "Rate der Auswanderer Juvenil [%]: " category: "Wanderung" init: 37.3;
	int wanderDauer parameter: "Dauer der Wandertätigkeit [d]: " category: "Wanderung" init: 30; 
	float winterImWasser parameter: "Anteil der adulten, welche im Wasser überwintern: " category: "Wanderung" init:0.95;
	int zeit_ei parameter: "Entwicklungsdauer Ei (Tage): " category: "Entwicklung" init:15;
	int zeit_larve parameter: "Entwicklungsdauer Larve (Tage): " category: "Entwicklung" init: 90;
	int zeit_juvenil parameter: "Entwicklungsdauer Juvenil (Tage): " category: "Entwicklung" init: 730;
	int gelegegroesse parameter: "Gelegegrösse (Eier / Weibchen): " category: "Entwicklung" init: 300;
	int fortpflanzungPause parameter: "Jahre ohne Fortpflanzung: " category: "Entwicklung" init: 2;
	int anz_gelegte_eier <- 0;
	int anz_geschluepfte_larven <- 0;
	int anz_entw_juvenil <- 0;
	int anz_entw_adult <- 0;
	int anz_wanderer_juvenil <- 0;
	int anz_wanderer_adult <- 0;
	int anz_bewohnte_gewaesser <- 0;
	
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
			self.wandern <- true;
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
	reflex berechnung {
		anz_bewohnte_gewaesser <- count(bb,each.anzahl_km>0);
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
			self.anzahl_km <- count(kammmolch, each.heimisches_gewaesser = self);
		}
	}
	reflex berechnungen {
		// Berechnung Larvendichte in Larven pro Quadratmeter pro Gewässer
		int anzahl_larven <- sum(collect(where(nachwuchs,!each.ei and each.heimisches_gewaesser = self),each.anzahl));
		larvendichte <- anzahl_larven / self.flaeche;
		// Berechnung Anzahl Kammmmolche pro Gewässer
		self.anzahl_km <- count(kammmolch, each.heimisches_gewaesser = self);
	}	
}

species nachwuchs {
	// Eier und Larven werden nicht als einzelne Agenten geführt sondern nummerisch in einem von der Muttern abstammenden Nachwuchs-Agenten
	bb heimisches_gewaesser;
	float gewaesser_flaeche;
	date eier_legedatum;
	date larve_schluepfdatum <- date(10000101);
	bool larve_bereitsgestorben <- false;		// sorgt dafür, dass dichteabhängige Sterblichkeit nur einmal in der ganzen Phase angewendet wird.
	bool ei; //"true" wenn Stadium = Ei, "false" wenn Stadium = Larve
	int anzahl;
	float larvendichte;
	float ueberlebensrate_larven;
	rgb color <- #silver;
	aspect standard {
		draw square(2) color:color border:#black;
	}
	reflex entwicklung {
		//dichteabhängig Sterblichkeit
		float dichte <- any(where(bb,each=self.heimisches_gewaesser)).larvendichte+0.00001;		//verhindern von "division by zero"
		//float dichte_linear <- dichte/(5*max_larvendichte);
		float sterb_larven <- (dichtekoeffizient*(dichte)^2 + (1-ueberlebensrate_larve))/zeit_larve;
		ask where(nachwuchs, each.heimisches_gewaesser = self.heimisches_gewaesser and !each.ei){ 
			self.anzahl <- round(self.anzahl * (1-sterb_larven));
		}
		
		// Entwicklung vom Ei zur Larve
		if current_date > (self.eier_legedatum + zeit_ei*24*3600) and ei {
			self.ei <- false;
			self.color <- #green;
			self.anzahl <- round(self.anzahl*ueberlebensrate_eier);
			self.larve_schluepfdatum <- current_date;
			anz_geschluepfte_larven <- anz_geschluepfte_larven + self.anzahl;
		}
		// Entwicklung von Larve zu Juvenil
		else if current_date > (self.larve_schluepfdatum + zeit_larve*24*3600) and !ei {
			create juvenil number: self.anzahl { 
				self.location <- myself.location;
				self.female <- flip(0.5);
				self.heimisches_gewaesser <- myself.heimisches_gewaesser;
				self.entwicklungsdatum <- current_date;
				self.wandern <- true;
			}
			anz_entw_juvenil <- anz_entw_juvenil + self.anzahl; 
			do die;
		}
	}
}

species juvenil {
	bb heimisches_gewaesser;
	bool female;
	date entwicklungsdatum;
	bool wandern;
	
	aspect standard {
		draw square(2) color:#red;
	}
	
	reflex entwicklung {
		// Entwicklung von juvenil zu adult
		if current_date > (entwicklungsdatum + zeit_juvenil*3600*24) {
			if flip (ueberlebensrate_juvenil) {
				create kammmolch number:1 {
					self.heimisches_gewaesser <- myself.heimisches_gewaesser;
					self.location <- myself.location;
					self.female <- myself.female;
					self.geschlechtsreife <- current_date;
					self.letzte_fortpflanzung <- current_date-500*3600*24;
				}
				anz_entw_adult <- anz_entw_adult + 1;
			}
			do die;
		}
		// wandernde Juvenile, nicht im Winter
		else if aktuelleJahreszeit !=4 { 
			if flip(wanderJuvenil/100) and self.wandern {
				create migrant number:1 {
					self.juvenil <- true;
					self.entwicklungsdatum <- myself.entwicklungsdatum;
					self.start_wanderung <- current_date;
					self.female <- myself.female;
					self.location <- myself.location;
					self.wandern <- true;
					self.ziel <- rnd(359.9);
				}
				anz_wanderer_juvenil <- anz_wanderer_juvenil +1;
				do die;
			}
			else {
				self.wandern <- false;
			}
		}
	}
}

species migrant skills: [moving]{
	bool juvenil; //false wenn adult, true wenn juvenil
	bool wandern;
	date entwicklungsdatum;
	date letzte_fortpflanzung;
	date start_wanderung;
	bool female;
	float ziel;	// Wanderrichtung in Grad;
	float tempo; // Wanderdistanz in Metern;
	geometry korridor; // Linie zwischen Ziel- und Endpunkt des Wanderschrittes
	point zielpunkt;	// Endpukt der Wanderung,
	
	bb heimisches_gewaesser;
		
	aspect standard {
		draw square(4) color: #brown;
		//draw korridor color: #black;
		//draw zielpunkt color: #purple;
	}
	
	reflex wandern {
		// wandernde Tiere wandern zur passenden Jahreszeit
		//wenn Wanderzeit vorbei ist, wechseln die Tiere das angestammte Gewässer und wandern dort ein
		if wandern and aktuelleJahreszeit != 3 {
			self.heimisches_gewaesser <- closest_to(where(bb,each.typ='Gewaesser.stehendes'),self);
			self.ziel <- towards(self.location,heimisches_gewaesser.location); 
			float links_rechts <- 1.0;
			float kurswechsel <- 0.0;
			self.wandern <- false;
		}
		else if !wandern and overlaps(self.location,buffer(self.heimisches_gewaesser,20)) {
			if self.juvenil {
				create juvenil number:1 {
					self.heimisches_gewaesser <- myself.heimisches_gewaesser;
					self.female <- myself.female;
					self.entwicklungsdatum <- myself.entwicklungsdatum;
					self.location <- myself.heimisches_gewaesser.location;
					self.wandern <- false;
				}
				do die;
			}
			else {
				create kammmolch number: 1 {
					self.heimisches_gewaesser <- myself.heimisches_gewaesser;
					self.female <- myself.female;
					self.geschlechtsreife <- myself.entwicklungsdatum;
					self.letzte_fortpflanzung <- myself.letzte_fortpflanzung;
					self.location <- myself.heimisches_gewaesser.location;
					self.wandern <- false;
				}
				do die;
			}
		}
		// im Winter wird eine Pause eingelegt
		if aktuelleJahreszeit = 3 {
			
		}
		// normale Wanderbewegung
		else if wandern {
			do kurswechsel;
		}
		
	//Brechnung ob ein Gebäude im Weg steht, und wandern, Im Winter ist stillstand!
		if aktuelleJahreszeit!=4 {
			self.zielpunkt <- {self.location.x + (cos(self.ziel)*tempo) ,self.location.y+(sin(self.ziel)*tempo)};
			self.korridor <- line(self.location,zielpunkt);
			bb barriere <- first(where(bb,intersects(each,line(self.location, zielpunkt)) and each.typ="Gebaeude"));
			if barriere != nil {
				self.tempo <- distance_to(self, barriere.shape);
			}
			if !self.wandern and self.tempo = 5 {
				self.ziel <- towards(self.location,heimisches_gewaesser.location);
				self.tempo <- 5.0 + rnd(20.0);
			}		
			if self.tempo < 0.5 {
				self.ziel <- self.ziel + 30;
				self.tempo <- 5.0;
			}
			do move speed: tempo #m/#day heading: self.ziel;	
		}
	}
	
	//Wanderbewegungen sind nicht gradlinig, sondern taumelnd 
	action kurswechsel {
		float kurswechsel <- rnd(30.0);
		float links_rechts <- flip(0.5) ? 1.0: -1.0;
		self.ziel <- self.ziel + links_rechts*kurswechsel;
		self.ziel <- self.ziel > 360 ? self.ziel-360 : self.ziel;
		self.ziel <- self.ziel < 0 ? self.ziel+360 : self.ziel;
		self.tempo <- 5.0 + rnd(20.0);
	}
}

// Adulte Kammmolche
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
	float v_wert;
	
	aspect standard 
	{
		draw circle(5) color: #transparent border:#aqua;
	}
	
	reflex jahreszeit {
		if aktuelleJahreszeit = 2{
			if letzte_fortpflanzung = nil or (self.letzte_fortpflanzung add_years fortpflanzungPause < current_date) {
				do fortpflanzen;
			}
		}
		else if aktuelleJahreszeit = 3 {
			//	Wandern
			if flip(wanderAdult/100) and self.wandern{
				create migrant number:1{
					self.start_wanderung <- current_date;
					self.juvenil <- false;
					self.entwicklungsdatum <- myself.geschlechtsreife;
					self.letzte_fortpflanzung <- myself.letzte_fortpflanzung;
					self.female <- myself.female;
					self.location <- myself.location;
					self.wandern <- true;
					self.ziel <- rnd(359.9);
				}
				anz_wanderer_adult <- anz_wanderer_adult + 1;
				write "Adult: " + self + " wandert!";
			}
			else {
				wandern <- false;
			}
		}
		else if current_date.day_of_year = 365 {
			// sterben
			if flip(1-ueberlebensrate_adult){
				do die;
			}
		}
	}
	
	action fortpflanzen {
		//do wander bounds: self.heimisches_gewaesser.shape speed: 0.1 #m/#day;
		if overlaps(self.heimisches_gewaesser.shape,self.location) {		
			if count(overlapping(kammmolch,self.heimisches_gewaesser.shape),!each.female)>=1 and self.female{
				create nachwuchs number:1 {
					self.location <- myself.location;
					self.ei <- true;
					self.anzahl <- gelegegroesse;
					self.heimisches_gewaesser <- myself.heimisches_gewaesser;
					self.eier_legedatum <- current_date; 					
					}
				anz_gelegte_eier <- anz_gelegte_eier + gelegegroesse;
				self.letzte_fortpflanzung <- current_date;
			}
		}
	}
}

experiment komplett {
	list<bb> kmProGewaesser;
	reflex Berechnungen_und_Ausgabe_Excel {
		kmProGewaesser <- sort(where(bb,each.typ="Gewaesser.stehendes"),1-each.anzahl_km);
		int schwelle <- collect(kmProGewaesser,each.anzahl_km)[16];
		kmProGewaesser <- where(kmProGewaesser,each.anzahl_km > schwelle);
		
		string pfad <- "./result/" + "resultate.csv";
		bool neuesfile <- current_date = starting_date ? true : false; 
		ask simulation {
			save [
				current_date,
				sum(collect(where(nachwuchs,each.ei),each.anzahl)),
				sum(collect(where(nachwuchs,!each.ei),each.anzahl)),
				count(juvenil,true) + count(migrant, each.juvenil),
				count(kammmolch,true) + count(migrant, !each.juvenil)
			] to:pfad type: "csv" rewrite:neuesfile;
		}
	}
	output {
		display Karte {
			species bb aspect:standard;
			species kammmolch aspect:standard;
			species nachwuchs aspect:standard;
			species juvenil aspect:standard;
			species migrant aspect:standard;			
			}
		display Grafiken {
			chart "Stadien" type:series size:{0.9,0.5} position:{0,0} y_log_scale: true {
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
					#aqua,
					#brown
				] value: [
					sum(collect(where(nachwuchs,each.ei),each.anzahl)),
					sum(collect(where(nachwuchs,!each.ei),each.anzahl)),
					count(juvenil,true) + count(migrant, each.juvenil),
					count(kammmolch,true) + count(migrant, !each.juvenil),
					count(migrant, true)
				] marker:false;
			}
			chart "Gewässer" type:histogram size:{0.9,0.5} position:{0,0.5} {
				datalist [collect(kmProGewaesser,each.name)]
				color: [#cornflowerblue]
				value: [collect(kmProGewaesser,each.anzahl_km)]
				;
			}
			chart "Stadien" type:histogram size:{0.1,1} position:{0.9,0} style:stack x_serie_labels:""
				{
				datalist [
					"Larve",
					"Juvenil, inkl Wanderer",
					"Adult, inkl. Wanderer"
				]
				color: [
					#green,
					#red,
					#aqua
				]
				value:[
					sum(collect(where(nachwuchs,!each.ei),each.anzahl)),
					count(juvenil,true),
					count(kammmolch, true)
				];
			}
		}
		monitor Datum value:current_date color:jahreszeit_color;
		monitor Tag value:current_date.day_of_year color:jahreszeit_color;
		monitor Jahreszeit value:display_jahreszeit color:jahreszeit_color;
		monitor Eier value: sum(collect(where(nachwuchs,each.ei),each.anzahl)) color:#silver;
		monitor Larven value: sum(collect(where(nachwuchs,!each.ei),each.anzahl)) color:#green;
		monitor Juvenile value: (count(juvenil,true) + count(migrant,each.juvenil)) color:#red;
		monitor Adulte value: (count(kammmolch,true) + count(migrant,!each.juvenil)) color:#aqua;
		monitor Wanderer value: count(migrant,true) color:#brown; 
	}	
}

experiment kalibration type:batch repeat:3 until:cycle=7300 {
	parameter koeffizient var:dichtekoeffizient among:[0.005,0.01,0.1]; 
	reflex count_stadien {
		ask simulations {
			save [
				current_date,
				self.name,
				dichtekoeffizient,
				self.anz_gelegte_eier,
				self.anz_geschluepfte_larven,
				self.anz_entw_juvenil,
				self.anz_entw_adult,
				self.anz_wanderer_juvenil,
				self.anz_wanderer_adult,
				self.anz_bewohnte_gewaesser
			] to:"./result/kalibration.csv" type:"csv" rewrite:self.name='Simulation 0' ? true : false;
		}	
	}
	permanent {
		monitor Anzahl_gelegte_Eier value:anz_gelegte_eier color:#grey;
		monitor Anzahl_geschluepfte_Larven value:anz_geschluepfte_larven color:#green;
		monitor Anzahl_entwickelte_Juvenile value: anz_entw_juvenil color:#red;
		monitor Anzahl_entwickelte_Adulte value: anz_entw_adult color:#aqua;
		monitor Anzahl_bewohnte_Gewaesser value: anz_bewohnte_gewaesser color:#blue;
	}
}
//		int anz_larven <- sum(collect(where(nachwuchs,!each.ei),each.anzahl));
////		self.sterb_ei <- (anz_geschluepfte_larven != 0 and anz_gelegte_eier != 0) ? 1- anz_geschluepfte_larven / (anz_gelegte_eier- sum(collect(where(nachwuchs,each.ei),each.anzahl))) : 0;
////		self.sterb_larve <- (anz_entw_juvenil != 0 and anz_geschluepfte_larven !=0) ? 1-anz_entw_juvenil / (anz_geschluepfte_larven - anz_larven +1): 0;
////		self.sterb_juvenil <- (anz_entw_adult !=0 and anz_entw_juvenil != 0) ? 1-anz_entw_adult / anz_entw_juvenil : 0;
//		write "anz lebende Larven: " + anz_larven + ", Sterberate: " + sterb_larve;
//		string pfad <- "./result/" + "sterblichkeit.csv";
//		bool neueDatei <- current_date=starting_date ? true : false;
//			ask simulation {
//				save [
//					current_date,
////					anz_gelegte_eier,
////					anz_geschluepfte_larven,
//					anz_larven,
//					anz_entw_juvenil,
////					anz_entw_adult,
////					anz_wanderer_juvenil,
////					anz_wanderer_adult,
////					myself.sterb_ei,
//					myself.sterb_larve
////					myself.sterb_juvenil
//				] to:pfad type: "csv" rewrite:neueDatei;
//			}
//		}
//	//}
//	output {
//		display Grafik {
//			chart "Summierte Stadien" type:series size:{1,0.5} position:{0,0}{
//				datalist [
//					"Anzahl gelegte Eier",
//					"Anzahl geschlüpfte Larve",
//					"Anzahl Entwickelte Juvenile",
//					"Anzahl Entwickelte Adulte"
//				] color: [
//					#grey,
//					#green,
//					#red,
//					#aqua
//				] value: [
//					anz_gelegte_eier,
//					anz_geschluepfte_larven,
//					anz_entw_juvenil,
//					anz_entw_adult
//				] marker: false;
//			}
////			chart "Sterblichkeit" type:series size:{1,0.5} position:{0,0.5}{
////				datalist [
//////					"Überleben Eier",
////					"Sterberate Larve"
//////					"Überleben Juvenile"
//////					"Anzahl Entwickelte Adulte"
////				] color: [
//////					#grey,
////					#green
//////					#red
//////					#aqua
////				] value: [
//////					self.sterb_ei,
////					self.sterb_larve
//////					self.sterb_juvenil
//////					anz_entw_adult
////				] marker: false;
////			}
//		}
//		monitor Datum value:current_date color:jahreszeit_color;
//		monitor Anzahl_gelegte_Eier value:anz_gelegte_eier color:#grey;
//		monitor Anzahl_geschluepfte_Larven value:anz_geschluepfte_larven color:#green;
//		monitor Anzahl_entwickelte_Juvenile value: anz_entw_juvenil color:#red; 
//		monitor Sterblichkeit_Eier value:(anz_geschluepfte_larven != 0 and anz_gelegte_eier != 0) ? 1-anz_geschluepfte_larven / anz_gelegte_eier : 0 color:#grey;
//		monitor Sterblickeit_Larven value: (anz_entw_juvenil != 0 and anz_geschluepfte_larven !=0) ? sterb_larve : 0 color:#green;
//		monitor Sterblickeit_Juvenile value: (anz_entw_adult !=0 and anz_entw_juvenil != 0) ? 1-anz_entw_adult / anz_entw_juvenil : 0 color:#red;
//	}