/**
* Name: kammmolche
* Based on the internal empty template. 
* Author: Philipp Franke
* Tags: Invasive Species, conservation, agend based modelling, triturus carnifex
*/


model italien_crested_newt

global {
	// Definition and representation seasons
	map seasons <- [nil::"init",1::"imigrate",2::"reproduction",3::"emigrate",4::"winter"];
	int seasonNow <- 1;
	string display_season <- "init";
	rgb season_color<- #white;
	// load shapefile
	file shp_bb <- file("./shp/bodenbedeckung_2.shp");
	geometry shape <- envelope(shp_bb);
	
//  ****** Parameter ********  //
	//  starting state
	int number_newts_start parameter:"Anzahl Kammmolche zu Beginn: " category: "Rahmenbedingungen" init:50;
	date starting_date parameter:"Start Datum" category: "Rahmenbedingungen" init:date([2000,2,28,0,0,0.0]);
	float step <- 1 #day;
	
	//  survivale rates of each stage
	float survival_egg parameter: "Überlebensrate Eier: " category:"Überlebensrate" init: 0.5;
	float survival_larva parameter: "Überlebenrate Larven: " category:"Überlebensrate" init: 0.4;
	float survival_juvenil parameter: "Überlebensrate Juvenil: " category: "Überlebensrate" init:0.4;
	float survival_adult parameter: "Überlebenrate Adulte, jährlich: " category:"Überlebensrate" init: 0.5;
	
	// reproduction and developement
	int maxAge_egg parameter: "Entwicklungsdauer Ei (Tage): " category: "Entwicklung" init:15;
	int maxAge_larva parameter: "Entwicklungsdauer Larve (Tage): " category: "Entwicklung" init: 90;
	int maxAge_juv parameter: "Entwicklungsdauer Juvenil (Tage): " category: "Entwicklung" init: 730;
	int clutchSize parameter: "Gelegegrösse (Eier / Weibchen): " category: "Entwicklung" init: 300;
	int reproductionBreak parameter: "Jahre ohne Fortpflanzung: " category: "Entwicklung" init: 2;
	
	// migration
	float migrationRate_adult parameter: "Rate der Auswanderer Adulte [%]: " category: "Wanderung" init: 1.2;
	float migrationRate_juv parameter: "Rate der Auswanderer Juvenil [%]: " category: "Wanderung" init: 37.3;
	
	//larval density
	float density_koeff parameter: "Koeffizient Dichteabhängige Sterblichkeit: " category: "Überlebensrate" init:0.1;
	
//  ****** state variablees ********  //
	int eggs_tot <- 0;
	int larva_tot <- 0;
	int juv_tot <- 0;
	int adult_tot <- 0;
	int migrantJuv_tot <- 0;
	int migrantAdult_tot <- 0;
	int occupiedPonds_number <- 0;
	
//	float max_larvendichte parameter: "Larvendichte Anz./m2" category:"Überlebensrate" init:1.0;	
//	int migrationDuration parameter: "Dauer der Wandertätigkeit [d]: " category: "Wanderung" init: 30; 
//	float winterImWasser parameter: "Anteil der adulten, welche im Wasser überwintern: " category: "Wanderung" init:0.95;


//  ****** initialisation ********  //
	init{
		create groundCover from:shp_bb with:[
			type::string(read("ART_TXT")),
			place_of_exposion::bool(read("aussetzung")),
			area::float(read("flaeche_m")),
			combatMeasures::bool(read("bekaempfun"))
		];
		create adult number:number_newts_start {
			self.location <- any_location_in(one_of(where (groundCover,each.place_of_exposion)));
			self.isFemale <- flip (0.5);
			self.color <- isFemale ? #springgreen : #seagreen;
			self.bcolor <- #maroon;
			self.display_form <- circle(5);
			self.nativePond <- first(overlapping(collect(groundCover,each),self.location));
			self.maturity <- starting_date - (rnd(500)*3600*24);
			self.lastReproduction <- starting_date - (rnd(500)*3600*24);
			self.isMigrant <- true;
		}
	}
	
	// set seasons according to the actual week of the year
	reflex seasons{
		switch current_date.week_of_year {
			match_between [0,8] {
				seasonNow<- 1;
				season_color<- #lightcyan;
			}
			match_between [9,35] {
				seasonNow <- 2;
				season_color<- #lightskyblue;
			}
			match_between [36,44] {
				seasonNow <- 3;
				season_color<- #mediumslateblue;
			}
			match_between [44,53] {
				seasonNow <- 4;
				season_color<- #snow;
			}
		}
		display_season <- seasons[seasonNow];
	}
	
	// calculations
	reflex calculation {
		occupiedPonds_number <- count(groundCover,each.newtCount>0);
	}
}

species groundCover{
	string type;
	float area;
	bool place_of_exposion;
	bool combatMeasures;
	rgb color;
	int newtCount;
	float densityLarva <- 0.0;
	aspect standard {
		draw shape color: color;
	}
	init{
		switch self.type{
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
			self.newtCount <- count(adult, each.nativePond = self);
		}
	}
	reflex calculations {
		// Berechnung densityLarva in Larven pro Quadratmeter pro Gewässer
		int larvaCount <- sum(collect(where(offspring,!each.egg and each.nativePond = self),each.count));
		densityLarva <- larvaCount / self.area;
		// Berechnung Anzahl Kammmmolche pro Gewässer
		self.newtCount <- count(adult, each.nativePond = self);
	}	
}

species offspring {
	// Eier und Larven werden nicht als einzelne Agenten geführt sondern nummerisch in einem von der Muttern abstammenden Nachwuchs-Agenten
	groundCover nativePond;
//	float pondArea;
	date layDate_egg;
	date hatchDate_larva <- date(10000101);
	bool allreadyDeath_larva <- false;		// sorgt dafür, dass dichteabhängige Sterblichkeit nur einmal in der ganzen Phase angewendet wird.
	bool egg; //"true" wenn Stadium = Ei, "false" wenn Stadium = Larve
	int count;
	float densityLarva;
	float survival_larven;
	rgb color <- #silver;
	aspect standard {
		draw square(2) color:color border:#black;
	}
	reflex development {
		//dichteabhängig Sterblichkeit
		float density <- any(where(groundCover,each=self.nativePond)).densityLarva+0.00001;		//verhindern von "division by zero"
		//float dichte_linear <- dichte/(5*max_larvendichte);
		float densityMortalityRate_larva <- (density_koeff*(density)^2 + (1-survival_larva))/maxAge_larva;
		ask where(offspring, each.nativePond = self.nativePond and !each.egg){ 
			self.count <- round(self.count * (1-densityMortalityRate_larva));
		}
		
		// Entwicklung vom Ei zur Larve
		if current_date > (self.layDate_egg + maxAge_egg*24*3600) and egg {
			self.egg <- false;
			self.color <- #green;
			self.count <- round(self.count * survival_egg);
			self.hatchDate_larva <- current_date;
			larva_tot <- larva_tot + self.count;
		}
		// Entwicklung von Larve zu Juvenil
		else if current_date > (self.hatchDate_larva + maxAge_larva*24*3600) and !egg {
			create juvenil number: self.count { 
				self.location <- myself.location;
				self.isFemale <- flip(0.5);
				self.nativePond <- myself.nativePond;
				self.dateDevelopment <- current_date;
				self.isMigrant <- true;
			}
			juv_tot <- juv_tot + self.count; 
			do die;
		}
	}
}

species juvenil {
	groundCover nativePond;
	bool isFemale;
	date dateDevelopment;
	bool isMigrant;
	
	aspect standard {
		draw square(2) color:#red;
	}
	
	reflex entwicklung {
		// Entwicklung von juvenil zu adult
		if current_date > (dateDevelopment + maxAge_juv*3600*24) {
			if flip (survival_juvenil) {
				create adult number:1 {
					self.nativePond <- myself.nativePond;
					self.location <- myself.location;
					self.isFemale <- myself.isFemale;
					self.maturity <- current_date;
					self.lastReproduction <- current_date-500*3600*24;
				}
				adult_tot <- adult_tot + 1;
			}
			do die;
		}
		// wandernde Juvenile, nicht im Winter
		else if seasonNow !=4 { 
			if flip(migrationRate_juv/100) and self.isMigrant {
				create migrant number:1 {
					self.isJuvenil <- true;
					self.dateDevelopment <- myself.dateDevelopment;
					self.beginMigration <- current_date;
					self.isFemale <- myself.isFemale;
					self.location <- myself.location;
					self.isMigrant <- true;
					self.target <- rnd(359.9);
				}
				migrantJuv_tot <- migrantJuv_tot +1;
				do die;
			}
			else {
				self.isMigrant <- false;
			}
		}
	}
}

species migrant skills: [moving]{
	bool isJuvenil; //false wenn adult, true wenn juvenil
	bool isMigrant;
	date dateDevelopment;
	date lastReproduction;
	date beginMigration;
	bool isFemale;
	float target;	// Wanderrichtung in Grad;
	float speed; // Wanderdistanz in Metern;
	geometry migrationSteps_line; // Linie zwischen Ziel- und Endpunkt des Wanderschrittes
	point targetPoint;	// Endpukt der Wanderung,
	
	groundCover nativePond;
		
	aspect standard {
		draw square(4) color: #brown;
		//draw migrationSteps_line color: #black;
		//draw targetPoint color: #purple;
	}
	
	reflex migrate {
		// wandernde Tiere wandern zur passenden Jahreszeit
		//wenn Wanderzeit vorbei ist, wechseln die Tiere das angestammte Gewässer und wandern dort ein
		if isMigrant and seasonNow != 3 {
			self.nativePond <- closest_to(where(groundCover,each.type='Gewaesser.stehendes'),self);
			self.target <- towards(self.location,nativePond.location); 
			float left_right <- 1.0;
			float deviation <- 0.0;
			self.isMigrant <- false;
		}
		else if !isMigrant and overlaps(self.location,buffer(self.nativePond,20)) {
			if self.isJuvenil {
				create juvenil number:1 {
					self.nativePond <- myself.nativePond;
					self.isFemale <- myself.isFemale;
					self.dateDevelopment <- myself.dateDevelopment;
					self.location <- myself.nativePond.location;
					self.isMigrant <- false;
				}
				do die;
			}
			else {
				create adult number: 1 {
					self.nativePond <- myself.nativePond;
					self.isFemale <- myself.isFemale;
					self.maturity <- myself.dateDevelopment;
					self.lastReproduction <- myself.lastReproduction;
					self.location <- myself.nativePond.location;
					self.isMigrant <- false;
				}
				do die;
			}
		}
		// im Winter wird eine Pause eingelegt
		if seasonNow = 3 {
			
		}
		// normale Wanderbewegung
		else if isMigrant {
			do change_course;
		}
		
	//Brechnung ob ein Gebäude im Weg steht, und wandern, Im Winter ist stillstand!
		if seasonNow!=4 {
			self.targetPoint <- {self.location.x + (cos(self.target)*speed) ,self.location.y+(sin(self.target)*speed)};
			self.migrationSteps_line <- line(self.location,targetPoint);
			groundCover obstacle <- first(where(groundCover,intersects(each,line(self.location, targetPoint)) and each.type="Gebaeude"));
			if obstacle != nil {
				self.speed <- distance_to(self, obstacle.shape);
			}
			if !self.isMigrant and self.speed = 5 {
				self.target <- towards(self.location,nativePond.location);
				self.speed <- 5.0 + rnd(20.0);
			}		
			if self.speed < 0.5 {
				self.target <- self.target + 30;
				self.speed <- 5.0;
			}
			do move speed: speed #m/#day heading: self.target;	
		}
	}
	
	//Wanderbewegungen sind nicht gradlinig, sondern taumelnd 
	action change_course {
		float deviation <- rnd(30.0);
		float left_right <- flip(0.5) ? 1.0: -1.0;
		self.target <- self.target + left_right*deviation;
		self.target <- self.target > 360 ? self.target-360 : self.target;
		self.target <- self.target < 0 ? self.target+360 : self.target;
		self.speed <- 5.0 + rnd(20.0);
	}
}

// Adulte Kammmolche
species adult skills: [moving]{
	date maturity;
	date beginMigration;
	date lastReproduction;
	groundCover nativePond;
	bool isMigrant;
	bool isFemale;
	rgb color;
	rgb bcolor;
	geometry display_form <- circle(10);
	float target <- 999.9;
//	float distance;
//	float v_wert;
	
	aspect standard 
	{
		draw circle(5) color: #transparent border:#aqua;
	}
	
	reflex seasons {
		if seasonNow = 2{
			if lastReproduction = nil or (self.lastReproduction add_years reproductionBreak < current_date) {
				do reproduction;
			}
		}
		else if seasonNow = 3 {
			//	Wandern
			if flip(migrationRate_adult/100) and self.isMigrant{
				create migrant number:1{
					self.beginMigration <- current_date;
					self.isJuvenil <- false;
					self.dateDevelopment <- myself.maturity;
					self.lastReproduction <- myself.lastReproduction;
					self.isFemale <- myself.isFemale;
					self.location <- myself.location;
					self.isMigrant <- true;
					self.target <- rnd(359.9);
				}
				migrantAdult_tot <- migrantAdult_tot + 1;
				write "Adult: " + self + " wandert!";
			}
			else {
				isMigrant <- false;
			}
		}
		else if current_date.day_of_year = 365 {
			// sterben
			if flip(1-survival_adult){
				do die;
			}
		}
	}
	
	action reproduction {
		//do wander bounds: self.heimisches_gewaesser.shape speed: 0.1 #m/#day;
		if overlaps(self.nativePond.shape,self.location) {		
			if count(overlapping(adult,self.nativePond.shape),!each.isFemale)>=1 and self.isFemale{
				create offspring number:1 {
					self.location <- myself.location;
					self.egg <- true;
					self.count <- clutchSize;
					self.nativePond <- myself.nativePond;
					self.layDate_egg <- current_date; 					
					}
				eggs_tot <- eggs_tot + clutchSize;
				self.lastReproduction <- current_date;
			}
		}
	}
}

experiment complete {
	list<groundCover> newtsPerPond;
	reflex Berechnungen_und_Ausgabe_Excel {
		newtsPerPond <- sort(where(groundCover,each.type="Gewaesser.stehendes"),1-each.newtCount);
		int schwelle <- collect(newtsPerPond,each.newtCount)[16];
		newtsPerPond <- where(newtsPerPond,each.newtCount > schwelle);
		
		string filePath <- "./result/" + "resultate.csv";
		bool newFile <- current_date = starting_date ? true : false; 
		ask simulation {
			save [
				current_date,
				sum(collect(where(offspring,each.egg),each.count)),
				sum(collect(where(offspring,!each.egg),each.count)),
				count(juvenil,true) + count(migrant, each.isJuvenil),
				count(adult,true) + count(migrant, !each.isJuvenil)
			] to:filePath type: "csv" rewrite:newFile;
		}
	}
	output {
		display Map {
			species groundCover aspect:standard;
			species adult aspect:standard;
			species offspring aspect:standard;
			species juvenil aspect:standard;
			species migrant aspect:standard;			
			}
		display Charts {
			chart "Stages" type:series size:{0.9,0.5} position:{0,0} y_log_scale: true {
				datalist [
					"Eggs", 
					"Larvae",
					"Juvenile",
					"Adults",
					"Migrants"
				] color:[
					#silver,
					#green,
					#red,
					#aqua,
					#brown
				] value: [
					sum(collect(where(offspring,each.egg),each.count)),
					sum(collect(where(offspring,!each.egg),each.count)),
					count(juvenil,true) + count(migrant, each.isJuvenil),
					count(adult,true) + count(migrant, !each.isJuvenil),
					count(migrant, true)
				] marker:false;
			}
			chart "Ponds" type:histogram size:{0.9,0.5} position:{0,0.5} {
				datalist [collect(newtsPerPond,each.name)]
				color: [#cornflowerblue]
				value: [collect(newtsPerPond,each.newtCount)]
				;
			}
			chart "Stages" type:histogram size:{0.1,1} position:{0.9,0} style:stack x_serie_labels:""
				{
				datalist [
					"Larvae",
					"Juvenil, incl juvenile migrants",
					"Adults, incl adult migrants"
				]
				color: [
					#green,
					#red,
					#aqua
				]
				value:[
					sum(collect(where(offspring,!each.egg),each.count)),
					count(juvenil,true),
					count(adult, true)
				];
			}
		}
		monitor Date value:current_date color:season_color;
		monitor Day value:current_date.day_of_year color:season_color;
		monitor Season value:display_season color:season_color;
		monitor Eggs value: sum(collect(where(offspring,each.egg),each.count)) color:#silver;
		monitor Larvae value: sum(collect(where(offspring,!each.egg),each.count)) color:#green;
		monitor Juvenile value: (count(juvenil,true) + count(migrant,each.isJuvenil)) color:#red;
		monitor Adults value: (count(adult,true) + count(migrant,!each.isJuvenil)) color:#aqua;
		monitor Migrants value: count(migrant,true) color:#brown; 
	}	
}

experiment calibration type:batch repeat:1 until:current_date=date("20200101") {
	parameter coeff var:density_koeff among:[0.005,0.005,0.005,0.01,0.01,0.01,0.1,0.1,0.1]; 
	reflex stages_count {
		ask simulations {
			save [
				current_date,
				self.name,
				density_koeff,
				self.eggs_tot,
				self.larva_tot,
				self.juv_tot,
				self.adult_tot,
				self.migrantJuv_tot,
				self.migrantAdult_tot,
				self.occupiedPonds_number
			] to:"./result/kalibration.csv" type:"csv" rewrite:self.name='Simulation 0' ? true : false;
		}	
	}
	permanent {
		monitor laid_eggs value:eggs_tot color:#grey;
		monitor hatched_larvae value:larva_tot color:#green;
		monitor developed_juvenile value: juv_tot color:#red;
		monitor developed_adults value: adult_tot color:#aqua;
		monitor occupied_ponds value: occupiedPonds_number color:#blue;
	}
}