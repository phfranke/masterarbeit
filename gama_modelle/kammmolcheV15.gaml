/**
* Name: kammmolche
* Based on the internal empty template. 
* Author: Philipp Franke
* Tags: Invasive Species, conservation, agend based modelling, triturus carnifex
*/


model italien_crested_newt

global {
//  ****** global variables ********  //
	// defining seasons
	map seasons <- [nil::"init",1::"imigrate",2::"reproduction",3::"emigrate",4::"winter"];
	int seasonNow <- 1;
	string display_season <- "init";
	rgb season_color<- #white;
	
	// load shapefile
	file shp_bb <- file("./shp/bodenbedeckung_2.shp");
	geometry shape <- envelope(shp_bb);
	
//  ****** Parameter ********  //
	//  time management
	int number_newts_start parameter:"Number of newts at starting date: " category: "System" init:50;
	date starting_date parameter:"Startdate" category: "System" init:date([2000,1,1,0,0,0.0]);
	date stop_date parameter: "Enddate" category: "System" init:date([2020,1,2,0,0,0.0]);
	float step <- 1 #day;
	date calibrationEnd parameter: "Enddate of calibration run: " category: "System" init: date([2022,1,1,0,0,0.0]);
	
	//  survivale rates of each stage
	float survival_egg parameter: "Survival rate eggs (0..1): " category:"Survival Rates" init: 0.5;
	float survival_larva parameter: "Survival rate larvae (0..1): " category:"Survival Rates" init: 0.4;
	float survival_juvenil parameter: "Survival rate juvenil (0..1): " category: "Survival Rates" init:0.4;
	float survival_adult parameter: "Survival rate adults, each year (0..1): " category:"Survival Rates" init: 0.5;
		
	//larval density
	float density_koeff parameter: "Coefficient mortality by density: " category: "Survival Rates" init:0.1;
	
	
	// reproduction and developement
	int maxAge_egg parameter: "Days to develop from egg to larva : " category: "Development" init:15;
	int maxAge_larva parameter: "Days to develop from larva to juvenile: " category: "Development" init: 90;
	int maxAge_juv parameter: "Days to develop from juvenile to adult: " category: "Development" init: 200;
	int clutchSize parameter: "Clutch size (egg/female): " category: "Development" init: 300;
	int reproductionBreak parameter: "Years without reproduction : " category: "Development" init: 2;
	
	// migration
	float migrationRate_adult parameter: "Rate of adults which migrate (0..1): " category: "Migration" init: 0.12;
	float migrationRate_juv parameter: "Rate of juvenile which migrate (0..1): " category: "Migration" init: 0.373;
	float migration_velocity parameter: "Distance in Meter per Day" category: "Migration" init: 15.0;
	float migration_time parameter: "Duration of the migration period in days: " category: "Migration" init: 40.0;
	
	// catching
	int start_catching parameter: "Year in which catching measures start: " category: "Catching" init: 2021;
	float probabilityCatch parameter: "Probality to be catched (0..1): " category: "Catching" init: 0.8;

//  ****** state variablees ********  //
	int eggs_tot <- 0;
	int larva_tot <- 0;
	int juv_tot <- 0;
	int adult_tot;
	int migrantJuv_tot <- 0;
	int migrantAdult_tot <- 0;
	int occupiedPonds_count <- 0;
	list<pond> occupiedPonds_list;
	float min_age_adult;
	float max_age_adult;
	float mean_age_adult;
	float median_age_adult;
	float sd_age_adult;
	int catched_adult_tot;
	int catched_juv_tot;
	float min_age_adult_catched <- 999999;
	float max_age_adult_catched <- 0;
	float mean_age_adult_catched <- 0;	

//  ****** initialisation ********  //
	init{
		adult_tot <- number_newts_start;
		create groundCover from:shp_bb with:[
			type::string(read("ART_TXT")),
			place_of_exposion::bool(read("aussetzung")=1?true:false),
			area::float(read("flaeche_m")),
			catchingNet::bool(read("bekaempfun")),
			pondNumber::string(read("Karch-Nr"))
		];
		create adult number:number_newts_start {
			self.location <- any_location_in(one_of(where (pond,each.place_of_exposion)));
			self.isFemale <- flip (0.5);
			self.color <- isFemale ? #springgreen : #seagreen;
			self.bcolor <- #maroon;
			self.display_form <- circle(5);
			self.nativePond <- first(overlapping(collect(pond,each),self.location));
			self.maturity <- starting_date - (rnd(500)*3600*24);
			self.lastReproduction <- self.maturity;
			self.isMigrant <- true;
		}
	}
	
	// set seasons according to the actual week of the year
	reflex seasons_setter {
		switch current_date.week_of_year {
			match_between [0,8] {
				seasonNow <- 1;
				season_color <- #lightcyan;
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
				season_color <- #snow;
			}
		}
		display_season <- seasons[seasonNow];
	}
	
	// calculations
	reflex count_occupied_ponds {
		occupiedPonds_list <- [];
		list<pond> occupiedPonds_list1 <- where(pond,each.newtCount > 0);
		loop pond over: occupiedPonds_list1 {
			if !contains_all(collect(occupiedPonds_list,each.pondNumber), [pond.pondNumber]){
				occupiedPonds_list <- occupiedPonds_list + pond;
			}
		}
		occupiedPonds_count <- count(occupiedPonds_list,true);
	}
	
	reflex adult_age {
		if current_date.day_of_year = 365 {
			list<int> age_adult <- collect(adult,(current_date - each.maturity)/3600/24);
			min_age_adult <- min(age_adult);
			max_age_adult <- max(age_adult);
			mean_age_adult <- mean(age_adult);
			median_age_adult <- median(age_adult);
			sd_age_adult <- standard_deviation(age_adult);
		}
	}
}

//  ****** Background ********  //
species groundCover{
	string type;
	float area;
	bool place_of_exposion;
	bool catchingNet;
	string pondNumber;
	rgb color;
	int newtCount;
	float densityLarva <- 0.0;
	aspect standard {
		draw shape color: color;
	}
	init{
		switch self.type{
			match "Gewaesser.stehendes"{
				create pond {
					self.area <- myself.area;
					self.place_of_exposion <- myself.place_of_exposion;
					self.shape <- myself.shape;
					self.catchingNet <- myself.catchingNet;
					self.pondNumber <- myself.pondNumber;
					self.newtCount <- myself.newtCount;
					self.densityLarva <- 0.0;
					self.color <- #blue;
				}
				do die;
			}
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
}

species pond {
	string type;
	float area;
	bool place_of_exposion;
	bool catchingNet;
	string pondNumber;
	rgb color;
	int newtCount;
	int larvaCount;
	float densityLarva;
	
	aspect standard {
		draw shape color: color;
	}
	reflex newt_count {
		self.newtCount <- count(adult, each.nativePond = self);
	}
	reflex larval_density {
		// calculation of larval density [number of larvae per m2] in each occupied pond
		larvaCount <- sum(collect(where(offspring, each.nativePond = self and !each.isEgg),each.count));
		
		self.densityLarva <- larvaCount / self.area;
	}
}

//  ******  species declaration ********  //
species offspring {
	// Eggs and larvae are managed in de species "offspring"
	pond nativePond;
	//	float pondArea;
	date layDate_egg;
	date hatchDate_larva <- date(10000101);
	bool isEgg; 	//"true" if stage = egg, "false" if stage = larva
	int count;		// number of eggs or larvae
//	float survival_larva;
	rgb color <- #silver;
	float densityMortalityRate_larva;
	aspect standard {
		draw square(2) color:color border:#black;
	}
//	reflex mortality_by_density {
//		float density <- first(collect(where(pond,each=self.nativePond),each.densityLarva));
//		densityMortalityRate_larva <- (density_koeff*(density^2) + (1-survival_larva))/maxAge_larva;
//		densityMortalityRate_larva <- densityMortalityRate_larva > 1.0 ? 1.0 : densityMortalityRate_larva;
//		densityMortalityRate_larva <- densityMortalityRate_larva < 0.0 ? 0.0 : densityMortalityRate_larva;
//		list<offspring> larvaInSamePond <- where(offspring, each.nativePond = self.nativePond and !each.isEgg); 
//		ask larvaInSamePond {
//			self.count <- round(self.count * (1-myself.densityMortalityRate_larva));
//			if (myself.densityMortalityRate_larva > 1 or myself.densityMortalityRate_larva < 0) {
//				write ('mortalityRate: ' + myself.densityMortalityRate_larva);
//			}
//		}
//	}
	
	reflex hatch {
		// Entwicklung vom Ei zur Larve
		if current_date > (self.layDate_egg + maxAge_egg*24*3600) and self.isEgg {
			self.isEgg <- false;
			self.color <- #green;
			self.count <- round(self.count * survival_egg);
			self.hatchDate_larva <- current_date;
			larva_tot <- larva_tot + self.count;
		}
	}
	reflex develop {
		// Berechnung dichteabhängige Sterblichkeit
		float density <- first(collect(where(pond,each=self.nativePond),each.densityLarva));
		densityMortalityRate_larva <- density_koeff*(density^2) + (1-survival_larva);
		densityMortalityRate_larva <- densityMortalityRate_larva > 1.0 ? 1.0 : densityMortalityRate_larva;
		densityMortalityRate_larva <- densityMortalityRate_larva < 0.0 ? 0.0 : densityMortalityRate_larva;
		int survivers <- round((1-densityMortalityRate_larva) * self.count);
		// Entwicklung von Larve zu Juvenil
		if current_date > (self.hatchDate_larva + maxAge_larva*24*3600) and !self.isEgg {
//			write survivers;
			create juvenil number: survivers { 
				self.location <- myself.location;
				self.isFemale <- flip(0.5);
				self.nativePond <- myself.nativePond;
				self.dateDevelopment <- current_date;
				self.isMigrant <- true;
			}
			juv_tot <- juv_tot + survivers; 
			do die;
		}
	}
}

species juvenil {
	pond nativePond;
	bool isFemale;
	date dateDevelopment;
	bool isMigrant;
	bool slipped <- false;
	
	aspect standard {
		draw square(2) color:#red;
	}
	
	reflex development_to_adult {
		// development of juvenil to adult
		if current_date > (dateDevelopment + maxAge_juv*3600*24) {
			if flip (survival_juvenil) {
				create adult number:1 {
					self.nativePond <- myself.nativePond;
					self.location <- myself.location;
					self.isFemale <- myself.isFemale;
					self.maturity <- current_date;
					self.lastReproduction <- current_date subtract_years reproductionBreak;  // to prevent empty variables
				}
				adult_tot <- adult_tot + 1;
			}
			do die;
		}
	}
	
	reflex migrating {
		// migrating juveniles, not in wintertime
		if seasonNow !=4 { 
			if flip(migrationRate_juv) and self.isMigrant {
				create migrant number:1 {
					self.isJuvenil <- true;
					self.dateDevelopment <- myself.dateDevelopment;
					self.beginMigration <- current_date;
					self.isFemale <- myself.isFemale;
					self.location <- myself.location;
					self.isMigrant <- true;
					self.target <- rnd(359.9);
					self.velocity <- flip(0.1) ? 3*migration_velocity: migration_velocity;
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
	bool isJuvenil;  // true means stage = juvenil, false means stage= adult
	bool isMigrant;
	date dateDevelopment;
	date lastReproduction;
	date beginMigration;
	bool isFemale;
	float target;	// migration direction in Degrees;
	float velocity; // migration distance per step in meters/day, individual speed;
	float velocity_real; // real migration distance, is calculated;
	geometry migrationSteps_line; // Line between startpoint and endpoint of each step
	point targetPoint;	// endpoint of each migration step
	pond nativePond;
		
	aspect standard {
		draw square(2) color: #brown;
		draw migrationSteps_line color: #black;
		draw targetPoint color: #purple;
	}
	
	reflex homing {
		date end_migration <- add_days(beginMigration, migration_time);
		// maigration stops when season = "imigration"
//		if self.isMigrant and seasonNow = 1 {
		if self.isMigrant and (current_date > end_migration) {
			self.nativePond <- closest_to(pond,self);
			self.target <- towards(self.location,nativePond.location); 
			float left_right <- 1.0;
			float deviation <- 0.0;
			self.isMigrant <- false;
		}
		// transform to origin stage when migrant is near of his new home pond 
		else if !isMigrant and overlaps(self.location,buffer(self.nativePond,20)) {
			if self.isJuvenil {
				// juveniles are catched while entering a pond with net
				if current_date.year >= start_catching and self.nativePond.catchingNet and between(current_date.month,2,5) and flip(probabilityCatch) {
					catched_juv_tot <- catched_juv_tot + 1;
					do die;
				}
				create juvenil number:1 {
					self.nativePond <- myself.nativePond;
					self.isFemale <- myself.isFemale;
					self.dateDevelopment <- myself.dateDevelopment;
					self.location <- myself.nativePond.location;
					self.isMigrant <- false;
					self.slipped <- true;
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
		
		if isMigrant {
			do change_course;
		}
		
		//calculation of obstacles (=buildings), normal movement, nothing happens in Wintertime
		if seasonNow!=4 {
			self.targetPoint <- {self.location.x + (cos(self.target)*self.velocity) ,self.location.y+(sin(self.target)*self.velocity)};
			self.migrationSteps_line <- line(self.location,targetPoint);
			groundCover obstacle <- first(where(groundCover,intersects(each,self.migrationSteps_line) and each.type="Gebaeude"));
			if obstacle != nil {
				self.velocity_real <- distance_to(self.location, obstacle.shape);
			}
			if !self.isMigrant {
				self.target <- towards(self.location,self.nativePond.location);
			}
			if self.velocity_real < 0.5 {
				self.target <- self.target + 30;
				self.velocity_real <- 5.0;
			}
			do move speed: self.velocity_real #m/#day heading: self.target;	
		}
	}
	
	//Wanderbewegungen sind nicht gradlinig, sondern taumelnd 
	action change_course {
		float deviation <- rnd(10.0);
		float left_right <- flip(0.5) ? 1.0: -1.0;
		self.target <- self.target + left_right*deviation;
		self.target <- self.target > 360 ? self.target-360 : self.target;
		self.target <- self.target < 0 ? self.target+360 : self.target;
		self.speed <- self.velocity #m/#day;
	}
}

species adult skills: [moving]{
	date maturity;
	date beginMigration;
	date lastReproduction;
	pond nativePond;
	bool isMigrant;
	bool isFemale;
	rgb color;
	rgb bcolor;
	geometry display_form <- circle(7);
	float target <- 999.9;
	bool slipped <- false;
//	float distance;
//	float v_wert;
	
	aspect standard 
	{
		draw circle(5) color: #transparent border:#aqua;
	}
	
	reflex seasons_triage {
		if seasonNow = 2 {
			// reproduction
			if lastReproduction = nil or (add_years(self.lastReproduction, reproductionBreak) < current_date) {
				do reproduction;
			}
		}
		else if seasonNow = 3 {
			//	migration
			if flip(migrationRate_adult) and self.isMigrant {
				create migrant number:1 {
					self.beginMigration <- current_date;
					self.isJuvenil <- false;
					self.dateDevelopment <- myself.maturity;
					self.lastReproduction <- myself.lastReproduction;
					self.isFemale <- myself.isFemale;
					self.location <- myself.location;
					self.isMigrant <- true;
					self.target <- rnd(359.9);
					self.velocity <- flip(0.1) ? 3*migration_velocity: migration_velocity;
				}
				do die;
				migrantAdult_tot <- migrantAdult_tot + 1;
				write "Adult: " + self + " wandert!";
			}
			else {
				isMigrant <- false;
			}
		}
		else if current_date.day_of_year = 365 {
			// die
			if flip(1-survival_adult){
				do die;
			}
		}
	}
	
	reflex catching {
		if current_date.year >= start_catching and 
			current_date.year >= (self.lastReproduction.year + reproductionBreak) and 
			!self.slipped and self.nativePond.catchingNet
		{
			if flip(probabilityCatch) {
				float age <- (current_date - self.maturity)/3600/24;
				if min_age_adult_catched >= age { min_age_adult_catched <- age;}
				if max_age_adult_catched <= age { max_age_adult_catched <- age;}
				mean_age_adult_catched <- ((mean_age_adult_catched * catched_adult_tot) + age) / (catched_adult_tot + 1);
				catched_adult_tot <- catched_adult_tot + 1;
				do die;
			}
			else {
				self.slipped <- true;
			}
		}
	}
	
	action reproduction {
		if overlaps(self.nativePond.shape,self.location) {		
			if count(overlapping(adult,self.nativePond.shape),!each.isFemale)>=1 and self.isFemale{
				create offspring number:1 {
					self.location <- myself.location;
					self.isEgg <- true;
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


//  ****** experiments ********  //
experiment complete type:batch repeat:1 parallel: false until:current_date=stop_date {
	parameter catching_probability var:probabilityCatch among:[0.95,0.8,0.5];
	list<pond> newtsPerPond;
	int schwelle;
	
	reflex output {
		string filePath <- "./result/" + "resultate.csv";
		bool newFile <- current_date.year = starting_date.year ? true : false; 
		ask simulation {
			if current_date.day_of_year =1 {
				save [
					current_date,
					probabilityCatch,
					sum(collect(where(offspring,each.isEgg),each.count)),
					sum(collect(where(offspring,!each.isEgg),each.count)),
					count(juvenil,true) + count(migrant, each.isJuvenil),
					count(adult,true) + count(migrant, !each.isJuvenil),
					count(adult,each.slipped),
					count(adult,!each.slipped),
					self.catched_adult_tot,
					self.catched_juv_tot,
					self.eggs_tot,
					self.larva_tot,
					self.juv_tot,
					self.adult_tot,
					self.migrantJuv_tot,
					self.migrantAdult_tot
				] to:filePath type: "csv" rewrite:newFile;	
			}
		}
	}
	output {
		display Map {
			species groundCover aspect:standard;
			species pond aspect:standard;
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
					sum(collect(where(offspring,each.isEgg),each.count)),
					sum(collect(where(offspring,!each.isEgg),each.count)),
					count(juvenil,true) + count(migrant, each.isJuvenil),
					count(adult,true) + count(migrant, !each.isJuvenil),
					count(migrant, true)
				] marker:false;
			}
			chart "Ponds" type:histogram size:{0.9,0.5} position:{0,0.5} {
				datalist [collect(occupiedPonds_list,each.pondNumber)]
				color: [#cornflowerblue]
				value: [collect(occupiedPonds_list,each.newtCount)]
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
					sum(collect(where(offspring,!each.isEgg),each.count)),
					count(juvenil,true),
					count(adult, true)
				];
			}
		}
		monitor Date value:current_date color:season_color;
		monitor Day value:current_date.day_of_year color:season_color;
		monitor Season value:display_season color:season_color;
		monitor Eggs value: sum(collect(where(offspring,each.isEgg),each.count)) color:#silver;
		monitor Larva value: sum(collect(where(offspring,!each.isEgg),each.count)) color:#green;
		monitor Juvenile value: (count(juvenil,true) + count(migrant,each.isJuvenil)) color:#red;
		monitor Adults value: (count(adult,true) + count(migrant,!each.isJuvenil)) color:#aqua;
		monitor Migrants value: count(migrant,true) color:#brown;
	}
}

experiment calibration_density type:batch repeat:3 parallel: false until:current_date=calibrationEnd {
	parameter coeff var:density_koeff among:[0.01, 0.008, 0.006];

	reflex save_results {
	
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
				count(migrant,each.isJuvenil),
				count(migrant,!each.isJuvenil),
				self.occupiedPonds_count,
				count(offspring, each.isEgg),
				count(offspring, !each.isEgg),
				count(juvenil,true),
				count(adult,true),
				count(adult, each.slipped),
				count(adult, !each.slipped),
				count(juvenil, each.slipped),
				count(juvenil, !each.slipped),
				self.catched_adult_tot,
				self.catched_juv_tot,
				self.min_age_adult,
				self.max_age_adult,
				self.mean_age_adult,
				self.median_age_adult,
				self.sd_age_adult,
				self.catched_adult_tot,
				self.catched_juv_tot,
				self.min_age_adult_catched,
				self.max_age_adult_catched,
				self.mean_age_adult_catched
			] to:"./result/kalibration_density.csv" type:"csv" rewrite:self.name='Simulation 0' ? true : false;
		}	
	}
	
}

experiment calibration_migration type:batch repeat:5 parallel: false until:current_date=calibrationEnd {
	parameter coeff var:migration_time among:[20.0,30.0,40.0];

	permanent {
		monitor laid_eggs value:eggs_tot color:#grey;
		monitor hatched_larvae value:larva_tot color:#green;
		monitor developed_juvenile value: juv_tot color:#red;
		monitor developed_adults value: adult_tot color:#aqua;
		monitor occupied_ponds value: occupiedPonds_count color:#blue;
	}
	
	reflex save_results {
	
		ask simulations {
			save [
				current_date,
				self.name,
				self.migration_time,
				self.occupiedPonds_count,
				self.occupiedPonds_list
			] to:"./result/kalibration_migration.csv" type:"csv" rewrite:self.name='Simulation 0' ? true : false;
		}	
	}
	
}