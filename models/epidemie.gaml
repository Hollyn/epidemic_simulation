/***
* Name: epidemie
* Author: Groupe1
* Description: 
* Tags: Tag1, Tag2, TagN
***/
model epidemie

global {
	/** Insert the global definitions, variables and actions here */
	
	file routes_shapefile <- ("../includes/roads.shp");
	file batiments_shapefile <- ("../includes/batiments.shp");
	file icon_hopital <- file ("../images/hopital.png");
	file icon_distraction <- file ("../images/fun.png");
	file icon_house <- file ("../images/house.png");
	file icon_work <- file ("../images/work.png");
	geometry shape <- envelope (routes_shapefile);
	list<batiments> batiments_habitable;
	list<batiments> batiments_travail;
	list<batiments> batiments_distraction;
	list<batiments> batiments_autres;
	
	//cette partie contient ou encore represente le nombre de jours necessaires a un individu en parfaite sante 
	//avant que la maladie ne ce manifeste apres contraction avec le virus
	int nb_jour_debut_maladie <- 2;
	//nombre patient maximal par medecin
	int nb_max_patient_medecin <- 15;
	int nb_hopital;
	int nb_age_minimal <- 2;
	int nb_age_maximal <- 100;
	int nb_rayon_contamination <- 5;
	int nb_rayon_virus <- 5;
	float duree_traitement_max <- 355 #m;
	float destroy <- 0.03;
	float total_aire_maison ;
	float total_aire_travail ;
	int cerrent_heure update: (time / # hour) mod 24;
	int min_debut_travail <- 6;
	int max_debut_travail <- 8;
	int min_fin_travail <- 16;
	int max_fin_travail <- 20;
	
	//cette partie contient le nombre de medecins que nous definissons dans le systeme
	int nb_medecin <- 10;
	float min_vitesse <- 1.1 # km / #h;
	float max_vitesse <- 5.0 # km / #h;
	float step <- 15;
	graph reseau_routier;
	int nb_personne <-100;
	
	//cette partie contient le nombre d'heure devant etre calcule de mamiere aleatoire en utilisant ces valeurs
	int min_debut_travail_garde <- 16;
	int max_debut_travail_garde <- 20;
	int min_fin_travail_garde <- 6;
	int max_fin_travail_garde <- 8; 
	
	int nb_personne_infectees <- 30 update: humain count (each.state = "malades");
	int nb_personne_traitement <- nb_personne_infectees update: humain count (each.state = "traitement");
	int nb_personne_sympthome <- nb_personne_infectees update: humain count (each.state ="sympthome");
	int nb_personne_non_infectees <- nb_personne - nb_personne_infectees update: nb_personne - nb_personne_infectees;
	float taux_infection <- nb_personne_infectees / nb_personne update: nb_personne_infectees / nb_personne;
	
	init {
		create route from: routes_shapefile;
		map<route, float> weights_map <-routes as_map (each::(each.destruction_coeff * each.shape.perimeter));
		reseau_routier <- as_edge_graph(route) with_weights weights_map;
		create batiments from: batiments_shapefile with: [type:: string(read('nature')),nb_personne::0]{
			
		if ((type = "hotel") or (type = "house") or (type =" motel") or (type = "appartements") or ("residences")){
			color <- #darkblue;
		}
		else if ((type ="public_building") or (type ="library") or (type ="place_of_worship") or (type ="office") or (type ="university") or (type ="company") or (type ="school")){
			color <- #pink;
		}
		else if ((type ="theatre") or (type ="church") or (type ="super_market") or (type ="cathedral") or (type ="museum") or (type ="bank") or (type ="hopital")){
			color <- #yellow;
		}
		else{
			color <- #gray;
		}
	}
		
		//ici on calcule le nbre des aires des maisons et le nbre d'habitants
		ask batiments where ((each.type ="hotel") or (each.type ="house") or (each.type ="motel") or (each.type ="appartements") or (each.type ="residences")){
			total_aire_maison <- total_aire_maison + self.shape.area;
		}
		
		//ici nous calculons le nbre d'hopital dans la ville
		ask batiments where ((each.type ="hopital")){
			nb_hopital <- nb_hopital + 1;
		}
		
		//ici on calcul la quantite d'aire des entreprises et le nbre d'habitants pour chaque entreprise
		ask batiments where ((each.type ="public_building") or (each.type ="library") or (each.type ="place_of_worship") or (each.type ="office")
			or (each.type ="school") or (each.type ="company") or (each.type ="university")){
				total_aire_travail <- total_aire_travail + self.shape.area;
			}
			
			//les batiments des habitants
		batiments_habitation <- batiments where ((each.type ="hotel") or (each.type ="house") or (each.type ="motel") or (each.type ="appartements") or (each.type ="residences"));
		//les batiments de travail
		batiments_de_travail <- batiments where ((each.type ="public_building") or (each.type ="library") or (each.type ="place_of_worship") or (each.type ="office") or (each.type ="school")
			or (each.type ="company") or (each.type ="university"));
		//les batiments de loisir
		batiments_distraction <- batiments where ((each.type ="theatre") or (each.type ="market_place") or (each.type ="church") or (each.type ="super_market") or (each.type ="cathedral")
			or (each.type ="museum") or (each.type ="bank") or (each.type ="cafe"));
			
			create humain number: nb_personne{
				age <- min_age + rnd(max_age - min_age);
			//definissons le niveau de sante
			niveau_sante <- 0.5 + rnd_float(0.5);
		//possibilite de tomber malade
		probabilite_tomber_malade <- (max_age +1 - age) / max_age - rnd_float(niveau_sante / max_age);
		color <-rgb(0, niveau_sante * 255, 0);
		//ici nous definissons un batiment ayant la capacite de contenir des personnes
		list<batiments> choix_habitation <- batiments where ((each.nb_personne < (each.shape.area / total_aire_maison) * (nb_personne + nb_medecin + 4 * nb_hopital)) and ((each.type ="hotel")
			or (each.type ="hous") or (each.type ="motel") or ("appartements") or (each.type ="residences")));
			ma_maison <- one_of(shuffle(choix_habitation));
			ma_maison.nb_personne <- ma_maison.nb_personne + 1;
			
			list<batiments> choix_travail <- batiments where ((each.nb_personne < (each.shape.area / total_aire_travail) * (nb_personne) and (each.type ="public_building") or (each.type ="library")
				or (each.type ="place_of_worship") or (each.type ="office") or (each.type ="school") or (each.type ="company") or (each.type ="university")));
				//choisir un lieu de travail
			lieu_de_travail <- one_of(shuffle(choix_travail));
			lieu_de_travail.nb_personne <- lieu_de_travail.nb_personne + 1;
			
			lieu_de_loisir <- one_of(shuffle(batiments_distraction));
			location <- any_location_in(ma_maison.shape);
			speed <- min_vitesse + rnd_float(max_vitesse - min_vitesse);
			
			debut_heure_travail <- min_debut_travail + rnd(max_debut_travail - min_debut_travail);
			fin_heure_travail <- min_fin_travail + rnd(max_fin_travail - min_fin_travail);
			
			objectif <- "repos";
			target <- any_location_in(lieu_de_travail.shape);
			set state <- "normal";
			}
		ask nb_personne_infectees among humain{
			set state <- "sympthome";
		}
	//ici nous choisissons le 1/4 de medecin pour etre en garde 
	  ask batiments where ((each.type ="hopital")){
	  	int nb_medecin_hosts <- 0;
	  	
	  	list<medecin> list_doct <- medecin where ((each.lieu_de_travail = myself));
	  	int i <- 0;
	  	loop while: i <- length(list_doct) / 4 {
	  	//attribuer des heures de nuit
	  	list_doct[i].debut_heure_travail <- min_debut_travail_garde + rnd(max_debut_travail_garde - min_debut_travail_garde);
	  	list_doct[i].fin_debut_travail <- min_fin_travail_garde + rnd(max_fin_travail_garde - min_fin_travail_garde);
	  	i <- i +1;
	  	}
	  }
	  //creation des medecin dans notre travail
	  create medecin number: nb_medecin {
	  	//definissons l'age compris entre age minimal et maximal
	  	age <-  min_age + rnd(max_age - min_age);
	  	//definissons le niveau de sante
	  	niveau_sante <- 0.7 + rnd_float(0.5);
	  	//couleur definissant la sante
	  	color <- rgb(0, niveua_sante * 255, 0);
	  	//choisissant un batiment pouvant contenir un bon nbre de personne
	  	list<batiments> choix_habitation <- batiments where ((each.nb_personne < (each.shape.area / total_aire_maison) * (nb_personne + nb_medecin + 4 * nb_hopital)) and ((each.type ="hotel")
			or (each.type ="hous") or (each.type ="motel") or ("appartements") or (each.type ="residences")));
			ma_maison <- one_of(shuffle(choix_habitation));
			ma_maison.nb_personne <- ma_maison.nb_personne + 1;
			
			list<batiments> choix_travail <- batiments where ((each.nb_personne < (each.shape.area / total_aire_travail) * (nb_personne) and (each.type ="public_building") or (each.type ="library")
				or (each.type ="place_of_worship") or (each.type ="office") or (each.type ="school") or (each.type ="company") or (each.type ="university")));
				
				lieu_de_travail <- one_of(shuffle(choix_travail));
			lieu_de_travail.nb_personne <- lieu_de_travail.nb_personne + 1;
			
			lieu_de_loisir <- one_of(shuffle(batiments_distraction));
			location <- any_location_in(ma_maison.shape);
			speed <- min_vitesse + rnd_float(max_vitesse - min_vitesse);
			
			debut_heure_travail <- min_debut_travail + rnd(max_debut_travail - min_debut_travail);
			fin_heure_travail <- min_fin_travail + rnd(max_fin_travail - min_fin_travail);
			
			objectif <- "repos";
	// definissons la competance du medecin
	competance <- 0.8 + rnd_float(0.4);
	vitesse_traitement <- 0.5 + rnd_float(0.5);
	//possibilite pour que le medecin tombe malade
	probabilite_tomber_malade <- 1 - competance;
	color <- rgb(0, 0, 255 * competance);
	target <- any_location_in(lieu_de_travail.shape);
	set state <- "normal";		
	  }
	  
	  //les delegues medicaux
	  create delegue number: 4 * nb_hopital{
	  	//definissons l'age compris entre l'age minimal et maximal
	  	age <- 22 + rnd(max_age - 22);
	  	//definissons le niveau de sante
	  	niveau_sante <- 0.7 + rnd_float(0.5);
	  	//choisissons un batiment capable de contenir un bon nombre de personne
	  	list<batiments> choix_habitation <- batiments where ((each.nb_personne < (each.shape.area / total_aire_maison) * (nb_personne + nb_medecin + 4 * nb_hopital)) and ((each.type ="hotel")
			or (each.type ="hous") or (each.type ="motel") or ("appartements") or (each.type ="residences")));
			ma_maison <- one_of(shuffle(choix_habitation));
			ma_maison.nb_personne <- ma_maison.nb_personne + 1;
		//definissons un lieu de travail
		lieu_de_travail <- one_of(shuffle(choix_travail));
			lieu_de_travail.nb_personne <- lieu_de_travail.nb_personne + 1;
			
			lieu_de_loisir <- one_of(shuffle(batiments_distraction));
			location <- any_location_in(ma_maison.shape);
			speed <- min_vitesse + rnd_float(max_vitesse - min_vitesse);
			
			debut_heure_travail <- min_debut_travail + rnd(max_debut_travail - min_debut_travail);
			fin_heure_travail <- min_fin_travail + rnd(max_fin_travail - min_fin_travail);
			
			objectif <- "repos";
			
			probabilite_tomber_malade <- 0.0;
			// ils de la couleur grise
			color <- rgb(100, 100, 100);
			set state <- "normal";
			
			list<batiments> hopit <- batiments where ((each.type ="hopital") and (each.nb_delegue < 4));
			lieu_de_travail <- hopit[0];
			hopit[0].nb_delegue <- hopit[0].nb_delegue + 1;
			target <- any_location_in(lieu_de_travail.shape);
			
			if(hopit[0].nb_delegue = 4){
			debut_heure_travail <- min_debut_travail + rnd(max_debut_travail - min_debut_travail);
			fin_heure_travail <- min_fin_travail + rnd(max_fin_travail - min_fin_travail);	
			}
			else{
				debut_heure_travail <- min_debut_travail_garde + rnd(max_debut_travail_garde - min_debut_travail_garde);
				fin_heure_travail <- min_fin_travail_garde + rnd(max_fin_travail_garde - min_fin_travail_garde);
			}
			hopit[0].nb_personne <- hopit[0].nb_personne + 1;
	  }
	}
}

 species batiments{
 	int id;
 	string type;
 	rgb color;
 	int nb_personne;
 	int nb_delegue;
 	float height <- rnd_float(5);
 	
 	aspect base{
 		draw shape color: color;
 		if(type ="hopital"){
 			draw icon_hopital size: 7 at: {location.x, location.y};
 		}
 		
 		if((type ="hotel") or (type ="house") or (type ="motel") or (type ="appartements") or (type ="residences")){
 			draw icon_house size: 7 at: {location.x, location.y};
 		}
 		else if ((type ="public_building") or (type ="library") or (type ="place_of_worship") or(type ="office") or (type ="school")
 			or (type ="company") or (type ="university")){
 				draw icon_work size: 7 at: {location.x, location.y};
 			}
 		else if((type ="theatre") or (type ="market_place") or (type ="super_market") or (type ="church") or (type ="museum")
 			or (type ="bank") or (type ="cafe") or (type ="cathedral")){
 			
 			draw icon_distraction size: 7 at: {location.x, location.y};	
 			}
 	  }
 	  
 	  aspect geom3D{
 	  	draw shape color: color depth: depht;
 	  	if(type ="hopital"){
 	  		draw icon_hopital size: 7 at: {location.x, location.y, height + 4} depth: height;
 	  	}
 	  	if((type ="hotel") or (type ="house") or (type ="motel") or (type ="appartements") or (type ="residences")){
 	  		draw icon_house size: 7 at: {location.x, location.y, height + 4} depth: height;
 	  	}
 	  	else if((type ="public_building") or (type ="library") or (type ="place_of_worship") or(type ="office") or (type ="school")
 			or (type ="company") or (type ="university")){
 			draw icon_work size: 7 at: {location.x, location.y, height + 4} depth: height;	
 			}
 		else if((type ="theatre") or (type ="market_place") or (type ="super_market") or (type ="church") or (type ="museum")
 			or (type ="bank") or (type ="cafe") or (type ="cathedral")){
 			draw icon_distraction size: 7 at: {location.x, location.y, height + 4} depth: height;	
 			}
 	  }
 	//le nbre de patients dans la file d'attente
 	int compter_patient (medecin m){
 		int i <- 0;
 		bool trouver <- false;
 		loop while: i < nb_max_patient_medecin and trouver = false {
 			if(m.maladie[i] != nil){
 				i <- i + 1;
 			}
 		}
 		return i;
 	}
 	//enregistrement de l'arrive des malades
 	reflex enregistrer_venue_malade{
 		write "debut liste medecin" + shape.area;
 		ask humain at_distance (shape.area / 1.6) where ((each.state ="malade") and (each.medecin_traitant = nil)){
 			write "heure" + current_hour + "area" + self.shape.area;
 			
 			//medecin trie par ordre croissant
 		list<medecin> list_medecin <- medecin where (each.lieu_de_travail = myself and current_hour >= each.debut_heure_travail and current_hour <= each.fin_heure_travail)
 		sort_by(myself.compter_patient(each));
 		write "pffffffffff list medecin" + list_medecin;
 		
 		if((length(list_medecin) != 0) and (list_medecin[0].nb_patient < nb_mx_patient_medecin)){
 		self.medecin_traitant <- list_medecin[0];
 		list_medecin[0].malade[list_medecin[0].nb_patient] <- self;
 		list_medecin[0].nb_patient <- list_medecin[0].nb_patient;	
 		}
 	  }
 	}
 	//les malades que les medecin ont oublie de traite
 	reflex liberer_malade{
 		ask humain where ((each.state ="malade") and (each.medecin_taitant != nil)){
 			self.medecin_traitant <- nil;
 		}
 	}
 }
 
 species routes{
 	float destruction_coeff <- 1 + ((rnd(100)) / 100) max: 3.0;
 	int colorValue <- int(255 * (destruction_coeff - 1)) update: int(255 * (destruction_coeff - 1));
 	rgb color <- rgb(min([255, colorValue]), max([0, 255 - colorValue]), 0);
 	
 	geometry display_shape <- line(shape.points, 3.0);
 	aspect base{
 		draw shape color: color;
 	}
 	aspect geom3D{
 		draw shape color: color;
 	}
 }
 
 species humain skills:[moving] control: fsm{
 	int temps_sympthome;
 	list<humain> malade;
 	int nb_patient;
 	int multiply_speed;
 	int age;
 	rgb color;
 	point target;
 	batiments ma_maison;
 	batiments lieu_de_travail;
 	batiments lieu_de_loisir;
 	float niveau_sante;
 	float probabilite_tomber_malade;
 	string objectif;
 	string state <- "normal";
 	int debut_heure_travail;
 	int fin_heure_travail;
 	int temps_arrivee_hopital;
 	medecin medecin_traitant;
 	
 	init{
 		temps_sympthome <- 0;
 		malade <- list with(nb_max_patient_medecin, nil);
 		nb_patient <- 0;
 		multiply_speed <- 1;
 	}
 	
 	action choisir_hopital_malade (humain h){
 		//liste des hopitaux de la ville
 		niveau_sante <- niveau_sante / 2;
 		list<batiments> list_hopital_dist_croiss <- batiments where(each.type ="hopital") sort_by (h distance_to each);
 		
 		if(list_hopital_dist_croiss != nil){
 			target <- any_location_in(list_hopital_dist_croiss[0].shape);
 		}
 	}
 	
 	reflex sympthome when: state ="sympthome" and temps_sympthome <= nb_debut_tour_maladie{
 		temps_sympthome <- temps_sympthome + 1;
 	}
 	
 	reflex heure_pour_travailler when: cuurent_hour = debut_heure_travail and objectif ="repos" and (state ="normal" or state ="sympthome"){
 		objectif <- "travail";
 		target <- any_location_in(lieu_de_travail.shape);
 	}
 	
 	reflex heure_pour_retourner when: current_hour = fin_heure_travail and objectif ="travail" and (state ="normal" or state ="sympthome" or state ="traitement"){
 		objectif <- "repos";
 		target <- any_location_in(ma_maison.shape);
 		malade <- list with(nb_max_patient_medecin, nil);
 		nb_patient <- 0;
 	} 
 	
 	reflex contaminer when: state ="normal" or state ="sympthome" or state ="traitement" {
 		ask target: humain at_distance rayon_contamination {
 			
 			if(myself.state != "mort"){
 				//tous ceux qui ont des voisinages proche avec la possibilite de tomber malade
 				if(flip(myself.probabilite_tomber_malade)){
 					set self.state <- "sympthome";
 					write "contaminer" + myself + "nouveau_malade" + self;
 				}
 			}
 		}
 	}
 	//permettant d'aller a l'hopital
 	reflex aller_hopit when: temps_sympthome >= nb_debut_tour_maladie * niveau_sante and state ="sympthome"{
	     do action: choisir_hopital_malade(self);
	     niveau_sante <- niveau_sante + 2.0;
	     state <- "malade";
	     temps_sympthome <- 0;
  } 
  
  reflex liberer_virus when: state != "normal"{
  	create virus number: rnd(5) with: (location: { location.x + rnd(5), location.y + rnd(5)});
  }
  
  reflex progresser_maladie when: state ="malade" or state ="traitement"{
  	niveau_sante <- niveau_sante - niveau_sante / 500.0;
  	
  	//la couleur devient plus rouge pour signifier que le patient meurt
  	if(niveau_sante > 0.000000000000000002){
  		if(state ="malade"){
  			color <- rgb((1 - niveau_sante) * 255, 0, 0);
  		}
  	}
  	else{
  		niveau_sante <- 0.0;
  		color <- rgb(0, 0, 0);
  		set state <- "mort";
  	}
  }
  
  reflex deplacer when: target != nil and state != "mort"{
  	path path_followed <- self goto [ target::target, on::reseau_routier, return_path::true];
  	list<geometry> segments <- path_followed.segments;
  	loop line over: segments{
  		
  		float dist <- line.perimeter;
  		ask routes(path_followed agent_from_geometry line){
  			destruction_coeff <- destruction_coeff + (destroy * dist / shape.perimeter);
  		}
  	}
  	//pour que la personne ne passe au meme endroit
  	target <- nil;
  }
  
  state normal initial: true{
  	enter{
  		niveau_sante <- 0.5 + rnd_float(0.5);
  	}
  }
  state sympthome{
  }
  state malade{
  	//le patient meurt si son niveau de sante atteind 0.0
  	transition to: mort when: niveau_sante <= 0.0;
  }
  
  state traitement {
  	enter{
  		medecin m <- medecin_traitant;
  		//demande de rentrer a la maison
  		target <- any_location_in(ma_maison.shape);
  	}
  	//le traitement du medecin prend effet
  	niveau_sante <- niveau_sante + m.competance / 100;
  	
  	//en phase de traitement les agents ont de la couleur orange
  	color <- rgb(255, rnd(255), 0);
  	
  	if(niveau_sante > 0.5){
  		medecin_traitant <- nil;
  		niveau_sante <- 0.5 + rnd(0.5);
  		color <- rgb(0, 255 * niveau_sante, 0);
  	} 
  	//le patient redevient normal si toute fois son niveau de sante est > 0.5
  	transition to: normal when: niveau_sante >= 0.5;
  }
  state mort{
  	enter{
  		color <- rgb(0, 0, 0);
  	}
  }
  
  aspect base{
  	draw circle(5) color: color;
  }
  
  aspect geom3D{
  	draw cylinder(2, 8) color: color;
  	draw sphere(3) at: { location.x, location.y, 9} color: color;
  	
  	//le rayon de contamination de l'individu
  	if(state ="sympthome"){
  		draw circle(rayon_contamination) color: # white at: { location.x, location.y, 9} border: rgb(rnd(255), 0, 0);
  	}
  	else if (state ="traitement"){
  		//individu en traitement
  		draw circle(rayon_contamination / 2) color: # white at: { location.x, location.y, 9} border: rgb(rnd(255), 0,0);
  	}
  	else if(state ="malade"){
  	}
  	//individu malade dans un rayon de 2* rayon de contamination
  	draw circle(2 * rayon_contamination) color: # white at: {location.x, location.y, 9} border: color;
  }
}
 
 species medecin skills: [communicating] parent: humain {
 	float competance;
 	float vitesse_traitement;
 	
 	reflex traiter{
 		int i <- 0;
 		bool trouver <- false;
 		loop while: i < nb_max_patient_medecin and trouver = false {
 			
 			if(malade [i] != nil){
 				//il est parti en traitement
 				malade[i].medecin_traitant <- self;
 				malade[i].objectif <- "travail";
 				set malade[i].state <- "traitement";
 				malade[i] <- nil;
 				trouver <-true;
 			}
 			i <- i + 1;
 		}
 	}
 }
 
 species virus skills: [moving]{
   int observation;
   int age;
   int max_age;
   int color;
   int multiply_speed;
   init{
   	multiply_speed <- 5 + rnd(10);
   	age <- 0;
   	color <- # red;
   	observation <- rayon_virus;
   	speed <- float(6 + rnd(4));
   	max_age <- 2 + rnd(15);
   }
   
   reflex growup{
   	if(age < max_age){
   		age <- age + 1;
   	}
   }	
   
   reflex multiply {
   	create virus number: rnd(6) with: (location: {location.x + rnd(6), location.y + rnd(6)});
   }
   
   reflex die when: age >= max_age{
   	write "le virus meurt";
   	do die;
   } 
   
   aspect base {
   	draw circle (0.50) color: color;
   }
   
   reflex explorer {
   	do wander amplitude: 500;
   	write "le virus bouge";
   }
   
   reflex contaminer {
   	ask humain at_distance rayon_virus{
   		if(self.state !="mort"){
   			if(flip(self.probabilite_tomber_malade)){
   				set self.state <- "sympthome";
   				write "virus" + myself + "nouveau sympthome by virus" + self;
   			}
   		}
   	 }
   }
 }
 
 species delegue parent: humain {	
 }
 
experiment epidemie type: gui {
	/** Insert here the definition of the input and output of the model */
	parameter "nbre de personne dans la ville" var: nb_personne category: "ville";
	parameter "nbre de personne infectees dans la ville" var: nb_personne_infectees category: "ville";
	parameter "hre au plus tot pour debuter le travail" var: min_debut_travail category: "personne" min: 4 max: 8;
	parameter "hre au plus tard pour debuter le travail" var: max_debut_travail category: "personne" min: 8 max: 12;
	parameter "l'age miximal" var: max_age category: "personne" min: 100;
	parameter "le rayon de contamination des personnes" var: rayon_contamination category: "ville" min: 0;
	parameter "nbre de jours pour que le maladie se manifeste" var: nb_debut_tour_maladie category: "personne" min: 2;
	parameter "hre au plus tot pour terminer le travail" var: min_fin_travail category: "personne" min: 11 max: 15;
	parameter "hre au plus tard pour terminer le travail" var: max_fin_travail category: "personne" min: 16 max: 21;
	parameter "nbre de medecin" var: nb_medecin category: "medecin" min: 4;
	parameter "la vitesse minimale" var: min_vitesse category: "personne" min: 0.1 #km / #h;
	parameter "la vitesse miximale" var: max_vitesse category: "personne" min: 10 #km / #h;
	parameter "la duree du traitement maximale du patient" var: duree_max_traitement category: "personne" max: 4320 #minutes;
	parameter "l'age minimal" var: min_age category: "personne" min: 2;
	parameter "nbre de patients au maximum par medecin" var: nb_max_patient_medecin category: "medecin" min: 7;
	 
	output {
		display map{
		species routes aspect: base;
		species medecin aspect: base;
		species batiments aspect: base;
		species humain aspect: base;
		species virus aspect: base;
		species delegue aspect: base;	
		}
		
		monitor "taux de personnes infectees" value: taux_infectees;
		monitor "heure de la journee" value: current_hour;
		
		display graph{
			chart "etat de sante au niveau de la population" type: pie {
				data "personnes non malades" color: rgb(0, 255, 0) value: length(list(humain) where(each.state ="normal"));
				data "personnes sous traitement" color: rgb(200, 200, 0) value: length(list(humain) where(each.state ="traitement"));
				data "personnes mortes" color: rgb(0, 0, 0) value: length(list(humain) where(each.state ="mort"));
				data "les nouveaux contamines" color: rgb(253, 233, 224) value: length(list(humain) where(each.state ="sympthome"));
				data "personnes malades" color: rgb(255, 0, 0) value: length(list(humain) where(each.state ="malade"));
			}
		}
	}
}
