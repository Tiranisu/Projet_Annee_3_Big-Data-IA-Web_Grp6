# Projet_Annee_3_Big-Data/IA/WebGrp6

# Description :
Approfondir les compétences acquises dans les modules Big Data, Intelligence Artificielle, Développement Web et Base de Données à travers une application complète de traitements et de visualisation de données concernant les accidents corporels de la circulation routière en France.

# To do list :
### Créer des histogrammes
  - [x] Construire des séries chronologiques sur l’évolution du nombre d’accidents par mois et semaines sur l’ensemble de la période
  - [x] Construire un jeu de données avec le nombre d’accidents selon la gravité pour 100.000 habitants par région (qui servirait à l’Analyse en Composantes Principales (ACP) discutée dans la section 3 de l’analyse de données)
  
 ### Créer des histogrammes 
  - [ ] Créer des représentations graphiques
    - [ ] Nombre d’accidents en fonction des conditions atmosphériques
    - [ ] Nombre d’accidents en fonction de la description de la surface
    - [x] Nombre d’accidents selon la gravité
    - [ ] Nombre d’accidents par tranches d’heure
    - [ ] Nombre d’accidents par ville


  - [x] Créer des histogrammes
    - [x] Quantité d’accidents en fonction des tranches d’âges
    - [x] Moyenne mensuelle des accidents
  - [ ] Proposer une représentation sous formes de carte de la quantité d’accidents enregistrés par région puis par départements
  - [ ] Même chose avec les taux d’accidents graves
  - [ ] Exporter et sauvegarder vos figures en png

 ### Analyse des données
  - [ ] Etude des relations entre variables qualitatives
    - [ ] Faire des tableaux croisées et des tests d’indépendance du chi2 sur les tableaux entre les différentes variables
    - [ ] Représenter graphiquement ces tableaux (mosaicplot) et les analyser
  - [ ] Calculer les régressions linéaires sur l’évolution du nombre d’accidents par mois, puis par semaine.
    - [ ] Comparer les résultats obtenus par les deux régressions mentionnées ci-dessus
      - [ ] Analyser les performances de la régression (proportion de la variabilité due aux résidus et aux variables explicatives)
      - [ ] Analyser des erreurs types associés aux estimateurs
      - [ ] Calculer les intervalles de confiance à 95% pour ces estimateurs
      - [ ] Calculer les R2 et R2 ajusté pour les deux modèles. Qu’en déduire ?
      - [ ] Quelle est la qualité des prédictions basées sur ces modèles de régression (à la semaine et au mois)?
  - [ ] Bonus: ACP-AFC-ACM sur le jeu de données avec le nombre d’accidents selon la gravité pour 100.000habitants par région
    - [ ] Eboulis des valeurs propres pour déterminer le nombre de variables principales à utiliser
    - [ ] A quelle part d’inertie, ça correspond
    - [ ] Contributions des variables (gravité) et des individus (régions) sur les axes principaux
    - [ ] Représentation des variables sur le cercle des corrélations
    - [ ] Interprétation des axes dans le plan principal
