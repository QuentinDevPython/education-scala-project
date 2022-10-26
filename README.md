# Projet solveur de contraintes
## ESIEE - Functional programming in Scala

- Quentin BARTHELEMY
- Clément BOUDOU
- Maxime BOURGAIN
- Baptiste BONTOUX

Ce solveur de contrainte permet de réduire des variables associées à un ensemble de valeur en appliquant des contraintes. Le résultat étant réduit à 1 valeur par variable par défaut.

Deux types de problèmes ont été implémentés :
- Sudoku
  - Easy
  - Medium
  - Hard
  - Impossible
- Map Coloring
  - Facile - L'Australie
  - Moyen - Les Etats-Unis

  
Contraintes implémentées :
 - VariableX = VariableY : `EqualVariables(VariableX, VariableY)`
   - Variable X = constante : `EqualConstant(VariableX, constante)`
 - VariableX ≠ VariableY : `DiffVariables(VariableX, VariableY)`
   - Variable ≠ contante : `DiffConstant(VariableX, constante)`
 - [VariableA, VariableB, ...] sont toutes différentes : `AllDiff(List(VariableA, VariableB, ...))`


Fonctionnement général :
![Schéma](https://zupimages.net/up/22/43/t90r.png)

