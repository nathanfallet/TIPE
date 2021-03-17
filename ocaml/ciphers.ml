(*
* Projet TIPE sur la cryptographie
* et le chiffrement
*
* Partie : Ciphers
*
* Auteurs :
* FALLET Nathan
* LAMIDEL Arthur
* MAKALOU Shérif
*)

(*
* Import des fichiers annexes
*)

open Aes

(*
* ECB
*
* On passe juste les données par l'algorithme
*)

let ecb_e entree cle = chiffrer entree cle

let ecb_d entree cle = dechiffrer entree cle

(*
* CFB
*
* On ajoute un vecteur d'initialisation
* On chiffre le vecteur d'initialisation et on
* applique un ou exclusif sur l'entrée
*
* Le chiffrement et déchiffrement se fait avec la même fonction
*)
let cfb_e entree cle vi =
    let s = chiffrer vi cle in
    Array.map2 (lxor) entree s

let cfb_d = cfb_e
