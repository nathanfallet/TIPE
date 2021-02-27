(*
* Projet TIPE sur la cryptographie
* et le chiffrement
*
* Partie : AES - Demo
* On fait un essaie de l'algorithme
*
* Auteurs :
* FALLET Nathan
* LAMIDEL Arthur
* MAKALOU Shérif
*)

(*
* On importe notre algorithme
*)

open Aes;;

(*
* Fonction utile pour la démonstration
* On affiche un array en héxadécimal
*)
let print_array array =
    for k = 0 to 15 do
        print_string (Printf.sprintf "%02x" array.(k));
    done;
    print_newline();;

(*
* On fait notre démonstration
*
* Clé    : 2b7e151628aed2a6abf7158809cf4f3c
* Entrée : 6bc1bee22e409f96e93d7e117393172a
* Sortie : 3ad77bb40d7a3660a89ecaf32466ef97
*
* Vérifie les données de test :
* National Institute of Standards and Technology Special Publication 800-38A 2001 ED
* https://nvlpubs.nist.gov/nistpubs/Legacy/SP/nistspecialpublication800-38a.pdf
* Example Vectors for Modes of Operation of the AES - ECB Example Vectors - Block #1
*)
let cle = [|0x2b; 0x7e; 0x15; 0x16; 0x28; 0xae; 0xd2; 0xa6; 0xab; 0xf7; 0x15; 0x88; 0x09; 0xcf; 0x4f; 0x3c|] in
let entree = [|0x6b; 0xc1; 0xbe; 0xe2; 0x2e; 0x40; 0x9f; 0x96; 0xe9; 0x3d; 0x7e; 0x11; 0x73; 0x93; 0x17; 0x2a|] in
let sortie = chiffrer entree cle in

print_string "Clé    : ";
print_array cle;
print_string "Entrée : ";
print_array entree;
print_string "Sortie : ";
print_array sortie;;
