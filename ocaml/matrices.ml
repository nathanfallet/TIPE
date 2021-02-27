(*
* Projet TIPE sur la cryptographie
* et le chiffrement
*
* Partie : AES - Matrices
* Permet la multiplication de matrices
* dans GF(2^8)
*
* Auteurs :
* FALLET Nathan
* LAMIDEL Arthur
* MAKALOU Shérif
*)

(*
* Import des polynômes
* Ce sont les éléments de nos matrices
*)

open Polynomes;;

(*
* La matrice utilisée pour faire le produit
* des colonnes (constante)
*)
let m = [|
    2;  3;  1;  1;
    1;  2;  3;  1;
    1;  1;  2;  3;
    3;  1;  1;  2
|];;

(*
* On défini le produit de deux matrices
* On va uniquement se concentrer sur le cas
* du produit pour le mixage des colonnes dont
* la taille des matrices est fixé
*)
let produit_colonne col =
    (* On fabrique la colonne de sortie *)
    let resultat = Array.make 4 0 in
    for i = 0 to 3 do
        (*
        * Chaque coefficient est la somme des
        * produits des éléments d'une ligne
        * avec ceux d'une colonne
        *)
        for k = 0 to 3 do
            resultat.(i) <- (fois m.(i*4 + k) col.(k)) lxor resultat.(i)
        done
    done;
    resultat;;
