(*
* Projet TIPE sur la cryptographie
* et le chiffrement
*
* Partie : AES - Polynômes
* Permet la multiplication de nombres
* dans GF(2^8)
*
* Auteurs :
* FALLET Nathan
* LAMIDEL Arthur
* MAKALOU Shérif
*)

(*
* Conversion d'un nombre en polynôme
* Un polynôme est représenté par un tableau
* de coefficents (0 ou 1 pour être dans GF(2))
*)
let polynome n =
    (* On fabrique un polynôme *)
    let p = Array.make 15 0 in
    for k = 0 to 14 do
        (* Coefficient de degré k *)
        p.(k) <- (n lsr k) land 1
    done;
    p

(*
* Conversion d'un polynôme en nombre
* On multiplie les coefficients par
* les puissances de 2
*)
let nombre p =
    (* On fait un nombre *)
    let n = ref 0 in
    for k = 0 to 14 do
        (* On multiplie le coefficient par la puissance de 2 *)
        n := !n + (p.(k) lsl k)
    done;
    !n

(*
* Somme de deux polynômes
*)
let somme p1 p2 =
    (* On fabrique un polynôme *)
    let p = Array.make 15 0 in
    for k = 0 to 14 do
        (* Somme des coefficients de degré k *)
        p.(k) <- (p1.(k) lxor p2.(k))
    done;
    p

(*
* Degré d'un polynôme
*)
let degre p =
    let l = Array.length p in
    let d = ref 0 in
    for k = 0 to l-1 do
        if p.(k) = 1 then d := k
    done;
    !d

(*
* Produit de deux polynômes
*)
let produit p1 p2 =
    (* On fabrique un polynôme *)
    let p = Array.make 15 0 in
    for k = 0 to 14 do
        (* Coefficient de degré k *)
        for i = 0 to k do
            (* La somme des coefficients de degré i et j avec i + j = k *)
            p.(k) <- p.(k) lxor (p1.(i) land p2.(k-i))
        done
    done;
    p

(*
* Reste de la division euclidienne de
* deux polynômes
*)
let rec reste dividende diviseur =
    (* Degrés des polynômes *)
    let d1 = degre dividende in
    let d2 = degre diviseur in
    (* On regarde lequel a le plus grand degré *)
    if d1 >= d2 then
        (*
        * Si c'est le dividende, on multiplie le diviseur par x à la
        * puissance la différence des dégrées, et on soustrait ce résultat
        * au dividende. Le reste de p1 par p2 est donc récursivement le reste
        * de la disivion de ce nouveau polynôme par p2.
        *)
        let quotient = polynome (1 lsl (d1-d2)) in
        let cequonsoustrait = produit diviseur quotient in
        let cequonredivise = somme dividende cequonsoustrait in
        reste cequonredivise diviseur
    else
        (* Sinon on ne peut pas diviser et alors le dividende est le reste *)
        dividende

(*
* Polynôme irréductible de GF(2^8)
*)
let irreductible = [|1; 1; 0; 1; 1; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0|]

(*
* Produit de deux nombres dans GF(2^8)
*)
let fois a b =
    let p1 = polynome a in
    let p2 = polynome b in
    let p = produit p1 p2 in
    let r = reste p irreductible in
    nombre r
