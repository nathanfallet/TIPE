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
    let rec polynome_rec n p =
        match n with
        (* 0 : cas d'arret *)
        | 0 -> List.rev p
        (* Nombre impair : on ajoute un coef 1 au polynôme *)
        | n when n land 1 = 1 -> polynome_rec (n lsr 1) (1 :: p)
        (* Nombre pair : on ajoute un coef 0 au polynôme *)
        | _ -> polynome_rec (n lsr 1) (0 :: p)
    in polynome_rec n []

(*
* Conversion d'un polynôme en nombre
* On multiplie les coefficients par
* les puissances de 2
*)
let nombre p =
    let rec nombre_rec p n c =
        match p with
        (* Polynôme nul : cas d'arret *)
        | [] -> n
        (* On multiplie le coef par la bonne puissance de 2 *)
        | h :: t -> nombre_rec t (n + h lsl c) (c + 1)
    in nombre_rec p 0 0

(*
* Simplification d'un polynôme
* On enlève les zeros en trop à la fin
*)
let simplifie p =
	let rec simplifie_rec p =
		match p with
        (* Un zero à simplifier *)
		| h :: t when h = 0 -> simplifie_rec t
        (* Pas de zero, c'est bon *)
		| _ -> List.rev p
	in simplifie_rec (List.rev p)

(*
* Somme de deux polynômes
*)
let somme p1 p2 =
	let rec somme_rec l p1 p2 =
		match p1, p2 with
        (* On a fini avec les deux : cas d'arret *)
		| [], [] -> List.rev l
        (* Coef seul de p1 *)
		| h :: t, [] -> somme_rec (h :: l) t p2
        (* Si on a fini avec p1 mais pas p2, on les échange (retour cas précédent) *)
		| [], p2 -> somme_rec l p2 p1
        (* On ajoute (avec un xor) les deux coefficients *)
		| h1 :: t1, h2 :: t2 -> somme_rec ((h1 lxor h2) :: l) t1 t2
	in simplifie (somme_rec [] p1 p2)

(*
* Degré d'un polynôme
*)
let degre p = (List.length p) - 1

(*
* Multiplier un polynôme par x^n
*)
let rec produit_xn p n =
	match p with
    (* Polynôme vide *)
	| [] -> []
    (* On multiplie par x^0 = 1 : cas d'arrêt *)
	| l when n = 0 -> p
    (* On ajoute un coef pour augmenter le degré *)
	| l -> produit_xn (0 :: l) (n-1)

(*
* Produit de deux polynômes
*)
let produit p1 p2 =
	let rec produit_rec p1 p2 r n =
		match p1 with
        (* On a fini de multiplier : cas d'arret *)
		| [] -> r
        (* On multiplie par un coef 1 *)
		| h :: t when h = 1 -> produit_rec t p2 (somme r (produit_xn p2 n)) (n+1)
        (* On multiplie par un coef 0 *)
        | h :: t -> produit_rec t p2 r (n+1)
	in produit_rec p1 p2 [] 0

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
let irreductible = [1; 1; 0; 1; 1; 0; 0; 0; 1]

(*
* Produit de deux nombres dans GF(2^8)
*)
let ( ** ) a b =
    let p1 = polynome a in
    let p2 = polynome b in
    let p = produit p1 p2 in
    let r = reste p irreductible in
    nombre r
