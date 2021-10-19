(*
* Projet TIPE sur la cryptographie
* et le chiffrement
*
* Partie : AES - Arbre de Huffman
* Utilisé dans la compression DEFLATE
*
* Auteur : FALLET Nathan <contact@nathanfallet.me>
*)

(*
* Déclaration du type de l'arbre binaire de Huffman
* Une feuille correspond à un code
* Un noeud a deux fils : celui de gauche pour le bit 0
* et celui de droite pour le bit 1
*)
type huffmanTree = Vide | Code of int | Node of huffmanTree * huffmanTree

(*
* Trouver un élément dans l'arbre en parcourant les bits
*)
let rec trouverCodeHuffman tree nextBit =
  match tree with
  (* Cas d'un noeud : on lit le bit suivant pour savoir où aller *)
  | Node(left, right) ->
    let bit = nextBit() in
    trouverCodeHuffman (if bit = 0 then left else right) nextBit

  (* Cas d'un code : on renvoit le code trouvé *)
  | Code(number) -> number

  (* Cas du vide : pas de code pour ce bit là *)
  | Vide -> failwith "Erreur dans le parcours de l'arbre de Huffman"

(*
* Insérer un code dans un arbre
*)
let rec insererCodeHuffman tree nextBit code =
  match tree with
  (* Cas d'un noeud : on lit le bit suivant pour savoir où aller *)
  | Node(left, right) ->
    let bit = nextBit() in
    if bit = 0 then
      Node(insererCodeHuffman left nextBit code, right)
    else
      Node(insererCodeHuffman right nextBit code, left)

  (* Cas du vide : on insert le code *)
  | Vide -> Code(code)

  (* Cas d'un code : emplacement déjà pris *)
  | Code(_) -> failwith "Erreur dans l'insertion dans l'arbre de Huffman"

(*
* Générer un arbre depuis l'association valeur vers taille de code
*)
let genererArbreHuffman valueToSize =
  (*
  * Fonction auxiliaire pour trier les codes par taille
  * basée sur le tri par fusion
  *)
  let rec trierCodesParTaille codes =
    let rec diviser liste =
      match liste with
      | [] -> [], []
      | h :: [] -> [h], []
      | h1 :: h2 :: t ->
        let l1, l2 = diviser t in
        (h1 :: l1, h2 :: l2) in
    let rec fusionner l1 l2 =
      match l1, l2 with
      | [], _ -> l2
      | _, [] -> l1
      | h1 :: t1, h2 :: t2 ->
        let x1, y1 = h1 in
        let x2, y2 = h2 in
        if y1 <= y2 then
          h1 :: (fusionner t1 l2)
        else
          h2 :: (fusionner l1 t2) in
    match codes with
    | [] -> []
    | h :: [] -> codes
    | _ ->
      let l1, l2 = diviser codes in
      fusionner (trierCodesParTaille l1) (trierCodesParTaille l2) in
  
  (*
  * Fonction auxiliaire pour générer un stream de bits
  * à partir d'un code et de sa taille
  *)
  let createStream code size =
    let rec aux code size stack =
      match size with
      | 0 ->
        (fun () ->
          match Stack.length stack with
          | 0 -> failwith "Stream vide"
          | _ -> Stack.pop stack
        )
      | _ -> Stack.push (code land 1) stack;
        aux (code lsr 1) (size - 1) stack in
    aux code size (Stack.create()) in
    
  (*
  * Fonction auxiliaire principale qui génère l'arbre
  * en parcourant les codes triés par taille
  *)
  let rec aux valueToSize lastCode lastCodeSize tree =
    match valueToSize with
    | [] -> tree
    | h :: t ->
      let value, currentCodeSize = h in
      if currentCodeSize = 0 then
        aux t lastCode lastCodeSize tree
      else
        let nextCode =
          if currentCodeSize > lastCodeSize then
            (lastCode + 1) lsl (currentCodeSize - lastCodeSize)
          else
            lastCode + 1
          in
        let newTree = insererCodeHuffman tree (createStream nextCode currentCodeSize) value in
        aux t nextCode currentCodeSize newTree in

  (* On lance le tri et on génère l'arbre *)
  aux (trierCodesParTaille valueToSize) (-1) 0 Vide
