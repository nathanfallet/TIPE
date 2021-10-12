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
