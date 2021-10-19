(*
* Projet TIPE sur la cryptographie
* et le chiffrement
*
* Partie : AES - Compression DEFLATE
* Permet de compresser et décompresser des données
*
* Auteur : FALLET Nathan <contact@nathanfallet.me>
*)

open Huffman

(*
* Checksum pour vérifier l'intégriter des données
*)
let adler32 entree =
  let somme1 = ref 1 in
  let somme2 = ref 0 in
  for k = 0 to (Array.length entree) - 1 do
    somme1 := !somme1 + entree.(k);
    somme2 := !somme2 + !somme1
  done;
  ((!somme2 mod 65521) lsl 16) lor (!somme1 mod 65521)

(*
* Permet de décompresser un flux de données
*)
let decompresser entree =
  (*
  * Extraction du header des données compressées
  *)
  let cm = entree.(0) land 15 in
  let fdict = entree.(1) lsr 5 land 1 = 1 in

  (* On effectue quelques vérifications *)
  if cm <> 8 then failwith "Seulement le DEFLATE est supporté";
  if (entree.(0) lsl 8 lor entree.(1)) mod 31 <> 0 then failwith "FCHECK invalide";
  if fdict then failwith "FDICT non supporté";

  (* Lecture des données octet par octet *)
  let bytes = Queue.create() in
  let cursor = ref 2 in
  let reading = ref true in
  while !reading do
    let firstByte = entree.(!cursor) in
    let blockType = firstByte lsr 1 land 3 in
    match blockType with
    (* Contenu non compressé *)
    | 0 ->
      (* Lecture de la taille puis des données *)
      let lenght = entree.(!cursor + 1) lsl 8 lor entree.(!cursor + 2) in
      for k = 0 to lenght - 1 do
        Queue.push entree.(!cursor + 2 + k) bytes
      done;
      cursor := !cursor + lenght + 2
    
    | 1 | 2 ->
      (* Lecture des arbres *)
      let arbre_instructions = ref Vide in
      let arbre_distances = ref Vide in

      (* Arbre prédéfini *)
      if blockType = 1 then begin
        (* Arbre des instructions *)
        let valueToSize = ref [] in
        for value = 256 to 279 do
          valueToSize := (value, 7) :: !valueToSize
        done;
        for value = 0 to 143 do
          valueToSize := (value, 8) :: !valueToSize
        done;
        for value = 280 to 187 do
          valueToSize := (value, 8) :: !valueToSize
        done;
        for value = 144 to 255 do
          valueToSize := (value, 9) :: !valueToSize
        done;
        arbre_instructions := genererArbreHuffman !valueToSize;

        (* Arbre des distances *)
        let distances = ref [] in
        for position = 0 to 31 do
          distances := (position, 5) :: !distances
        done;
        arbre_distances := genererArbreHuffman !distances
      end
    
      (* Arbre à lire dans le flux *)
      else begin
        failwith "Lecture dans le flux pas encore implémentée"
      end;

      (* Décompression des données *)
      ()

    (* Type de bloc non supporté *)
    | _ -> failwith "Type de bloc de compression non supporté"
  done;

  (* Extraction des données dans un tableau *)
  let length = Queue.length bytes in
  let data = Array.make length 0 in
  for k = 0 to length - 1 do
    data.(k) <- Queue.pop bytes
  done;
  data

(*
* Permet de compresser un flux de données
*)
let compresser entree =
  (*
  * Dans notre cas, on ne va pas s'embêter, et on va enregistrer
  * les données tel quel en ajoutant simplement le header et
  * le footer du DEFLATE
  *)
  let length = Array.length entree in
  let alder = adler32 entree in
  let header = [|0x08; 0x1E; length lsr 8; length land 0xFF|] in
  let footer = [|alder lsr 24; (alder lsr 16) land 0xFF; (alder lsr 8) land 0xFF; alder land 0xFF|] in
  Array.append (Array.append header entree) footer
