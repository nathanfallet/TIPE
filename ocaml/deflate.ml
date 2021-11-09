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
* Lecteur de stream auxiliaire pour lire
* bit par bit les données
*)
class bitsReader data =
  object (self)
    (* Stockage des données à lire *)
    val mutable data = data
    val mutable head = 0
    val mutable non_read_bits = 0
    val mutable non_read_bits_size = 0

    (* Aligner la tête  de lecture *)
    method alignReader: (unit -> unit) = fun () ->
      non_read_bits <- 0;
      non_read_bits_size <- 0

    (* Lecture d'octet *)
    method readBytes count =
      self#alignReader();
      let bytes = Array.sub data head count in
      head <- head + count;
      bytes

    (* Lecture de bits *)
    method readBits count =
      while non_read_bits_size < count do
        let nextByte = self#readBytes 1 in
        non_read_bits <- non_read_bits lor (nextByte.(0) lsl non_read_bits_size);
        non_read_bits_size <- non_read_bits_size + 8
      done;
      let mask = (1 lsl count) - 1 in
      let bits = non_read_bits land mask in
      non_read_bits <- non_read_bits lsr count;
      non_read_bits_size <- non_read_bits_size - count;
      bits
  end

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
  let reader = new bitsReader entree in
  let cm = reader#readBits 4 in
  let _ = reader#readBits 4 in
  let _ = reader#readBits 5 in
  let fdict = reader#readBits 1 in
  let _ = reader#readBits 2 in

  (* On effectue quelques vérifications *)
  if cm <> 8 then failwith "Seulement le DEFLATE est supporté";
  if (entree.(0) lsl 8 lor entree.(1)) mod 31 <> 0 then failwith "FCHECK invalide";
  if fdict = 1 then failwith "FDICT non supporté";

  (* Lecture des données octet par octet *)
  let bytes = Queue.create() in
  let reading = ref true in
  while !reading do
    let leftBlocks = reader#readBits 1 in
    let blockType = reader#readBits 2 in
    match blockType with
    (* Contenu non compressé *)
    | 0 ->
      (* Lecture de la taille puis des données *)
      reader#alignReader();
      let lenght = reader#readBits 16 in
      assert (((reader#readBits 16) lxor 0xffff) = lenght);
      for _ = 0 to lenght - 1 do
        Queue.push (reader#readBits 8) bytes
      done
    
    | 1 | 2 ->
      (* Lecture des arbres *)
      let arbre_instructions = ref Vide in
      let arbre_distances = ref Vide in
      let arbre_tailles = ref Vide in

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
        let arbre_instructions_size = (reader#readBits 5) + 257 in
        let arbre_distances_size = (reader#readBits 5) + 1 in
        let arbre_tailles_size = (reader#readBits 4) + 4 in
        let ordered_values = [|16; 17; 18; 0; 8; 7; 9; 6; 10; 5; 11; 4; 12; 3; 13; 2; 14; 1; 15|] in
        let arbre_tailles_valueToSize = ref [] in
        for position = 0 to arbre_tailles_size - 1 do
          let truePosition = ordered_values.(position) in
          arbre_tailles_valueToSize := (truePosition, reader#readBits 3) :: !arbre_tailles_valueToSize
        done;
        arbre_tailles := genererArbreHuffman !arbre_tailles_valueToSize;
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
