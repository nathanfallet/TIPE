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
        let nextByte = data.(head) in
        head <- head + 1;
        non_read_bits <- non_read_bits lor (nextByte lsl non_read_bits_size);
        non_read_bits_size <- non_read_bits_size + 8
      done;
      let mask = (1 lsl count) - 1 in
      let bits = non_read_bits land mask in
      non_read_bits <- non_read_bits lsr count;
      non_read_bits_size <- non_read_bits_size - count;
      bits

    (* Création d'un stream *)
    method stream =
      fun () -> self#readBits 1
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
* Génération de n tailles de code
*)
let getNValueToSize n arbre_tailles reader =
  let rec repeat element n =
    match n with
    | 0 -> []
    | _ -> element :: (repeat element (n-1)) in
  let rec enumerate l counter =
    match l with
    | h :: t -> (counter, h) :: (enumerate t (counter + 1))
    | [] -> [] in
  let rec aux valueToSize n =
    match n with
    | n when n <= 0 -> enumerate (List.rev valueToSize) 0
    | _ ->
      let code_de_taille = trouverCodeHuffman arbre_tailles reader#stream in
      (
        match code_de_taille with
        | 16 ->
          let lastValue = List.hd valueToSize in
          let times = 3 + (reader#readBits 2) in
          (aux ((repeat lastValue times) @ valueToSize) (n-times))
        | 17 ->
          let times = 3 + (reader#readBits 3) in
          (aux ((repeat 0 times) @ valueToSize) (n-times))
        | 18 ->
          let times = 11 + (reader#readBits 7) in
          (aux ((repeat 0 times) @ valueToSize) (n-times))
        | _ ->
          (aux (code_de_taille :: valueToSize) (n-1))
      )
  in aux [] n

(*
* Répéter une portion de queue via la distance et la taille
*)
let repeatInQueue queue distance taille_repetition =
  let result = Array.make taille_repetition 0 in
  let copied = Queue.copy queue in
  let length = Queue.length queue in
  let start = length - distance in
  for k = 0 to start - 1 do
    ignore(Queue.pop copied)
  done;
  for k = 0 to taille_repetition - 1 do
    let element =
      if Queue.is_empty copied then
        result.(k mod (taille_repetition - distance))
      else
        Queue.pop copied
    in
    result.(k) <- element;
    Queue.push result.(k) queue
  done

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
  if fdict = 1 then ignore(reader#readBits 32);

  (* Lecture des données octet par octet *)
  let bytes = Queue.create() in
  let reading = ref true in
  while !reading do
    let leftBlocks = reader#readBits 1 in
    let blockType = reader#readBits 2 in
    (
      match blockType with
      (* Contenu non compressé *)
      | 0 ->
        (* Lecture de la taille puis des données *)
        reader#alignReader();
        let length = reader#readBits 16 in
        assert (((reader#readBits 16) lxor 0xffff) = length);
        for _ = 0 to length - 1 do
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
          for value = 280 to 287 do
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
          (* Lecture des tailles *)
          let arbre_instructions_size = (reader#readBits 5) + 257 in
          let arbre_distances_size = (reader#readBits 5) + 1 in
          let arbre_tailles_size = (reader#readBits 4) + 4 in

          (* Arbre des tailles *)
          let ordered_values = [|16; 17; 18; 0; 8; 7; 9; 6; 10; 5; 11; 4; 12; 3; 13; 2; 14; 1; 15|] in
          let arbre_tailles_valueToSize = ref [] in
          for position = 0 to arbre_tailles_size - 1 do
            let truePosition = ordered_values.(position) in
            arbre_tailles_valueToSize := (truePosition, reader#readBits 3) :: !arbre_tailles_valueToSize
          done;
          arbre_tailles := genererArbreHuffman !arbre_tailles_valueToSize;

          (* Arbre des instructions *)
          arbre_instructions := genererArbreHuffman (getNValueToSize arbre_instructions_size !arbre_tailles reader);

          (* Arbre des distances *)
          arbre_distances := genererArbreHuffman (getNValueToSize arbre_distances_size !arbre_tailles reader)
        end;

        (* Décompression des données *)
        let decoding = ref true in
        while !decoding do
          let code = trouverCodeHuffman !arbre_instructions reader#stream in
          match code with
          (* Cas d'arrêt et de lecture simple *)
          | 256 -> decoding := false
          | code when code < 256 -> Queue.push code bytes

          (* Cas de répétition *)
          | _ ->
            let taille_repetition = ref 0 in
            let distance = ref 0 in
            (
              if code < 265 then
                taille_repetition := 3 + (code - 257)
              else if code < 285 then
                let extra_bits = 1 + ((code - 265) lsr 2) in
                taille_repetition := 3 + ((4 lor ((code - 265) land 3)) lsl extra_bits) + (reader#readBits extra_bits)
            );
            let code_distance = trouverCodeHuffman !arbre_distances reader#stream in
            (
              if code_distance < 4 then
                distance := 1 + code_distance
              else
                let extra_bits = 1 + ((code_distance - 4) lsr 1) in
                distance := 1 + ((2 lor ((code_distance - 2) land 1)) lsl extra_bits) + (reader#readBits extra_bits)
            );
            repeatInQueue bytes !distance !taille_repetition
        done

      (* Type de bloc non supporté *)
      | _ -> failwith "Type de bloc de compression non supporté"
    );

    (* On regarde si on a fini *)
    if leftBlocks = 1 then begin
      reader#alignReader();
      reading := false
    end
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
