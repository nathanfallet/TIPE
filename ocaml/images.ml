(*
* Projet TIPE sur la cryptographie
* et le chiffrement
*
* Partie : AES - Image
* Chiffrement d'une image, tout en conservant la structure du fichier
*
* Auteur : FALLET Nathan <contact@nathanfallet.me>
*)

(*
* On importe nos algorithmes
*)

open Ciphers
open Deflate

(*
* Le header des fichiers PNG
* 137 80 78 71 13 10 26 10
*)
let header = [|137; 80; 78; 71; 13; 10; 26; 10|]

(*
* Fonction qui converti 4 chars en string et vice versa
*)
let chars_to_string char1 char2 char3 char4 =
  (String.make 1 char1) ^ (String.make 1 char2) ^ (String.make 1 char3) ^ (String.make 1 char4)
let string_to_chars str =
  (str.[0], str.[1], str.[2], str.[3])

(*
* Définition d'un chunk d'une image
*)
class chunk data chunkType =
  object (self)
    (* Stockage des données des chunks *)
    val mutable data = data
    val mutable chunkType = chunkType

    (* Getters *)
    method getData: int array = data
    method getChunkType: string = chunkType

    (* Calcul du CRC du chunk *)
    method crc =
      (* Initialisation de la table de CRC *)
      let table = Array.make 256 0 in
      for n = 0 to 255 do
        let c = ref n in
        for k = 0 to 7 do
          if !c land 1 = 1 then
            c := 0xedb88320 lxor (!c lsr 1)
          else
            c := !c lsr 1
        done;
        table.(n) <- !c
      done;

      (*
      * Ensuite on calcul selon les données du chunk
      * On commence par traiter le type du chunk, puis ses données
      *)
      let crc = ref 0xffffffff in
      String.iter (fun x -> crc := table.((!crc lxor (Char.code x)) land 0xff) lxor (!crc lsr 8)) chunkType;
      for n = 0 to (Array.length data) - 1 do
        crc := table.((!crc lxor data.(n)) land 0xff) lxor (!crc lsr 8)
      done;
      !crc lxor 0xffffffff
  end

(*
* Classe pour structurer les données d'une image
*)

class img =
  object (self)
    (* Stockage des chunks *)
    val mutable chunks = (Queue.create(): chunk Queue.t)

    (* Lecture de l'image et extraction des chunks *)
    method read path =
      let ic = open_in path in
        try
          (*
          * On vérifie le header des fichiers PNG
          * On en profite pour lancer une erreur si le fichier est malformé
          *)
          for k = 0 to 7 do
            let b = input_byte ic in
            if header.(k) <> b then
              failwith "Le fichier n'est pas un fichier PNG"
          done;
          
          (*
          * On décode des chunks tant qu'il y en a
          *)
          Queue.clear chunks;
          let fileLength = in_channel_length ic in
          while (pos_in ic) < fileLength do
            let length = input_binary_int ic in
            let ctype1 = input_char ic in
            let ctype2 = input_char ic in
            let ctype3 = input_char ic in
            let ctype4 = input_char ic in
            let data = Array.init length (fun x -> input_byte ic) in
            let _ = input_binary_int ic in
            Queue.push (new chunk data (chars_to_string ctype1 ctype2 ctype3 ctype4)) chunks
          done;

          (*
          * Fermeture du flux
          *)
          close_in ic
        with e ->
          close_in_noerr ic;
          raise e
    
    (* Ecriture d'une image *)
    method write path =
      let oc = open_out path in
        try
          (*
          * On écrit le header des fichiers PNG
          *)
          for k = 0 to 7 do
            output_byte oc header.(k)
          done;

          (*
          * On parcours les chunks pour les écrire dans le fichier
          *)
          while not (Queue.is_empty chunks) do
            let c = Queue.pop chunks in
            let ctype1, ctype2, ctype3, ctype4 = string_to_chars c#getChunkType in
            let data = c#getData in
            let crc = c#crc in
            output_binary_int oc (Array.length data);
            output_char oc ctype1;
            output_char oc ctype2;
            output_char oc ctype3;
            output_char oc ctype4;
            for k = 0 to (Array.length data) - 1 do
              output_byte oc data.(k)
            done;
            output_binary_int oc crc
          done;

          (*
          * Fermeture du flux
          *)
          close_out oc
        with e ->
          close_out_noerr oc;
          raise e

    method crypt (cipher: cipher) =
      (*
      * On parcours les chunks pour les chiffrer
      *
      * On isole les chunks de données IDAT pour récupérer le stream complet,
      * le décompresser et le chiffrer, pour ensuite recréer des chunks avec
      * les nouvelles données chiffrées.
      *)
      let newChunks = (Queue.create(): chunk Queue.t) in
      let isolatedIDAT = (Queue.create(): chunk Queue.t) in
      let isolatedIDATLength = ref 0 in
      while not (Queue.is_empty chunks) do
        (* On récupère un chunk *)
        let c = Queue.pop chunks in

        (* Si on a un IDAT, on l'isole pour le traiter *)
        if c#getChunkType = "IDAT" then begin
          isolatedIDATLength := !isolatedIDATLength + (Array.length c#getData);
          Queue.push c isolatedIDAT
        end

        (* Une fois qu'on a fini avec les IDAT, on traite le tout *)
        else if not (Queue.is_empty isolatedIDAT) then begin
          (* D'abord on récupère le steam de données *)
          let data = Array.make !isolatedIDATLength 0 in
          let cursor = ref 0 in
          while not (Queue.is_empty isolatedIDAT) do
            let cIDAT = Queue.pop isolatedIDAT in
            let cIDATData = cIDAT#getData in
            for k = 0 to (Array.length cIDATData) - 1 do
              data.(!cursor) <- cIDATData.(k);
              cursor := !cursor + 1
            done;
          done;

          (* On décompresse le stream *)
          let decompressed = decompresser data in

          (* On chiffre par bloc de 16, avec padding si besoin pour éviter les erreurs *)
          let length = Array.length decompressed in
          let count = ((length - 1) / 16) + 1 in
          let crypted = Array.make length 0 in
          for k = 0 to count - 1 do
            let cLength = min 16 (length - k*16) in
            let c = Array.init 16 (fun i -> if i < cLength then decompressed.(k*16 + i) else 0) in
            let output = cipher#encrypt c in
            for p = 0 to cLength - 1 do
              crypted.(k*16 + p) <- output.(p)
            done
          done;

          (* On force une filter method, sinon l'image n'est pas lisible *)
          crypted.(0) <- 0;

          (* On recompresse le stream chiffré *)
          let compressed = compresser crypted in

          (* Enfin, on le remet dans des chunks *)
          let length = Array.length compressed in
          let count = ((length - 1) / 16384) + 1 in
          for k = 0 to count-1 do
            let cstart = k*16384 in
            let cdata = Array.sub compressed cstart (min 16384 (length-cstart)) in
            Queue.push (new chunk cdata "IDAT") newChunks
          done
        end;

        (* On met les autres chunks dans la liste sans les toucher *)
        if c#getChunkType <> "IDAT" then
          Queue.push c newChunks
      done;
      chunks <- newChunks
  end
