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

(*
* Le header des fichiers PNG
* 137 80 78 71 13 10 26 10
*)
let header = [|137; 80; 78; 71; 13; 10; 26; 10|]

(*
* Alias pour la définition d'un chunk
*)
type chunk = int * char * char * char * char * int array * int

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
            let crc = input_binary_int ic in
            Queue.push (length, ctype1, ctype2, ctype3, ctype4, data, crc) chunks
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
            let length, ctype1, ctype2, ctype3, ctype4, data, crc = c in
            output_binary_int oc length;
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
  end

(*
* Application
*)

let _ =
  let o = new img in
  o#read "/Users/nathanfallet/Desktop/moi.png";
  o#write "/Users/nathanfallet/Desktop/output.png"
