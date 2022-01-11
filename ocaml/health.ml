(*
* Projet TIPE sur la cryptographie
* et le chiffrement
*
* Partie : AES - Health
* Chiffrement des données de santé
*
* Auteur : FALLET Nathan <contact@nathanfallet.me>
*)

(*
* On importe notre algorithme
*)

open Ciphers

(*
* Classe pour structurer les données des données de santé
*)

class healthData =
  object (self)
    (* Stockage des données *)
    val mutable data = (Array.make 0 0: int array)

    (* Lecture de l'image et extraction des chunks *)
    method read path =
      let ic = open_in path in
        try
          (*
          * On lit les données du fichier
          *)
          let fileLength = in_channel_length ic in
          data <- Array.init fileLength (fun x -> input_byte ic);

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
          * On écrit dans le fichier
          *)
          for k = 0 to (Array.length data) - 1 do
            output_byte oc data.(k)
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
      * On chiffre par bloc de 16, avec padding si besoin pour éviter les erreurs
      * Comme on a du texte, on utilise le caractère 32 comme padding afin de padder avec des espaces
      *)
      let length = Array.length data in
      let count = ((length - 1) / 16) + 1 in
      let crypted = Array.make (count * 16) 0 in
      for k = 0 to count - 1 do
        let cLength = min 16 (length - k*16) in
        let c = Array.init 16 (fun i -> if i < cLength then data.(k*16 + i) else 32) in
        let output = cipher#encrypt c in
        for p = 0 to 15 do
          crypted.(k*16 + p) <- output.(p)
        done
      done;

      data <- crypted

    method decrypt (cipher: cipher) =
      (* On dechiffre par bloc de 16, avec padding si besoin pour éviter les erreurs *)
      let length = Array.length data in
      let count = ((length - 1) / 16) + 1 in
      let crypted = Array.make (count * 16) 0 in
      for k = 0 to count - 1 do
        let cLength = min 16 (length - k*16) in
        let c = Array.init 16 (fun i -> if i < cLength then data.(k*16 + i) else 0) in
        let output = cipher#decrypt c in
        for p = 0 to 15 do
          crypted.(k*16 + p) <- output.(p)
        done
      done;

      data <- crypted
  end
