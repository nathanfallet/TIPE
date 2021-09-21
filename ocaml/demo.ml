(*
* Projet TIPE sur la cryptographie
* et le chiffrement
*
* Partie : AES - Demo
* On fait un essaie de l'algorithme
*
* Auteur : FALLET Nathan <contact@nathanfallet.me>
*)

(*
* On importe nos algorithmes
*)

open Ciphers
open Images

(*
* On fait notre démonstration
*
* Vérifie les données de test :
* National Institute of Standards and Technology Special Publication 800-38A 2001 ED
* https://nvlpubs.nist.gov/nistpubs/Legacy/SP/nistspecialpublication800-38a.pdf
*)

(* Données communes à tous les tests *)
let cle = [|0x2b; 0x7e; 0x15; 0x16; 0x28; 0xae; 0xd2; 0xa6; 0xab; 0xf7; 0x15; 0x88; 0x09; 0xcf; 0x4f; 0x3c|]
let entree = [|0x6b; 0xc1; 0xbe; 0xe2; 0x2e; 0x40; 0x9f; 0x96; 0xe9; 0x3d; 0x7e; 0x11; 0x73; 0x93; 0x17; 0x2a|]
let vi = [|0x00; 0x01; 0x02; 0x03; 0x04; 0x05; 0x06; 0x07; 0x08; 0x09; 0x0a; 0x0b; 0x0c; 0x0d; 0x0e; 0x0f|]

(* Déclaration des tests (nom, cipher, check) *)
let tests = [
    (* Example Vectors for Modes of Operation of the AES - ECB-AES123 Example Vectors - Block #1 *)
    ("ECB", new ecb cle, [|0x3a; 0xd7; 0x7b; 0xb4; 0x0d; 0x7a; 0x36; 0x60; 0xa8; 0x9e; 0xca; 0xf3; 0x24; 0x66; 0xef; 0x97|]);

    (* Example Vectors for Modes of Operation of the AES - CBC-AES128 Example Vectors - Block #1 *)
    ("CBC", new cbc cle vi, [|0x76; 0x49; 0xab; 0xac; 0x81; 0x19; 0xb2; 0x46; 0xce; 0xe9; 0x8e; 0x9b; 0x12; 0xe9; 0x19; 0x7d|]);

    (* Example Vectors for Modes of Operation of the AES - OFB-AES128 Example Vectors - Block #1 *)
    ("OFB", new ofb cle vi, [|0x3b; 0x3f; 0xd9; 0x2e; 0xb7; 0x2d; 0xad; 0x20; 0x33; 0x34; 0x49; 0xf8; 0xe8; 0x3c; 0xfb; 0x4a|]);

    (* Example Vectors for Modes of Operation of the AES - CFB128-AES128 Example Vectors - Block #1 *)
    ("CFB", new cfb cle vi, [|0x3b; 0x3f; 0xd9; 0x2e; 0xb7; 0x2d; 0xad; 0x20; 0x33; 0x34; 0x49; 0xf8; 0xe8; 0x3c; 0xfb; 0x4a|])
]

(* Execution des tests *)
let rec faire_les_tests tests =
    match tests with
    (* On a fini les tests *)
    | [] -> print_endline "Fin des tests avec succès !"

    (* On effectue le test en tête de liste *)
    | h :: t ->
        (* Récupère ses données *)
        let nom, cipher, check = h in

        (* On chiffre et on déchiffre *)
        let chiffre = cipher#encrypt entree in
        let dechiffre = cipher#decrypt chiffre in

        (* On vérifie que ça correspond à ce qui est attendu *)
        if chiffre = check && dechiffre = entree then
            print_endline (nom ^ " : Test réussi")
        else
            failwith (nom ^ " : Echec du test");

        (* On test également le chiffrement d'images *)
        let o = new img in
        o#read "../images/moi.png";
        o#crypt cipher;
        o#write ("../images/moi_" ^ nom ^ ".png");
        
        (* On passe aux tests suivants *)
        faire_les_tests t

(* Test de performance *)
let test_performance repeat =
    let start = Sys.time() in
    let c = new ecb cle in
    for k = 0 to repeat do
        ignore (c#encrypt entree)
    done;
    let stop = Sys.time() in
    print_string "Test de performance : ";
    print_float (stop -. start);
    print_string "s pour ";
    print_int repeat;
    print_endline " blocs"

(* On lance les tests *)
let _ = faire_les_tests tests
let _ = test_performance 100_000
