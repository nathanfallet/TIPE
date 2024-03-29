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
open Health

(*
* On fait notre démonstration
*
* Vérifie les données de test :
* National Institute of Standards and Technology Special Publication 800-38A 2001 ED
* https://nvlpubs.nist.gov/nistpubs/Legacy/SP/nistspecialpublication800-38a.pdf
*)

(* Données communes à tous les tests *)
let cle     = [|0x2b; 0x7e; 0x15; 0x16; 0x28; 0xae; 0xd2; 0xa6; 0xab; 0xf7; 0x15; 0x88; 0x09; 0xcf; 0x4f; 0x3c|]
let entree  = [|0x6b; 0xc1; 0xbe; 0xe2; 0x2e; 0x40; 0x9f; 0x96; 0xe9; 0x3d; 0x7e; 0x11; 0x73; 0x93; 0x17; 0x2a|]
let entree2 = [|0xae; 0x2d; 0x8a; 0x57; 0x1e; 0x03; 0xac; 0x9c; 0x9e; 0xb7; 0x6f; 0xac; 0x45; 0xaf; 0x8e; 0x51|]
let vi      = [|0x00; 0x01; 0x02; 0x03; 0x04; 0x05; 0x06; 0x07; 0x08; 0x09; 0x0a; 0x0b; 0x0c; 0x0d; 0x0e; 0x0f|]

(* Déclaration des tests (nom, cipher, check) *)
let tests = [
    (* Example Vectors for Modes of Operation of the AES - ECB-AES128 Example Vectors - Block #1 *)
    (
        "ECB", new ecb cle, new ecb cle,
        [|0x3a; 0xd7; 0x7b; 0xb4; 0x0d; 0x7a; 0x36; 0x60; 0xa8; 0x9e; 0xca; 0xf3; 0x24; 0x66; 0xef; 0x97|],
        [|0xf5; 0xd3; 0xd5; 0x85; 0x03; 0xb9; 0x69; 0x9d; 0xe7; 0x85; 0x89; 0x5a; 0x96; 0xfd; 0xba; 0xaf|]
    );

    (* Example Vectors for Modes of Operation of the AES - CBC-AES128 Example Vectors - Block #1 *)
    (
        "CBC", new cbc cle vi, new cbc cle vi,
        [|0x76; 0x49; 0xab; 0xac; 0x81; 0x19; 0xb2; 0x46; 0xce; 0xe9; 0x8e; 0x9b; 0x12; 0xe9; 0x19; 0x7d|],
        [|0x50; 0x86; 0xcb; 0x9b; 0x50; 0x72; 0x19; 0xee; 0x95; 0xdb; 0x11; 0x3a; 0x91; 0x76; 0x78; 0xb2|]
    );

    (* Example Vectors for Modes of Operation of the AES - OFB-AES128 Example Vectors - Block #1 *)
    (
        "OFB", new ofb cle vi, new ofb cle vi,
        [|0x3b; 0x3f; 0xd9; 0x2e; 0xb7; 0x2d; 0xad; 0x20; 0x33; 0x34; 0x49; 0xf8; 0xe8; 0x3c; 0xfb; 0x4a|],
        [|0x77; 0x89; 0x50; 0x8d; 0x16; 0x91; 0x8f; 0x03; 0xf5; 0x3c; 0x52; 0xda; 0xc5; 0x4e; 0xd8; 0x25|]
    );

    (* Example Vectors for Modes of Operation of the AES - CFB128-AES128 Example Vectors - Block #1 *)
    (
        "CFB", new cfb cle vi, new cfb cle vi,
        [|0x3b; 0x3f; 0xd9; 0x2e; 0xb7; 0x2d; 0xad; 0x20; 0x33; 0x34; 0x49; 0xf8; 0xe8; 0x3c; 0xfb; 0x4a|],
        [|0xc8; 0xa6; 0x45; 0x37; 0xa0; 0xb3; 0xa9; 0x3f; 0xcd; 0xe3; 0xcd; 0xad; 0x9f; 0x1c; 0xe5; 0x8b|]
    )
]

(* Execution des tests *)
let rec faire_les_tests tests =
    match tests with
    (* On a fini les tests *)
    | [] -> print_endline "Fin des tests avec succès !"

    (* On effectue le test en tête de liste *)
    | h :: t ->
        (* Récupère ses données *)
        let nom, cipher_encrypt, cipher_decrypt, check, check2 = h in

        (* On chiffre et on déchiffre *)
        let chiffre    = cipher_encrypt#encrypt entree in
        let chiffre2   = cipher_encrypt#encrypt entree2 in
        let dechiffre  = cipher_decrypt#decrypt chiffre in
        let dechiffre2 = cipher_decrypt#decrypt chiffre2 in

        (* On vérifie que ça correspond à ce qui est attendu *)
        if chiffre = check && chiffre2 = check2 && dechiffre = entree && dechiffre2 = entree2 then
            print_endline (nom ^ " : Test réussi")
        else
            failwith (nom ^ " : Echec du test");

        (* On effectue le chiffrement des données de santé *)
        let o = new healthData in
        o#read "./files/health.json";
        o#crypt cipher_encrypt;
        o#write ("./files/health_" ^ nom ^ ".bin");
        print_endline (nom ^ " : Données de santé chiffrées");

        (* Que l'on déchiffre pour vérifier *)
        let o = new healthData in
        o#read ("./files/health_" ^ nom ^ ".bin");
        o#decrypt cipher_decrypt;
        o#write ("./files/health_" ^ nom ^ "_decrypted.json");
        print_endline (nom ^ " : Données de santé déchiffrées");

        (* On test également le chiffrement d'images *)
        let o = new img in
        o#read "./files/moi.png";
        o#crypt cipher_encrypt;
        o#write ("./files/moi_" ^ nom ^ ".png");
        print_endline (nom ^ " : Image chiffrée");
        
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
