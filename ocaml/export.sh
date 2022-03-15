echo "(*** Export du code - $(date) ***)" > ../files/export.ml

echo "\n\n(*** polynomes.ml ***)\n\n" >> ../files/export.ml
cat polynomes.ml >> ../files/export.ml

echo "\n\n(*** matrices.ml ***)\n\n" >> ../files/export.ml
cat matrices.ml >> ../files/export.ml

echo "\n\n(*** aes.ml ***)\n\n" >> ../files/export.ml
cat aes.ml >> ../files/export.ml

echo "\n\n(*** ciphers.ml ***)\n\n" >> ../files/export.ml
cat ciphers.ml >> ../files/export.ml

echo "\n\n(*** huffman.ml ***)\n\n" >> ../files/export.ml
cat huffman.ml >> ../files/export.ml

echo "\n\n(*** deflate.ml ***)\n\n" >> ../files/export.ml
cat deflate.ml >> ../files/export.ml

echo "\n\n(*** images.ml ***)\n\n" >> ../files/export.ml
cat images.ml >> ../files/export.ml

echo "\n\n(*** health.ml ***)\n\n" >> ../files/export.ml
cat health.ml >> ../files/export.ml

echo "\n\n(*** demo.ml ***)\n\n" >> ../files/export.ml
cat demo.ml >> ../files/export.ml

echo "\n\n(*** Fin du code - $(date) ***)" >> ../files/export.ml
