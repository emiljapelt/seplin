echo "Building INEX..."

echo "Building bytecode machine..."
cd ./machine
source ./compile.sh
cd ..
mv ./machine/inex.exe .

echo "Building compiler..."
cd ./compiler
dune build
cd ..
mv ./compiler/_build/default/src/inexc.exe .

echo "Done"