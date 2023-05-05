echo "Building INEX..."

echo "Building bytecode machine..."
cd ./machine
source ./compile.sh
cd ..
mv -f ./machine/inex.exe .

echo "Building compiler..."
cd ./compiler
dune build
cd ..
if [ -e inexc.exe ] 
then rm -f ./inexc.exe
fi
mv -f ./compiler/_build/default/src/inexc.exe .

echo "Done"