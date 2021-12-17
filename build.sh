cabal build exe:bark
EXE_PATH=$(cabal exec which bark)
mkdir -p dist 
mv $EXE_PATH ./dist/