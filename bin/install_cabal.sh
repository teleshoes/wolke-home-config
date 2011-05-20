
echo fetching some required packages
sudo apt-get install ghc6 zlib1g-dev zlib1g zlibc libncurses5-dev

rm -rf ~/build_cabal_tmp
mkdir -p ~/build_cabal_tmp
cd ~/build_cabal_tmp

wget http://hackage.haskell.org/packages/archive/Cabal/1.6.0.3/Cabal-1.6.0.3.tar.gz
wget http://hackage.haskell.org/packages/archive/mtl/1.1.0.2/mtl-1.1.0.2.tar.gz
wget http://hackage.haskell.org/packages/archive/parsec/3.0.1/parsec-3.0.1.tar.gz
wget http://hackage.haskell.org/packages/archive/network/2.2.1.5/network-2.2.1.5.tar.gz
wget http://hackage.haskell.org/packages/archive/HTTP/4000.0.8/HTTP-4000.0.8.tar.gz
wget http://hackage.haskell.org/packages/archive/zlib/0.5.2.0/zlib-0.5.2.0.tar.gz
wget http://hackage.haskell.org/packages/archive/cabal-install/0.6.2/cabal-install-0.6.2.tar.gz

for i in *.tar.gz; do tar xvf $i; done

rm *.tar.gz

for pkg in Cabal* mtl* parsec* network* HTTP* zlib* cabal-install*; do
cd ~/build_cabal_tmp/$pkg;
echo building $pkg;
ghc --make Setup;
./Setup configure --user;
./Setup build;
./Setup install;
cd ..;
done

rm -rf ~/build_cabal_tmp

echo making cabal executable and placing a link in /usr/bin
chmod +x ~/.cabal/bin/cabal
sudo ln -s ~/.cabal/bin/cabal /usr/bin/cabal

