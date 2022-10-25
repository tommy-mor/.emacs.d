#https://www.emacswiki.org/emacs/GccEmacs

sudo apt install autoconf make gcc texinfo libgtk-3-dev libxpm-dev libjpeg-dev libgif-dev libtiff5-dev libgnutls28-dev libncurses5-dev libgccjit-9-dev

git clone git://git.savannah.gnu.org/emacs.git
cd emacs
./autogen.sh
./configure --with-native-compilation
make -j$(nproc)
sudo make install
