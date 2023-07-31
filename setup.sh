#!/bin/bash

# Update package lists
sudo apt update

# Install software
sudo apt install -y vim vim-gtk3 bspwm sxhkd polybar alacritty wget curl picom git nodejs npm gnome-tweaks make gcc xdotool gawk ripgrep fzf

# Install ULauncher
wget https://github.com/Ulauncher/Ulauncher/releases/download/5.15.3/ulauncher_5.15.3_all.deb
sudo apt install -y ./ulauncher_5.15.3_all.deb
sudo rm -r ./ulauncher_5.15.3_all.deb

# Vim Plugin Setup
curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

# Install tdrop
git clone https://github.com/noctuid/tdrop
cd tdrop
sudo make install
cd ..
sudo rm -r tdrop

# Download and install Iosevka Nerd Font
wget https://github.com/ryanoasis/nerd-fonts/releases/download/v3.0.2/Iosevka.zip
mkdir -p /usr/local/share/fonts
sudo unzip Iosevka.zip -d /usr/local/share/fonts/
sudo rm -r Iosevka.zip
fc-cache -fv

# Install starship
wget https://starship.rs/install.sh
chmod +x install.sh
./install.sh
sudo rm -r ./install.sh

# Download configs
git clone https://github.com/s9chroma/backups.git
cd backups

# Install configs
mkdir -p .config
cp .vimrc ~/
cp .bashrc ~/
cp picom.conf ~/.config/
cp starship.toml ~/.config/
cp -r alacritty ~/.config/
cp -r bspwm ~/.config/
cp -r sxhkd ~/.config/
cp -r ulauncher ~/.config/
cp -r polybar ~/.config/

cd ..
sudo rm -r backups
