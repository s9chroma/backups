#!/bin/bash

# Update package lists
sudo apt update

# Install software
sudo apt install -y vim vim-gtk3 bspwm sxhkd polybar alacritty wget curl picom git nodejs npm

# Install ULauncher
wget https://github.com/Ulauncher/Ulauncher/releases/download/5.15.3/ulauncher_5.15.3_all.deb
sudo apt install ./ulauncher_5.15.3_all.deb
rm -r ./ulauncher_5.15.3_all.deb

# Vim Plugin Setup
curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

# Install vimrc dependencies
sudo apt install -y ripgrep fzf

# Download and install Iosevka Nerd Font
wget https://github.com/ryanoasis/nerd-fonts/releases/download/v3.0.2/Iosevka.zip
mkdir -p /usr/local/share/fonts
sudo unzip Iosevka.zip -d /usr/local/share/fonts/
rm -r Iosevka.zip
fc-cache -fv

# Install starship
curl -sS https://starship.rs/install.sh
chmod +x install.sh
./install.sh

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
rm -r backups
