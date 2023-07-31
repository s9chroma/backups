#!/bin/bash

# Update package lists
sudo apt update

# Install software
sudo apt install -y vim bspwm sxhkd polybar alacritty wget curl ulauncher picom git

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
yes | curl -sS https://starship.rs/install.sh | sh

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
