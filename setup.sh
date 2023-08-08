#!/bin/bash

# Update package lists
yes | sudo apt update

# Install software
yes | sudo apt install vim vim-gtk3 bspwm sxhkd polybar alacritty wget curl git nodejs npm gnome-tweaks make gcc xdotool gawk ripgrep fzf feh blueman fonts-noto-color-emoji silversearcher-ag

# Install picom
yes | sudo apt install libxext-dev libxcb1-dev libxcb-damage0-dev libxcb-xfixes0-dev libxcb-shape0-dev libxcb-render-util0-dev libxcb-render0-dev libxcb-randr0-dev libxcb-composite0-dev libxcb-image0-dev libxcb-present-dev libxcb-xinerama0-dev libxcb-glx0-dev libpixman-1-dev libdbus-1-dev libconfig-dev libgl1-mesa-dev  libpcre2-dev  libevdev-dev uthash-dev libev-dev libx11-xcb-dev
git clone https://github.com/jonaburg/picom
cd picom
meson --buildtype=release . build
ninja -C build
sudo ninja -C build install
cd ..
sudo rm -r picom

# Install ULauncher
wget https://github.com/Ulauncher/Ulauncher/releases/download/5.15.3/ulauncher_5.15.3_all.deb
yes | sudo apt install -y ./ulauncher_5.15.3_all.deb
sudo rm -r ./ulauncher_5.15.3_all.deb

# Vim Plugin Setup
curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

# Install tdrop
git clone https://github.com/noctuid/tdrop
cd tdrop
yes | sudo make install
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
yes | ./install.sh
sudo rm -r ./install.sh

# Download configs
git clone https://github.com/s9chroma/backups.git
cd backups

# Copy wallpaper
mkdir -p ~/Pictures
cp wallpaper.jpg ~/Pictures/

# Install configs
mkdir -p .config
cp .vimrc ~/
cp .bashrc ~/
cp .gtkrc-2.0 ~/
cp picom.conf ~/.config/
cp starship.toml ~/.config/
cp -r gtk-3.0 ~/.config/
cp -r alacritty ~/.config/
cp -r bspwm ~/.config/
cp -r sxhkd ~/.config/
cp -r ulauncher ~/.config/
cp -r polybar ~/.config/
cp -r ulauncher ~/.config/

cd ..
sudo rm -r backups

# Install chrome
wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
yes | sudo apt install -y ./google-chrome-stable_current_amd64.deb
sudo rm -r ./google-chrome-stable_current_amd64.deb
