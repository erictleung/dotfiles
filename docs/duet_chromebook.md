# Lenovo Duet 5 Chromebook

Here are my notes on using my Lenovo Duet 5 Chromebook and setting it up for
programming and development.

For Chromebooks before 2019, install Crouton as noted below. Otherwise,
Chromebooks after 2019 support Linux (Beta)
(Source: https://sites.google.com/a/chromium.org/dev/chromium-os/chrome-os-systems-supporting-linux).

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Contents**

- [Specs](#specs)
- [Setup](#setup)
- [Emacs](#emacs)
- [Firefox](#firefox)
- [freeCodeCamp and Web Development Setup](#freecodecamp-and-web-development-setup)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Specs

```sh
cat /etc/issue
# Debian GNU/Linux 11 \n \l
```

```sh
lscpu | head -n 10
# Architecture:                       aarch64
# CPU op-mode(s):                     32-bit, 64-bit
# Byte Order:                         Little Endian
# CPU(s):                             8
# On-line CPU(s) list:                0-7
# Thread(s) per core:                 1
# Core(s) per socket:                 8
# Socket(s):                          1
# Vendor ID:                          Qualcomm
# Model:                              14
```

**Note**: this model has a 64-bit ARM-based CPU, so be mindful when compiling
programs and that it matches.


## Setup

Core packages to have when on a Linux-like environments.

```sh
sudo apt-get update
sudo apt-get upgrade
sudo apt-get install build-essential stow git vim make curl htop tmux
```

## Emacs

Here are some instructions to
[install Emacs 26.1 on Ubuntu](http://ubuntuhandbook.org/index.php/2019/02/install-gnu-emacs-26-1-ubuntu-18-04-16-04-18-10/):

```shell
sudo apt-get install software-properties-common
sudo add-apt-repository ppa:kelleyk/emacs
sudo apt update
sudo apt install emacs26
echo "alias emacs=emacs26" >> .bash_local
```

## Firefox

**Note**: I ended up just going with the built-in Chrome browser for 
general performance and usability, despite my browser of choice is
Firefox.

With an ARM-powered Chromebook, run the following

```
sudo apt install firefox-esr
```

This will download a Linux Firefox browser, rather than the more limited Android
Firefox version.

Source: http://techradar.com/how-to/how-to-install-firefox-on-a-chromebook

## freeCodeCamp and Web Development Setup

Source: https://contribute.freecodecamp.org/#/how-to-setup-freecodecamp-locally

Node.js installation: https://github.com/nodesource/distributions

```sh
# Download and import Nodesource GPG key
sudo apt-get update
sudo apt-get install -y ca-certificates curl gnupg
sudo mkdir -p /etc/apt/keyrings
curl -fsSL https://deb.nodesource.com/gpgkey/nodesource-repo.gpg.key | sudo gpg --dearmor -o /etc/apt/keyrings/nodesource.gpg

# Create deb repository
NODE_MAJOR=20
echo "deb [signed-by=/etc/apt/keyrings/nodesource.gpg] https://deb.nodesource.com/node_$NODE_MAJOR.x nodistro main" | sudo tee /etc/apt/sources.list.d/nodesource.list

# Run update and install
sudo apt-get update
sudo apt-get install nodejs -y
```

pnpm installation: https://pnpm.io/installation

MongoDB installation: https://www.mongodb.com/try/download/community

Get Version 5.0.23, Platform Ubuntu 20.04 ARM 64, Package server.

```sh
sudo dpkg -i mongodb-org-server_5.0.23_arm64.deb
```

```sh
# Start server
sudo systemctl start mongod

# Verify
sudo systemctl status mongod

# Stop process
sudo systemctl stop mongod

# Restart
sudo systemctl restart mongod

# Mongo Shell
mongosh
```
