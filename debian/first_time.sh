#!/bin/bash


generate () 
{ 

echo -n "Enter you diary username": 
read USERNAME
echo -n "Enter you diary password": 
read -s PASSWORD

echo 'username =' '"'$USERNAME'"'  > $HOME/.hapidry
echo 'password =' '"'$PASSWORD'"' >> $HOME/.hapidry

fin
}

fin ()
{
    exit
}



echo "This is first your first hapidry run. This script can generate hapidry config for you"

while true; do
    read -p "Do you to generate config? [Y/n] $(echo -e '\n')" yn
    case $yn in
        [Yy]* ) generate; break;;
        [Nn]* ) fin; break;;
        * ) echo "Please answer yes or no.";;
    esac
done