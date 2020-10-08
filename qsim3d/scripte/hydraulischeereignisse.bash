#! /bin/bash

# echo $0
                                          
_user="$(id -u -n)"
# echo "User name : $_user"

ereig="/home/$_user/qmodelle/hydraulische_ereignisse.txt"
# echo "3D-Ereignisse: "$ereig

# cat /mreferate/schaeferma/qmodelle/hydraulische_ereignisse.txt
# cat /home/Schoenung/hydraulische_ereignisse.txt
# cat /home/Wyrwa/qmodelle/hydraulische_ereignisse.txt
# cat /home/"$_user"/qmodelle/hydraulische_ereignisse.txt

cat $ereig

exit 0
