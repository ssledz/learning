#!/bin/bash

ssh_key=$(vagrant ssh-config n1 | grep IdentityFile | sed 's/^ *//g' | cut -d' ' -f2)
ssh-add $ssh_key
vagrant ssh n1 -c 'cd /vagrant; ./install-ansible.sh; ansible-playbook -i hosts playbook.yml'
