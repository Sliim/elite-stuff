#!/bin/bash

# Elite-stuff environment setup

read -p "Team Server Host: " ts_host
read -p "MSR RPC Port: " msf_port
read -p "MSR RPC User: " msf_user
read -p "MSR RPC Password: " msf_pass

export PATH=$PATH:$(realpath msf/bin)
export MSFRPC_HOST=$ts_host
export MSFRPC_PORT=$msf_port
export MSFRPC_USER=$msf_user
export MSFRPC_PASS=$msf_pass

msf-status
$SHELL
