#!/bin/bash

cp positions_test_data.csv positions_data.csv
ansible-vault decrypt positions_data.csv
cp transaction_test_data.csv transaction_data.csv
ansible-vault decrypt transaction_data.csv
