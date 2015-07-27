#!/bin/bash 

#if [[ -z $3 ]]; then
if [[ -z $1 ]]; then
	echo 'Deleted all experiment files'
	rm [[:digit:]]*
else
	cp $1'.csv' ./experiment_result/$1*/
	cp $1'_PSO_time.csv' ./experiment_result/$1*/
	cp $1'_NSGA_time.csv' ./experiment_result/$1*/
	#cp $ '_time.csv' ./experiment_result/$1*/$2*/
	#if [[ -e $1'.csv' ]];then
		#rm $1'.csv' $1'_PSO_time.csv'
		#rm $1'.csv' $1'_NSGA_time.csv'
	#fi
	rm [[:digit:]]*
fi
