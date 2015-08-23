#!/bin/sh

need sgegrid

#Absolute dir for grid tests
GRID_DIR="/home/st-james1/tanboxi/data/"

NUM=1
NAME="grid_job"
BASE_FILE="$1"
WAIT_FOR_COMPLETION=false
ARGUMENTS=""

#Handling the case where we have a file name
#as opposed to -h or some other argument
if [ "$#" -gt "1" ]; then
  shift
fi

#Parse Arguments
while  [[ $# -ge 1 ]]
do
  key="$1"
  shift

  case $key in 
    -n|--num_tests)
    NUM=$1
    shift
    ;;
    -N|--name|-nm)
    NAME=$1
    shift
    ;;
    -e|--email)
    EMAIL=$1
    shift
    ;;
    -ec|--email_code)
    EMAIL_CODE=$1
    shift
    ;;
    -w|--wait)
    WAIT_FOR_COMPLETION=true
    ;;
    -a|--args)
    ARGUMENTS=$1
    shift
    ;;
    -ja|--javaarg)
    JAVA_ARGUMENTS=$1
    shift
    ;;
    -h|--help)
      echo "maketest. Used for automating grid submission"
      echo "Format: maketest {File to run} {Arguments}"
      echo "Possible Arguments: "
      echo "-n|--num_tests Sets the number of tests to run (default 30)"
      echo "-nm|--name Specify the name of this job (default 'grid_job'"
      echo "-e|--email Specify what email to send any results to"
      echo "-a|--args Specify the arguments that will be passed to the program"
      echo "-ec|--email_code Specify *what* emails will be sent to above email:"
      echo "-w|--wait Send an email to the given email when all tests complete"
      echo "Possible email codes: b (When the job begins) 
		      e (When the job ends) 
		      a (If the job is aborted)
		      n (No notifications (default))"
      echo "Format Arguments: "
      echo "The '--args' argument accepts the following values which will be generated on the fly: "
      echo "    %n - The number of the trial"
      echo "-h|--help Show this help message"
      exit 0
  esac
done

if [ -n "${EMAIL_CODE}" ] && [ -z "${EMAIL}" ]; then
  echo "Warning: Email codes given without email to send them to. Ignoring."
fi

if [ "${WAIT_FOR_COMPLETION}" = true ] && [ -z "${EMAIL}" ]; then
  echo "Cannot wait for completion with no email to send notification to. Ignoring."
  WAIT_FOR_COMPLETION=false
fi

#Set email code to n (No Emails)
if [ -z "${EMAIL_CODE}" ]; then
  EMAIL_CODE="n"
fi

#Check whether given job file actually exists
if [ ! -f "${BASE_FILE}" ]; then
  echo "Failed to find script file: ${BASE_FILE}"
  exit 1
fi

dir="${GRID_DIR}/test_${NAME}"

if [ -e "$dir" ]; then
  echo "Job Directory already exists. Please choose a different name"
  exit 1
fi

file=$(basename "${BASE_FILE}")

#Do extention comparison without case sensitivity
shopt -s nocasematch

#Find how to run file
case "$file" in
  *.jar)
    RUN_CMD="java ${JAVA_ARGUMENTS} -jar "
  ;;
  *.py)
    RUN_CMD="python "
  ;;
  *.sh|*)
    RUN_CMD="./"
  ;;
  *.R|*)
    RUN_CMD="Rscript "
  ;;
esac

if [ -z "${RUN_CMD}" ]; then
  echo "Failed to resolve how to run script with extension $extension"
  exit 1
fi

#Copy everthing to new test directory
mkdir $dir
cp "$BASE_FILE" "$dir"
cd $dir

#used to copy aux scripts for analysis (averaging, removing logs etc)
#cp ~/Research/* ./

if [ -z "${EMAIL}" ]; then
    EMAIL=""
else
    EMAIL="#$ -M ${EMAIL}"
fi

job_list=""

mkdir scripts

#Submit the jobs!
for i in `seq ${NUM}`
do

#Defines the grid script which executes the submitted file
echo "#!/bin/sh
#$ -wd $dir
${EMAIL}
#$ -m ${EMAIL_CODE}

echo 'Starting Job'
${RUN_CMD}$file ${ARGUMENTS//'%n'/$i}
echo 'End Job'" > "scripts/${NAME}$i.sh"

chmod +x "scripts/${NAME}$i.sh"

 name="${NAME}$i"
 job_list="$name,$job_list"
 qsub -l arch=lx-amd64,mem_free=5000M,ecs_model=9020 -N $name "scripts/${NAME}$i.sh" > /dev/null
done

#Strip last comma from job_list
job_list="${job_list%?}"

if [ "${WAIT_FOR_COMPLETION}" = true ]; then
#create dummy wait script
echo "#!/bin/sh
#$ -wd $dir
#$ -M ${EMAIL}
#$ -m e" > wait_script
  chmod +x wait_script
  
  #submit wait script
  qsub -o /dev/null -e /dev/null -N "${NAME}" -hold_jid "${job_list}" wait_script > /dev/null
  echo "Submitted wait job"
fi

echo "Submitted ${NUM} grid jobs for $file in $dir"
