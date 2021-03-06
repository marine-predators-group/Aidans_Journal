#!/bin/bash
## Job Name
#SBATCH --job-name=blastn_metagenomics
## Allocation Definition
#SBATCH --account=coenv
#SBATCH --partition=coenv
## Resources
## Nodes
#SBATCH --nodes=1
## Walltime (days-hours:minutes:seconds format)
#SBATCH --time=25-00:00:00
## Memory per node
#SBATCH --mem=120G
##turn on e-mail notification
#SBATCH --mail-type=ALL
#SBATCH --mail-user=samwhite@uw.edu
## Specify the working directory for this job
#SBATCH --workdir=/gscratch/scrubbed/samwhite/outputs/20190611_metagenomics_pgen_fastq_blastn

# Exit script if any command fails
set -e

# Load Python Mox module for Python module availability

module load intel-python3_2017

# Document programs in PATH (primarily for program version ID)

date >> system_path.log
echo "" >> system_path.log
echo "System PATH for $SLURM_JOB_ID" >> system_path.log
echo "" >> system_path.log
printf "%0.s-" {1..10} >> system_path.log
echo "${PATH}" | tr : \\n >> system_path.log


threads=28

# Paths to programs
blast_dir="/gscratch/srlab/programs/ncbi-blast-2.8.1+/bin"
blastn="${blast_dir}/blastn"
seqtk="/gscratch/srlab/programs/seqtk-1.3/seqtk"

# Paths to blastdbs
blastdb_dir="/gscratch/srlab/blastdbs/ncbi-nr-nt-v5"
blast_db="${blastdb_dir}/nt"
samtools="/gscratch/srlab/programs/samtools-1.9/samtools"

# Input files
fastq_dir="/gscratch/srlab/sam/data/metagenomics/P_generosa/"


## Inititalize arrays
fastq_array_R1=()
fastq_array_R2=()
names_array=()

# Create array of fastq R1 files
for fastq in ${fastq_dir}/*R1*.gz
do
  fastq_array_R1+=(${fastq})
done

# Create array of fastq R2 files
for fastq in ${fastq_dir}/*R2*.gz
do
  fastq_array_R2+=(${fastq})
done

# Create array of sample names
## Uses parameter substitution to strip leading path from filename
## Uses awk to parse out sample name from filename
for R1_fastq in ${fastq_dir}/*R1*.gz
do
  names_array+=($(echo ${R1_fastq#${fastq_dir}} | awk -F"_" '{print $3 $4}'))
done

# Create list of fastq files used in analysis
## Uses parameter substitution to strip leading path from filename
for fastq in ${fastq_dir}*.gz
do
  echo "${fastq#${fastq_dir}}" >> fastq.list.txt
done

# Concatenate paired-end reads into singular FastA files for each sample.
# Uses seqtk to convert FastQ to FastA.
for index in "${!fastq_array_R1[@]}"
do
  sample_name=$(echo "${names_array[index]}")
  if [ "${sample_name}" == "MG1" ] \
  || [ "${sample_name}" == "MG2" ] \
  || [ "${sample_name}" == "MG5" ]
  then
    sample_name="${sample_name}"_pH82
  else
    sample_name="${sample_name}"_pH71
  fi
  "${seqtk}" seq -a "${fastq_array_R1[index]}" "${fastq_array_R2[index]}" >> "${sample_name}".fasta
done

# Export BLAST database directory
export BLASTDB=${blastdb_dir}

# Loop through FastAs
# Create list of those FastAs for reference
# Parse out sample names
# Run BLASTn on each FastA
for fasta in *.fasta
do
  echo "${fasta}" >> input.fasta.list.txt
  "${samtools}" faidx "${fasta}"
  no_ext=${fasta%%.*}
  sample_name=$(echo ${no_ext##*/})

  # Run BLASTn on each FastA
  ${blastn} \
  -query "${fasta}" \
  -db ${blast_db} \
  -max_target_seqs 1 \
  -max_hsps 1 \
  -outfmt "6 std staxid ssciname" \
  -evalue 1e-10 \
  -num_threads ${threads} \
  > "${sample_name}".blastn.outfmt6
done
