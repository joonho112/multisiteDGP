cd "/Users/joonholee/Documents/00_IES Multisite Trial Project/multisiteDGP-R-package"
export MULTISITEDGP_VALIDATION_MODE=full MULTISITEDGP_VALIDATION_OVERWRITE=true MULTISITEDGP_VALIDATION_RESUME=false
mkdir -p /tmp/vfull
for v in v01 v02 v03 v04 v05 v06 v07 v08 v09 v10 v11 v12; do
  s=$(date +%s)
  Rscript "tools/validation/jobs/run-${v}-validation.R" > "/tmp/vfull/${v}.txt" 2>&1
  rc=$?
  st=$(grep -oE "status: (pass|fail|skip)" "/tmp/vfull/${v}.txt" | tail -1)
  printf "%-5s %-14s rc=%d  %ds\n" "$v" "${st:-무상태}" "$rc" "$(( $(date +%s) - s ))" >> /tmp/vfull/RESULTS.txt
done
echo "DONE" >> /tmp/vfull/RESULTS.txt
