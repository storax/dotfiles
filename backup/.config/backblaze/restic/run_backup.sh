#!/bin/bash
# Runs Restic backup on a schedule via cron, emails with status

EMAIL="zuber.david@gmx.de"
LOG="/var/log/restic.log"
RDIR="/home/david/.config/backblaze"

### keep last # of days of snapshots
KEEPDAYS=10

log() { 
    echo -e "$(date "+%Y-%m-%d %H:%M:%S"): ${1}" | tee -a $LOG
}

notify() {
    echo -e "Subject: Errors running Restic Backup on host on: $(hostname)\n\nS${1}" | su - david -c "sendmail -v ${EMAIL}"
}

cd $RDIR

echo -e "\n" | tee -a $LOG

if [ ! -f pw ]
then
	log "${RDIR}/pw file not present, exiting..\n\n"
	exit 1
fi

source ${RDIR}/cred

log "starting backup.."

msg=$(restic backup --exclude-caches --files-from=restic/include --exclude-file=restic/exclude >> $LOG 2>&1)

if [ $? -eq 1 ]
then
    notify "[restic backup]\n${msg}"
    log "${msg}\n-----------------------------------------"
    exit 1
fi

msg=$(restic check >> $LOG 2>&1)

# Check for Errors
if [ $? -eq 1 ]
then
    notify "[restic check]\n${msg}"
    log "${msg}\n--------------------------------------"
    exit 1
fi


log "removing old snapshots.."

msg=$(restic forget --keep-daily ${KEEPDAYS} --prune)

if [ $? -eq 1 ]
then
    notify "[restic forget]\n${msg}"
    log "${msg}"
    exit 1
fi


log "end of run\n-----------------------------------------\n\n"

# notify OK
echo -e "Subject: Restic Backup OK on: $(hostname)\n\nSnapshot complete, snapshots older than $KEEPDAYS days deleted." | su - david -c "sendmail -v ${EMAIL}"

